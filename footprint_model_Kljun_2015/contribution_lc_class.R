##### convert raster footprint output to stars in crs 3035 ####
# to calculate the contribition of each land cover class, I have to convert the local coordinate raster given as 
# output from the Kljun footprint to a raster (stars) in crs 3035 (in which my land cover classification data is) 

library(tidyverse)
library(stars)
library(sf)

# load calculated footprint
load("./Footprint_model_Kljun_2015/FFP_results_all.RData")

# create vectors of heights and stab to loop over
heights <- c("30", "70", "148")
stabilities <- c("stable", "neutral", "unstable")

# tower location, transform from WGS84 grs 4326 to crs 3035, UTM in m
tower_utm <- st_sfc(
  st_point(c(13.41897, 56.09763)),
  crs = 4326) %>%
  st_transform(3035)

origin <- st_coordinates(tower_utm)
x0 <- origin[1]
y0 <- origin[2]

# list to store
FFP_3035_all <- list()

# start loop
for (h in heights) {
  FFP_3035_all[[h]] <- list()
  
  for (st in stabilities) {
    
    FFP <- FFP_results_all[[h]][[st]]
    
    # shift x and y values of the footprint grid, so that local coordinate system (tower at 0, 0) gets 3035 (both in m!)
    x_utm <- FFP$x_2d + x0
    y_utm <- FFP$y_2d + y0
    
    #create stars object from the raster 
    FFP_3035 <- st_as_stars(FFP$fclim_2d)
    # set the x and y dimensions, as done in Kljun 2015 README for R in the plot
    st_dimensions(FFP_3035) <- st_dimensions(
      x = x_utm[1, ],
      y = y_utm[, 1])
    st_crs(FFP_3035) <- 3035 # assign crs
    
    # store in list
    FFP_3035_all[[h]][[st]] <- FFP_3035
  }
}

# remove original local grids as obtained from KLjun run_footprint 
rm(FFP_results_all)

# example plot
plot(FFP_3035_all[["30"]][["stable"]])

# save to RDate
save(FFP_3035_all, file = "footprint_model_Kljun_2015/FFP_CRS_3035_stars_all.RData")

####### contribution in a loop ###########
library(sf)
library(stars)
library(terra)
library(exactextractr)
library(dplyr)

# Load once
load("footprint_model_Kljun_2015/FFP_CRS_3035_stars_all.RData")

land_cover <- read_stars(
  "C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/footprint_model_Kljun_2015/basemap_CLCplus_Backbone_2023/CLMS_CLCPLUS_RAS_S2023_R10m_E45N36_03035_V01_R00.tif",
  proxy = TRUE
)

heights <- c("30", "70", "148")
stabilities <- c("stable", "neutral", "unstable")

results_list <- list()
counter <- 1

for (height in heights) {
  
  for (stability in stabilities) {
    
    cat("Processing: ", height, " m / ", stability)
    
    # read in footprint areas (polygons) written to geojson
    path <- paste0(
      "C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/",
      "footprint_model_Kljun_2015/footprint_shp_files/",
      "footprint_", height, "_", stability, ".geojson"
    )
    
    footprint <- st_read(path, quiet = TRUE)
    
    # select only the largest polygon, which is the 75% contribution
    footprint_max <- footprint %>%
      filter(Contribution == "75%") %>%
      mutate(area = st_area(.)) %>%
      slice_max(area, n = 1) %>%
      select(-area) %>%
      st_transform(3035)
    
    # from the list with all footprints
    weight_cropped <- st_crop(
      FFP_3035_all[[height]][[stability]],
      footprint_max)
    
    # normalise, so that sum is 1 (so it becomes persentage)
    weight_cropped[["A1"]] <-
      weight_cropped[["A1"]] /
      sum(weight_cropped[["A1"]], na.rm = TRUE)
    
    # convert to terra raster object for the exactextract function
    weight_cropped_terra <- terra::rast(weight_cropped)
    
    #cop land cover by footprint 75% area
    land_cover_cropped <- st_crop(
      land_cover,
      footprint_max)
    # vectorise, so make polygons from raster, and merge
    lc_vectorized <- st_as_sf(
      land_cover_cropped,
      as_points = FALSE,
      merge = TRUE)
    
    # quickly rename 
    names(lc_vectorized)[1] <- "class"
    
    ################################################
    #use exactextractr to calculate the contribution
    ################################################
    result <- exact_extract(
      weight_cropped_terra,
      lc_vectorized,
      function(values, coverage_fraction) {
        sum(values * coverage_fraction, na.rm = TRUE)
      }
    )
    
    # output is a vector, which has same order as the lc_vectorized (sf object with geomteries in there, so one entry in the result vector gives the contribution of the ith object)
    lc_vectorized$fp_contribution <- result
    
    #sum, grouped by land cover class
    df_sum <- lc_vectorized %>%
      st_drop_geometry() %>%
      group_by(class) %>%
      summarise(
        fp_contribution = sum(fp_contribution, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        height = height,
        stability = stability
      )
    
    # write in list
    results_list[[counter]] <- df_sum
    counter <- counter + 1
  }
}

# Combined table
df_all <- bind_rows(results_list)

df_all

# check that all footprints total contribution sum to 1
df_all %>%
  group_by(height, stability) %>%
  summarise(total = sum(fp_contribution))

# plot as dodge 
library(ggplot2)

df_plot <- df_all %>%
  mutate(
    footprint = factor(
      paste(height, stability, sep = "\n"),
      levels = c(
        "30\nunstable", "30\nneutral", "30\nstable",
        "70\nunstable", "70\nneutral", "70\nstable",
        "148\nunstable", "148\nneutral", "148\nstable"
      )
    )
  )%>%
  mutate(fp_contribution = fp_contribution * 100)

ggplot(
  df_plot,
  aes(
    x = footprint,
    y = fp_contribution,
    fill = factor(class)
  )
) +
  geom_col() +
  labs(
    x = "",
    y = "Weighted contribution [%]",
    fill = ""
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
