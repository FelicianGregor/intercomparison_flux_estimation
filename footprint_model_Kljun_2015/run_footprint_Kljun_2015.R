# run footprint parametrization by KlJun 2015

# load the footprint climatology function from Kljun 2015
source("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/footprint_model_Kljun_2015/calc_footprint_fFP_climatology.R")

# prep data first
##### prepare data for footprint parametrization model by Kljun 2015 ####
library(dplyr)
library(lubridate)
library(tidyverse)
library(fields)

# sonic data
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/sonic_profile_data.RData") # load profile data
# data on Boundary layer height, derived from ERA5 data
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/footprint_model_Kljun_2015/BL_height.Rdata")
BL_h$datetime <- as.POSIXct(BL_h$time, tz = "UTC")

# define measurement height:
zm = 70

# use only one ensemble member:
data_30m = sonic_profile_data%>%
  filter(folder == paste0("2021_", zm, "m_double_block"))%>%
  # join the BL height data
  left_join(y = BL_h, by = "datetime")
  

df <- data_30m %>%
  mutate(
    #make datetime column
    datetime = ymd_hm(paste(`date_[yyyy-mm-dd]`, `time_[HH:MM]`), tz = "UTC"),
    yyyy = year(datetime),
    mm   = month(datetime),
    day  = day(datetime),
    HH   = hour(datetime),
    MM   = minute(datetime),
    zm = zm, #measurement height
    u_mean = `wind_speed_[m+1s-1]`,
    L = `L_[m]`,
    sigma_v = sqrt(`v_var_[m+2s-2]`), # take the square root to get sd from var
    u_star = `u*_[m+1s-1]`,
    wind_dir = `wind_dir_[deg_from_north]`,
    h = BL_height_m_a_g_l, 
    
    #calculate displacement height d and roughness length z0
    d = 0.666 * 19, # mean canopy height is 19m
    z0 = 1.9  # approximate as 0.1*canopy height
  ) %>%
  
  # apply filter
  filter(
    u_star > 0.1,
    zm / L >= -15.5, 
    h>zm # if measurement height is larger than the ABL height 
  ) %>%
  
  # select required columns
  select(yyyy, mm, day, HH, MM, zm, d, z0, u_mean, L, sigma_v, u_star, wind_dir, h, datetime)%>%
  mutate(across(where(is.numeric), ~ replace_na(., -999)))


# select a shorter time frame during summer for test purpose
df_short = df%>%
  filter(datetime >= ymd("2021-07-20"),
         datetime <  ymd("2021-08-01"))%>%
  select(-datetime)

### test with df_short ####
FFP = calc_footprint_FFP_climatology(zm=zm, 
                               z0=1.9, 
                               umean= df_short$u_mean, 
                               h=df_short$h, 
                               ol=df_short$L,
                               sigmav=df_short$sigma_v, 
                               ustar=df_short$u_star, 
                               wind_dir=df_short$wind_dir,
                               domain=c(-10000,10000,-10000,10000), 
                               nx=1000, 
                               r=c(25, 50, 75), 
                               smooth_data=0, # 1 often gives an error 
                               crop = 1)

# plot with percentile lines
image.plot(FFP$x_2d[1,], FFP$y_2d[,1], FFP$fclim_2d)
for (i in 1:3) lines(FFP$xr[[i]], FFP$yr[[i]], type="l", col="red")

# compare with wind rose, as recommended in the README of KLjun 2015 R tool 
library(openair)
# prepare data
wind_data = data.frame('wd' = df_short$wind_dir, 
                       'ws' = df_short$u_mean)

# do for full data series or per month
polarFreq(wind_data)
# seems like no transpose needed here...


#### get polygons ####
library(sf)
library(purrr)
library(dplyr)

# HTM tower
lon0 <- 13.41897
lat0 <- 56.09763

ref_utm <- st_sfc(st_point(c(lon0, lat0)), crs = 4326)%>%
  st_transform(32633)

origin <- st_coordinates(ref_utm)
x0 <- origin[1]
y0 <- origin[2]

make_polygons <- function(x, y, x0, y0) {
  
  idx <- cumsum(is.na(x) | is.na(y))
  
  polys <- list()
  
  for (i in unique(idx)) {
    
    sel <- idx == i
    
    xx <- x[sel]
    yy <- y[sel]
    
    # remove NA inside segment
    ok <- !(is.na(xx) | is.na(yy))
    xx <- xx[ok]
    yy <- yy[ok]
    
    # need at least 4 points for polygon (incl closure)
    if (length(xx) < 3) next
    
    coords <- cbind(xx + x0, yy + y0)
    
    # extra safety: remove any remaining NA rows
    coords <- coords[complete.cases(coords), , drop = FALSE]
    
    if (nrow(coords) < 3) next
    
    # close polygon safely (no NA comparison)
    if (!(all(is.na(coords[1,])) || all(is.na(coords[nrow(coords),])))) {
      if (!identical(coords[1, ], coords[nrow(coords), ])) {
        coords <- rbind(coords, coords[1, ])
      }
    }
    
    polys[[length(polys) + 1]] <- st_polygon(list(coords))
  }
  
  st_sfc(polys, crs = 32633)
}

ffp_sf <- map_dfr(seq_along(FFP$xr), function(i) {
  
  geom <- make_polygons(
    FFP$xr[[i]],
    FFP$yr[[i]],
    x0,
    y0
  )
  
  st_sf(
    percentile = i,
    geometry = geom
  )
})

ffp_wgs84 <- st_transform(ffp_sf, 3857)

#### get basemap $###

#library(basemaps)

#new_aoi = draw_ext()
#st_write(new_aoi, paste0(map_dir, "AOI.geojson"))

# set defaults for the basemap
#set_defaults(map_service = "esri", map_type = "world_imagery")

# load and save basemap
#basemap_geotif(aoi, zoom = 11, map_dir = map_dir)

####### final map footprint ####
library(stars)
library(sf)
library(ggplot2)
library(ggspatial)

map_dir = "C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/footprint_model_Kljun_2015/basemap_esri_world_imagery/"
AOI <- read_sf(paste0(map_dir, "AOI.geojson"))
base <- read_stars(paste0(map_dir, "basemap_20260515194847.174023.tif"))

box <- st_polygon(list(matrix(
  c(
    13.26, 56.17,   # lower left
    13.37, 56.17,   # lower right
    13.37, 56.1985,   # upper right
    13.26,  56.1985,   # upper left
    13.26, 56.17    # close polygon
  ),
  ncol = 2,
  byrow = TRUE
)))

# set crs
bbox_sf <- st_sf(geometry = st_sfc(box),
                 crs = 4326
)

# GeoJSON is WGS84
st_crs(AOI) <- 4326
# transform to Web Mercator ((match basemap))same as basemap)
AOI <- st_transform(AOI, 3857)

# transform footprint and tower
footprint_sf = st_transform(ffp_wgs84, 3857)


# convert to rgb for plotting
base_rgb <- st_rgb(base)

# plot
ggplot() +
  geom_stars(data = base_rgb) +
  geom_sf(data = st_transform(bbox_sf, 3857),
          fill = "white",
          color = "lightgrey",
          linewidth = 0.3, alpha = 0.2)+
  geom_sf(data = footprint_sf, aes(fill = as.factor(percentile)),
          alpha = 0.4, color = "black", linewidth = 0.2) +
  geom_sf(
    data = tower <- st_sfc(
      st_point(c(13.41897, 56.09763)),
      crs = 4326
    ) %>%
      st_transform(3857),
    color = "red", size = 3) +
  coord_sf(crs = 3857, expand = FALSE) +
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(0.2, "cm"),
    pad_y = unit(0.8, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  annotation_scale(
    location = "tl",
    height = unit(0.3, "cm")
  )+
  scale_fill_discrete(
    name = "",
    labels = c("25%", "50%", "75%")
  )+
  labs(y = "", x = "")+
  theme(
    axis.text.y = element_text(
      angle = 90,
      hjust = 0.5,
      vjust = 0.5
    )
  
  )


