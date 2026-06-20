
# load packages
library(sf)
library(purrr)
library(dplyr)
library(ggplot2)
library(basemaps)
library(stars)
library(ggspatial)
library(grid)


# load calculated footprint
load("./Footprint_model_Kljun_2015/FFP_results_all.RData")


#assign one footprint first, and do everything with just one
height = "148"
stability = "unstable"
FFP = FFP_results_all[[height]][[stability]]


#### get polygons of FFP ####

# HTM tower
lon0 <- 13.41897
lat0 <- 56.09763

ref_utm <- st_sfc(st_point(c(lon0, lat0)), crs = 4326)%>%
  st_transform(3035)

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
  
  st_sfc(polys, crs = 3035)
}

ffp_sf <- map_dfr(seq_along(FFP$xr), function(i) {
  
  geom <- make_polygons(
    x = FFP$xr[[i]],
    y = FFP$yr[[i]],
    x0 = x0,
    y0 = y0
  )
  
  st_sf(
    percentile = i,
    geometry = geom
  )
})

ffp_wgs84 <- st_transform(ffp_sf, 4326)
footprint <- st_transform(ffp_sf, 3035)

#### plot ####
land_cover= read_stars("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/footprint_model_Kljun_2015/basemap_CLCplus_Backbone_2023/CLMS_CLCPLUS_RAS_S2023_R10m_E45N36_03035_V01_R00.tif", proxy = F)
st_crs(land_cover)

# function for getting squared bounding box
squared_bbox = function(polygon, padding = 50){
  # padding in m
  
  # load package needed
  library(sf)
  
  # bbox of footprint
  bb <- st_bbox(polygon)
  
  dx <- bb["xmax"] - bb["xmin"]
  dy <- bb["ymax"] - bb["ymin"]
  
  cx <- (bb["xmin"] + bb["xmax"]) / 2
  cy <- (bb["ymin"] + bb["ymax"]) / 2
  
  half_side <- (max(dx, dy) / 2) + padding # padding in meter
  
  # create bbox
  square_bb <- st_bbox(c(
    xmin = as.numeric(cx - half_side),
    ymin = as.numeric(cy - half_side),
    xmax = as.numeric(cx + half_side),
    ymax = as.numeric(cy + half_side)
  ), crs = st_crs(polygon))
  
  # return
  return(square_bb)
}

# crop to extent of footprint, 50m padding
land_cover  = st_crop(land_cover, squared_bbox(footprint))

# recode and rename the classes
# recode and rename land cover classes to rather land use classes
land_cover[[1]] <- as.character(land_cover[[1]])

land_cover[[1]] <- recode(
  land_cover[[1]],
  "Low-growing woody plants" = "Other",
  "Non and sparsely vegetated" = "Other",
  
  "Permanent herbaceous" = "Herbaceous",
  "Periodically herbaceous" = "Herbaceous",
  
  "Woody needle leaved trees" = "Coniferous forest",
  "Woody broadleaved deciduous trees" = "Deciduous forest"
)

# downsample for faster plotting
if(height == "148" & stability == "stable"){
  land_cover = st_downsample(land_cover, n = 3)
}

# make classes a factor, so that they dont disappear
land_cover[[1]] <- factor(land_cover[[1]])

# define the color values
landcover_colors <- c(
  "No data" = "#000000",
  "Snow and ice" = "#B3D9FF",
  "Water" = "blue",
  #"Non and sparsely vegetated" = "#7A7A7A",
  "Lichens and mosses" = "#F0E442",
  # Herbaceous / agriculture
  #"Periodically herbaceous" = "#FEE08B",
  "Herbaceous" = "wheat", 
  #"Grassland" = "wheat3",
  #"Permanent herbaceous" = "wheat3",
  # Shrubs / low woody
  #"Low-growing woody plants" = "#A6D96A",
  "Other" = "#7A7A7A",
  # forests
  "Deciduous forest" = "#3FA34D",  
  "Coniferous forest" = "#1F5F5B",
  "Sealed" = "black"
)


# make categories
footprint <- footprint %>%
  mutate(percentage = case_when(percentile == 1 ~ "25%",
                                percentile == 2 ~ "50%",
                                percentile == 3 ~ "75%"))

plot_test <- ggplot() +
  
  # land cover raster
  geom_stars(data = land_cover, alpha = 0.7) +
  scale_fill_manual(
    values = landcover_colors,
    name = "Land cover class"
  ) +
  
  # footprint polygons
  geom_sf(
    data = footprint,
    aes(color = percentage),
    fill = NA,
    linewidth = 1
  ) +
  
  scale_color_manual(
    name = "Contribution",
    values = c(
      "25%" = "#F0E442",
      "50%" = "#D55E00",
      "75%" = "#6A3D9A",
      "Tower ICOS Se-Htm" = "black"
    )
  ) +
  
  # tower location
  geom_sf(
    data = ref_utm,
    aes(color = "Tower ICOS Se-Htm"),
    size = 3
  ) +
  
  coord_sf(expand = FALSE) +
  
  labs(
    x = "",
    y = ""
  ) +
  
  #annotation_north_arrow(
  #  location = "tr",
  #  which_north = "true",
  #  style = north_arrow_fancy_orienteering(),
  #  height = unit(0.5, "cm"),
  #  width = unit(0.5, "cm")
  #) +
  
  # rectangle for the scale
  annotation_custom(
    grob = rectGrob(
      x = unit(0.86, "npc"),
      y = unit(0.95, "npc"),
      width = unit(0.30, "npc"),
      height = unit(0.08, "npc"),
      just = c("center", "center"),
      gp = gpar(
        fill = scales::alpha("white", 0.4),
        col = NA
      )
    )
  )+
  annotation_scale(
    location = "tr",
    width_hint = 0.15,
    text_cex = 1, 
    bar_cols = c("black", "white")
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 3)
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 3)
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(
      angle = 90,
      hjust = 0.5,
      vjust = 0.5
    ),
    legend.position = "right"
  )



# save the legend for later
library(cowplot)
legend_only = get_legend(plot_test)




##### do this for all heights #####
heights <- c("30", "70", "148")
stabilities <- c("unstable", "neutral", "stable")
land_cover= read_stars("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/footprint_model_Kljun_2015/basemap_CLCplus_Backbone_2023/CLMS_CLCPLUS_RAS_S2023_R10m_E45N36_03035_V01_R00.tif", proxy = F)

# recode and rename land cover classes to rather land use classes
land_cover[[1]] <- as.character(land_cover[[1]])

land_cover[[1]] <- recode(
  land_cover[[1]],
  "Low-growing woody plants" = "Other",
  "Non and sparsely vegetated" = "Other",
  
  "Permanent herbaceous" = "Herbaceous",
  "Periodically herbaceous" = "Herbaceous",
  
  "Woody needle leaved trees" = "Coniferous forest",
  "Woody broadleaved deciduous trees" = "Deciduous forest"
)

land_cover[[1]] <- as.character(land_cover[[1]])

# location of scale and the rectangle
scale_positions <- data.frame(
  height = rep(c("30", "70", "148"), each = 3),
  stability = rep(c("unstable", "neutral", "stable"), times = 3),
  location = c(
    "tl", "tl", "tl",   # 30
    "br", "br", "br",   # 70
    "tl", "tl", "tl"    # 148
  ),
  stringsAsFactors = FALSE
)

#define a function to get the location of the scale inside the ggplot 
get_scale_location <- function(h, s) {
  scale_positions |>
    dplyr::filter(height == h, stability == s) |>
    dplyr::pull(location)
}

# also a table with the position of the rectangles:
rect_positions <- data.frame(
  height = rep(c("30", "70", "148"), each = 3),
  stability = rep(c("unstable", "neutral", "stable"), times = 3),
  
  location = c(
    "tl", "tl", "tl",
    "br", "br", "br",
    "tl", "tl", "tl"
  ),
  
  x = c(
    0, 0, 0,   # tl → anchor at left
    1, 1, 1,   # br → anchor at right
    0, 0, 0
  ),
  
  y = c(
    1, 1, 1,   # tl → top
    0, 0, 0,   # br → bottom
    1, 1, 1
  ),
  
  just_h = c(
    "left", "left", "left",
    "right", "right", "right",
    "left", "left", "left"
  ),
  
  just_v = c(
    "top", "top", "top",
    "bottom", "bottom", "bottom",
    "top", "top", "top"
  ),
  
  width = c(
    0.46, 0.51, 0.53,
    0.51, 0.51, 0.39,
    0.38, 0.44, 0.45
  ),
  
  height_rect = 0.14,
  stringsAsFactors = FALSE
)

#helper fun to retrieve the data from table
get_rect_params <- function(h, s) {
  rect_positions |>
    dplyr::filter(height == h, stability == s)
}


plots <- list()

for (h in heights) {
  plots[[h]] <- list()
  
  for (s in stabilities) {
    
    message("Creating plot: height = ", h, " | stability = ", s)
    
    FFP <- FFP_results_all[[h]][[s]]
    
    footprint <- st_transform(
      map_dfr(seq_along(FFP$xr), function(i) {
        
        geom <- make_polygons(
          x = FFP$xr[[i]],
          y = FFP$yr[[i]],
          x0 = x0,
          y0 = y0
        )
        
        st_sf(
          percentile = i,
          geometry = geom
        )
      }),
      3035
    ) %>%
      mutate(percentage = case_when(
        percentile == 1 ~ "25%",
        percentile == 2 ~ "50%",
        percentile == 3 ~ "75%"
      ))
    
    land_cover_crop <- st_crop(land_cover, squared_bbox(footprint))
    
    # IMPORTANT: do not overwrite global land_cover
    land_cover_local <- land_cover_crop
    
    # optional downsampling
    if (h == "148" & s == "stable") {
      land_cover_local <- st_downsample(land_cover_local, n = 3)
    }
    
    rp <- get_rect_params(h, s)
    print(rp)
    
    p <- ggplot() +
      
      # -----------------------
    # BACKGROUND (land cover)
    # -----------------------
    geom_stars(data = land_cover_local, alpha = 0.7) +
      scale_fill_manual(
        values = landcover_colors,
        name = "Land cover class"
      ) +
      
      # -----------------------
    # FOOTPRINT LINES
    # -----------------------
    geom_sf(
      data = footprint,
      aes(color = percentage),
      fill = NA,
      linewidth = 1
    ) +
      
      scale_color_manual(
        values = c(
          "25%" = "#F0E442",
          "50%" = "#D55E00",
          "75%" = "#6A3D9A",
          "Tower ICOS Se-Htm" = "black"
        ),
        name = "Contribution"
      ) +
      
      # -----------------------
    # TOWER
    # -----------------------
    geom_sf(
      data = ref_utm,
      aes(color = "Tower ICOS Se-Htm"),
      size = 3
    )  +
      
      # -----------------------
    # WHITE BACKGROUND BOX (UI AREA)
    # -----------------------
    annotation_custom(
      grob = rectGrob(
        x = unit(rp$x, "npc"),
        y = unit(rp$y, "npc"),
        width = unit(rp$width, "npc"),
        height = unit(rp$height_rect, "npc"),
        just = c(rp$just_h, rp$just_v),
        gp = gpar(
          fill = scales::alpha("white", 0.5),
          col = NA
        )
      )
    ) +
      
      # -----------------------
    # MAP ELEMENTS
    # -----------------------
    annotation_scale(
      location = get_scale_location(h, s),
      width_hint = 0.2,
      text_cex = 1.0
    ) +
      
      coord_sf(expand = FALSE) +
      
      scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
      
      labs(
        title = paste0(h, " m, ", s)
      ) +
      
      theme_bw() +
      theme(
        axis.text.y = element_text(
          angle = 90,
          hjust = 0.5,
          vjust = 0.5
        ),
        axis.text = element_text(size = 8),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, 
           margin = margin(t = 2, b = 1)), 
        plot.margin = margin(0, 0, 0, 0.3), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
    
    plots[[h]][[s]] <- p
  }
}

# free unused memory space
gc()

# arrange the plots, also legend
library(patchwork)

final_plot <- (
  (plots[["30"]][["unstable"]] |
     plots[["30"]][["neutral"]] |
     plots[["30"]][["stable"]]) /
    (plots[["70"]][["unstable"]] |
       plots[["70"]][["neutral"]] |
       plots[["70"]][["stable"]]) /
    (plots[["148"]][["unstable"]] |
       plots[["148"]][["neutral"]] |
       plots[["148"]][["stable"]])
) +
  plot_layout(
    widths = c(1, 1, 1),
    heights = c(1, 1, 1),
    guides = "collect"
  ) &
  theme(
    plot.margin = margin(0, 0, 0, 0.3)
  )

final_plot

# save as pdf
ggsave(
  filename = "./Footprint_model_Kljun_2015/footprint_height_stability_all.pdf",
  plot = final_plot,
  width = 17,
  height = 17,
  units = "cm",
  dpi = 300,
  device = cairo_pdf
)

# save legend to pdf
ggsave(
  filename = "./Footprint_model_Kljun_2015/footprint_height_stability_all_legend.pdf",
  plot = legend_only, 
  width = 6,
  height = 20,
  units = "cm",
  dpi = 300)

### add legend on pdf
library(magick)

# read PDFs
pdf1 <- image_read_pdf("./Footprint_model_Kljun_2015/footprint_height_stability_all.pdf", density = 300)
pdf2 <- image_read_pdf("./Footprint_model_Kljun_2015/footprint_height_stability_all_legend.pdf", density = 300)

# combine side by side
combined <- image_append(c(pdf1[1], pdf2[1]))

# save
image_write(combined, "./Footprint_model_Kljun_2015/footprint_height_stability_with_legend.pdf", format = "pdf")


################################### test area ####################################

