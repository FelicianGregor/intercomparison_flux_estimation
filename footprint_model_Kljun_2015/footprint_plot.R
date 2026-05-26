
# load packages
library(sf)
library(purrr)
library(dplyr)
library(ggplot2)
library(basemaps)
library(stars)
library(ggspatial)


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

# tower location: ref_utm (this is not utm anymore)

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
  "Non and sparsely vegetated" = "#7A7A7A",
  "Lichens and mosses" = "#F0E442",
  # Herbaceous / agriculture
  "Periodically herbaceous" = "#FEE08B",
  "Permanent herbaceous" = "wheat3",
  # Shrubs / low woody
  "Low-growing woody plants" = "#A6D96A",
  # forests
  "Woody broadleaved deciduous trees" = "#3FA34D",  
  "Woody needle leaved trees" = "#1F5F5B",
  "Sealed" = "#D7191C"
)

# make categories
footprint <- footprint %>%
  mutate(percentage = case_when(percentile == 1 ~ "25%",
                                percentile == 2 ~ "50%",
                                percentile == 3 ~ "75%"))

# plt in ggplot
plot_test = ggplot()+
  geom_stars(data = land_cover)+
  geom_sf(data = footprint, aes(linetype = percentage), fill = NA, size = 1.5, color = "black")+
  geom_sf(data = ref_utm, aes(color = "Tower ICOS Se-Htm"), size = 3) +
  scale_color_manual(
    name = NULL,
    values = c("Tower ICOS Se-Htm" = "red")
  )+
  scale_fill_manual(values = landcover_colors) +
  coord_sf(expand = FALSE)+
  labs(x = "", y = "", fill = "Land cover class")+
  annotation_north_arrow(
    location = "tr",  # top right
    which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) +
  annotation_scale(
    location = "tr",  # top right
    width_hint = 0.3, text_cex = 1.4
  ) +
  # set explicitly only 5 breaks
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 3)
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 3)
  )+
  # change the line type
  scale_linetype_manual(
    values = c(
      "25%" = "solid",
      "50%" = "longdash",
      "75%" = "3313"), name = "Contribution") +
  theme_bw()+
  theme(
    axis.text.y = element_text(
      angle = 90,
      hjust = 0.5,
      vjust = 0.5), legend.position = "right")



# save the legend for later
library(cowplot)
legend_only = get_legend(plot_test)


##### do this for all heights #####
heights <- c("30", "70", "148")
stabilities <- c("unstable", "neutral", "stable")
land_cover= read_stars("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/footprint_model_Kljun_2015/basemap_CLCplus_Backbone_2023/CLMS_CLCPLUS_RAS_S2023_R10m_E45N36_03035_V01_R00.tif", proxy = F)


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
    )
    
    footprint <- footprint %>%
      mutate(percentage = case_when(
        percentile == 1 ~ "25%",
        percentile == 2 ~ "50%",
        percentile == 3 ~ "75%"
      ))
    
    land_cover_crop <- st_crop(land_cover, squared_bbox(footprint))
    
    # downsample for faster plotting
    if(h == "148" & s == "stable"){
      land_cover = st_downsample(land_cover_crop, n = 3)
    }
    
    p <- ggplot() +
      geom_stars(data = land_cover_crop) +
      geom_sf(data = footprint,
              aes(linetype = percentage),
              fill = NA, size = 1.1, color = "black") +
      geom_sf(data = ref_utm,
              aes(color = "Tower ICOS Se-Htm"),
              size = 3) +
      scale_color_manual(values = c("Tower ICOS Se-Htm" = "red")) +
      # set explicitly only 5 breaks
      scale_x_continuous(
        breaks = scales::pretty_breaks(n = 3)) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 3))+
      scale_fill_manual(values = landcover_colors) +
      scale_linetype_manual(values = c(
        "25%" = "solid",
        "50%" = "longdash",
        "75%" = "3313"
      )) +
      coord_sf(expand = FALSE) +
      labs(title = paste0(h,"m, ", s),
           x = "", y = "") +
      theme_bw()+
      theme(
        axis.text.y = element_text(
          angle = 90,
          hjust = 0.5,
          vjust = 0.5), 
        axis.text = element_text(size = 8)) +
      theme(legend.position = "none", 
            plot.title = element_text(hjust = 0.5))
    
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
)

# save as pdf
ggsave(
  filename = "./Footprint_model_Kljun_2015/footprint_height_stability_all.pdf",
  plot = final_plot,
  width = 20,
  height = 20,
  units = "cm",
  dpi = 300,
  device = cairo_pdf
)

# add legend
final_plot 
