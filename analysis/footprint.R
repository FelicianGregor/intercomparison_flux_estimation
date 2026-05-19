##### footprint analysis #####


#### wind rose #####
library(openair) # use the openair package for windrose etc
library(tidyverse)
library(ggspatial)

# load data
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/sonic_profile_data.RData") # EC data output from different heights

# filter for just one height...
sonic_profile_data = sonic_profile_data%>%
  filter(folder == "2021_148m_double_linear")

# prepare data
wind_data = data.frame('wd' = as.numeric(sonic_profile_data[["wind_dir_[deg_from_north]"]]), 
                       'ws' = as.numeric(sonic_profile_data[["wind_speed_[m+1s-1]"]]), 
                       date = date(sonic_profile_data[["datetime"]]))

# do for full data series or per month
polarFreq(wind_data)
polarFreq(wind_data, type = "month")



#### simplified footprint #####'

# prepare data first
wind_dir = data.frame(
  # need wind direction 
  dir = as.numeric(sonic_profile_data[["wind_dir_[deg_from_north]"]]), 
  
  # and the contribution for the respective percentages in distance meter
  perc_10 = as.numeric(sonic_profile_data[["x_10%_[m]"]]), 
  perc_30 = as.numeric(sonic_profile_data[["x_30%_[m]"]]), 
  perc_50 = as.numeric(sonic_profile_data[["x_50%_[m]"]]), 
  perc_70 = as.numeric(sonic_profile_data[["x_70%_[m]"]]), 
  perc_90 = as.numeric(sonic_profile_data[["x_90%_[m]"]]), 
  
  # date
  datetime = sonic_profile_data$datetime
)

# filter out -9999
wind_dir[wind_dir == -9999.0000] <- NA

# wind direction in polar coordinates, but is needed in cartesian ones
# calculate footprint, convert from polar to cartesian coordinates
percentiles <- c("perc_10", "perc_30", "perc_50", "perc_70", "perc_90")

footprint <- wind_dir %>%
  mutate(
    two_hour = floor_date(datetime, "1 hours"), 
    season = case_when(
      month(datetime) %in% c(12,1,2) ~ "Winter",
      month(datetime) %in% c(6,7,8) ~ "Summer",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(two_hour) %>%
  summarise(
    across(all_of(percentiles), 
           ~mean(.x * sin(dir * pi / 180), na.rm = TRUE), 
           .names = "x_{.col}"),
    across(all_of(percentiles), 
           ~mean(.x * cos(dir * pi / 180), na.rm = TRUE), 
           .names = "y_{.col}"),
    season = season, 
    .groups = "drop"
  )

# reshape to long format
footprint_long <- footprint %>%
  pivot_longer(
    cols = matches("^(x|y)_perc_"),
    names_to = c(".value", "percentile"),
    names_pattern = "(x|y)_(perc_\\d+)"
  )

# plot
ggplot(footprint_long) +
  geom_point(aes(x = x, y = y, color = percentile), alpha = 0.1) +
  coord_equal() +
  labs(x = "X (m)", y = "Y (m)", color = "Percentile") +
  facet_grid(~season)+
  theme_bw()


##### mfootprint map ####
library(sf)

# make coordinates from local coordinate system to global

#tower
tower_lonlat <- st_sfc(
  st_point(c(13.4189, 56.0976)),crs = 4326) # tower coords from webpage Hyltemossa, in 4326
tower_utm <- st_transform(tower_lonlat, 32633) # transform to UTM 33N
tower_coords <- st_coordinates(tower_utm) # extract coordinates

# get tower coordinates in m
tower_x <- tower_coords[1]
tower_y <- tower_coords[2]

# add the footprint in m (since it is UTM)
# do this in the footprint dataframe, with x and y in m in there
footprint_long <- footprint_long %>%
  mutate(
    x_map = x + tower_x,
    y_map = y + tower_y
  )

# delete NA (dont do this, then i cannot analyse the missing data)
#footprint_long = na.omit(footprint_long)

# make it spatial again
footprint_sf <- st_as_sf(
  footprint_long%>%drop_na(),
  coords = c("x_map", "y_map"),
  crs = 32633
)

# make tower UTM 33N 
tower_sf <- st_sfc(
  st_point(c(tower_x, tower_y)),
  crs = 32633
)

ggplot() +
  geom_sf(data = footprint_sf, aes(color = percentile), alpha = 0.1) +
  geom_sf(data = tower_sf, color = "black", size = 4) +
  #facet_wrap(~season) +
  theme_bw()

### add basemap
library(sf)
library(ggplot2)
library(maptiles)

# Get bounding box
bbox <- st_bbox(footprint_sf)

# Download satellite tiles
tiles <- get_tiles(bbox, provider = "Esri.WorldImagery", zoom = 12)

ggplot() +
  geom_raster(data = tiles) +
  geom_sf(data = footprint_sf, fill = NA, color = "red")


### another approach
library(ggspatial)

ggplot() +
  annotation_map_tile(type = "osm") +
  geom_sf(data = footprint_sf, fill = NA, color = percentile) +
  coord_sf()

#####
library(stars)
library(sf)
library(ggplot2)
library(ggspatial)

map_dir = "C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/footprint_model_Kljun_2015/basemap_esri_world_imagery/"
AOI <- read_sf(paste0(map_dir, "AOI.geojson"))
base <- read_stars(paste0(map_dir, "basemap_20260515171916.257602.tif"))

box <- st_polygon(list(matrix(
  c(
    13.47, 56.17,   # lower left
    13.5575, 56.17,   # lower right
    13.5575, 56.1935,   # upper right
    13.47, 56.1935,   # upper left
    13.47, 56.17    # close polygon
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
footprint_sf = st_transform(footprint_sf, 4326)


# convert to rgb for plotting
base_rgb <- st_rgb(base)

# plot
ggplot() +
  geom_stars(data = base_rgb) +
  geom_sf(data = st_transform(bbox_sf, 3857),
          fill = "white",
          color = "lightgrey",
          linewidth = 0.3, alpha = 0.2)+
  coord_sf(crs = 3857, expand = FALSE) +
  annotation_north_arrow(
    location = "tr",
    pad_x = unit(0.2, "cm"),
    pad_y = unit(0.8, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  annotation_scale(
    location = "tr",
    height = unit(0.3, "cm")
  )

