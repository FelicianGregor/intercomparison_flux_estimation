#### get the boundary layer height from ERA5 data, as done also in: https://doi.org/10.5194/acp-20-13399-2020-supplement
library(terra)
library(sf)
library(rnaturalearth)
library(lubridate)

# read grib file
file_path = "C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/footprint_model_Kljun_2015/ERA5_reanalysis_BL_height_20_21.grib"

BL_h = rast(file_path)
BL_h


# Define location
lon <- 13.41897
lat <- 56.09763

# Create point
HTM_point <- vect(data.frame(lon, lat),
                  geom = c("lon", "lat"),
                  crs = "EPSG:4326")

# Extract all timesteps
BL_h_timeseries <- terra::extract(BL_h, HTM_point)

# Convert to dataframe
df <- data.frame(
  time = time(BL_h),
  height = as.numeric(BL_h_timeseries[1, -1])
)

df_2021 <- df[year(df$time) == 2021, ]

# change from 1hour to 30min resolution, since this is also the case for the fluxes, use linear interpolation

# create 30-min time sequence
new_time <- seq(min(df_2021$time), max(df_2021$time), by = "30 min")

# lin interpolate heights using approx
new_height <- approx(
  x = df$time,
  y = df$height,
  xout = new_time
)$y

df_30min <- data.frame(
  time = new_time,
  BL_height_m_a_g_l = new_height
)


# plot 
plot(df_30min$time, df_30min$BL_height_m_a_g_l,
     type = "l",
     xlab = "",
     ylab = "BL height [m]")


summary(df_30min$BL_height_m_a_g_l)
save(sonic_profile_data, file = "data/processed/sonic_profile_data.Rdata")

BL_h = df_30min
# write to file
save(BL_h, file = "C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/footprint_model_Kljun_2015/BL_height.Rdata")
