#### log wind profile ####

# load packages
library('tidyverse')


# load data
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/sonic_profile_data.RData") # EC data output from different heights

# `u_rot_[m+1s-1]`
# `u*_[m+1s-1]`

library(dplyr)
library(stringr)
library(ggplot2)

# constants
k <- 0.4                 # von Karman constant
d <- 12.667             # displacement height
z0 <- 1.9               # roughness length

# prepare data
wind <- sonic_profile_data %>%
  select(datetime, `u_rot_[m+1s-1]`, `u*_[m+1s-1]`, height) %>%
  group_by(height, datetime) %>%
  summarise(
    `u_rot_[m+1s-1]` = mean(`u_rot_[m+1s-1]`, na.rm = TRUE),
    `u*_[m+1s-1]` = mean(`u*_[m+1s-1]`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    height_numeric = as.numeric(str_replace(height, "m", "")),
    # take log to get linear scale, subtract displacement height and roughness length 
    X = log((height_numeric - d) / z0)
  )


# loop that fits log wind profile to every hour for one day

# select day here
time_seq <- seq(from = as.POSIXct("2021-07-04 00:00:00", tz = "UTC"),
                to   = as.POSIXct("2021-07-04 23:00:00", tz = "UTC"),
                by   = "hour")

# set pars to have 24 profiles
# reduce mars
par(mfrow = c(4, 6), 
    mar = c(1, 1, 1, 1))

for(i in time_seq){
  
  # skip in case of missing data
  if(nrow(wind_test) < 2) next
  
  # fit lm
  log_model <- lm(`u_rot_[m+1s-1]` ~ X, data = wind_test)
  
  # prediction grid (based on model predictor X)
  X_new <- seq(0,
               max(wind_test$X, na.rm = TRUE) + 1,
               length.out = 1000)
  # predict
  u_pred <- predict(log_model, newdata = data.frame(X = X_new))
  # transform
  z_pred <- d + z0 * exp(X_new)
  
  # plot observations
  plot(wind_test$`u_rot_[m+1s-1]`,
       wind_test$height_numeric,
       pch = 16,
       xlab = "wind speed [m/s]",
       ylab = "height [m]",
       ylim = c(0, 160),
       xlim = c(0, 11))
  
  # model curve
  lines(u_pred,
        z_pred,
        col = "red",
        lwd = 2)
  
  # canopy height 
  abline(h = 19, col = "darkgreen", lwd = 3)
}

# reset plotting layout
par(mfrow = c(1, 1))


#### each profile in the same plot ####

# select a day
time_seq <- seq(from = as.POSIXct("2021-07-10 00:00:00", tz = "UTC"),
                to   = as.POSIXct("2021-07-10 23:00:00", tz = "UTC"),
                by   = "30 min")

# set mars to default back
par(mar = c(5.1 ,4.1, 4.1, 2.1))

# empty plot
plot(NA,
     xlim = c(0, 11),
     ylim = c(0, 160),
     xlab = "wind speed [m/s]",
     ylab = "height [m]")

# canopy height line
abline(h = 19, col = "darkgreen", lwd = 3)

# start loop
for(i in time_seq){
  
  wind_test <- wind %>%
    filter(datetime >= i &
             datetime <  i + 1800)
  
  if(nrow(wind_test) < 2) next
  
  # fit model
  log_model <- lm(`u_rot_[m+1s-1]` ~ X, data = wind_test)
  
  # new u values (x_new)
  X_new <- seq(0,
               max(wind_test$X, na.rm = TRUE) + 1,
               length.out = 200)
  #predict
  u_pred <- predict(log_model,
                    newdata = data.frame(X = X_new))
  # transform/calculate y value
  z_pred <- d + z0 * exp(X_new)
  
  # measurements
  points(wind_test$`u_rot_[m+1s-1]`,
         wind_test$height_numeric,
         pch = 16,
         col = rgb(0, 0, 0, 0.3)) # adjust alpha
  
  # plot the profile
  lines(u_pred,
        z_pred,
        col = rgb(1, 0, 0, 0.5), # set alpha to 0.5
        lwd = 1)
}


