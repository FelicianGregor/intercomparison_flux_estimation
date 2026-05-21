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

# use only block average and double rotation sonic data (in folder column)
data = sonic_profile_data %>%
  filter(str_detect(folder, "double_block"))%>%
  # join the BL height data
  left_join(y = BL_h, by = "datetime")
  
# do some data preparation
data <- data %>%
  mutate(
    #make datetime column
    datetime = ymd_hm(paste(`date_[yyyy-mm-dd]`, `time_[HH:MM]`), tz = "UTC"),
    yyyy = year(datetime),
    mm   = month(datetime),
    day  = day(datetime),
    HH   = hour(datetime),
    MM   = minute(datetime),
    zm = as.numeric(str_extract(folder, "30|70|148")), # get measurement height as numeric from folder 
    u_mean = `wind_speed_[m+1s-1]`,
    L = `L_[m]`,
    sigma_v = sqrt(`v_var_[m+2s-2]`), # take the square root to get sd from var
    u_star = `u*_[m+1s-1]`,
    wind_dir = `wind_dir_[deg_from_north]`,
    h = BL_height_m_a_g_l, # kinda rename ABL heights to h
    
    #calculate displacement height d and roughness length z0
    d = 0.666 * 19, # mean canopy height is 19m
    z0 = 1.9  # approximate as 0.1*canopy height %>%
  )%>%
  # select required columns
  select(yyyy, mm, day, HH, MM, zm, d, z0, u_mean, L, sigma_v, u_star, wind_dir, h, datetime, `(z-d)/L_[#]`)%>%
  mutate(across(where(is.numeric), ~ replace_na(., -999)))


# select a shorter time frame during summer for test purpose
data = data%>%
  filter(datetime >= ymd("2021-01-01"),
         datetime <  ymd("2021-02-01"))%>%
  #filter(datetime == ymd("2021-07-30"))%>%
  select(-datetime)

# classify by stability, following classification by Biermann et al. 2014 (10.1007/s00704-013-0953-6)
# z-d/L <= -0.0625 --> unstable
# -0.0625 <= z-d/L <= 0.0625 --> neutral
# z-d/L > 0.0625 --> stable
classify_stability = function(zeta){
  ifelse(is.na(zeta), NA,
         ifelse(zeta < -0.0625, "unstable",
                ifelse(zeta < 0.0625, "neutral",
                       "stable")))
}

# apply stab classes
data = data %>%
  mutate(stability = classify_stability(zeta = `(z-d)/L_[#]`))

# stability regime and measurement heights (sonics) vector to loop over
heights = c(30, 70, 148)
stability_char = c("stable", "neutral", "unstable")
#stability_char = c("stable")

# container list to store results
FFP_results_all = list()

# start loop for heights
for (i in heights) {
  
  # create a list with heights
  FFP_results_all[[as.character(zm)]] = list()
  
  # use only heights zm of current loop
  data_zm = data%>%
    filter(zm == i)
  
  # loop through stab classes
  for (stab in stability_char) {
    
    data_stab = data_zm %>%
      # filter for stability
      filter(stability == stab)%>%
      # filter for Kljun 2015 model to be valid
      filter(
        u_star > 0.1,
        `(z-d)/L_[#]` >= -15.5,
        h > i
      )
    
    #if (nrow(data_stab) == 0) {next} # skip if no data in one class
    
    # define domain and decide whether to enlarge or keep default size of domain
    if(stab == "stable" & i == 148){
      domain = c(-20000, 20000, -20000, 20000) # enlarge
    }else{
      domain = c(-10000, 10000, -10000, 10000)
    }
    
    
    # run footprint model KLjun 2015
    FFP = calc_footprint_FFP_climatology(
      zm = i,
      z0 = 1.9,
      umean = data_stab$u_mean,
      h = data_stab$h,
      ol = data_stab$L,
      sigmav = data_stab$sigma_v,
      ustar = data_stab$u_star,
      wind_dir = data_stab$wind_dir,
      domain = domain,
      nx = 1000,
      r = c(25, 50, 75),
      smooth_data = 0,
      crop = 1
    )
    
    # write result in the nested list
    FFP_results_all[[as.character(i)]][[stab]] = FFP
  }
}


# plot quickly with relative coordinates
par(mfrow = c(3, 3))
for (j in heights) {
  for (stab in stability_char) {
    FFP = FFP_results_all[[as.character(j)]][[stab]]
    image.plot(
      FFP$x_2d[1,],
      FFP$y_2d[,1],
      FFP$fclim_2d,
      main = paste(stab, j),ylab = "Y in m",xlab = "X in m")
    for (k in seq_along(FFP$xr)) {
      lines(FFP$xr[[k]], FFP$yr[[k]], col = "red")
    }
  }
}
par(mfrow = c(1, 1)) # set back to default


# caluclate number of 30min intervals in each:
library(purrr)
fraction_table <- imap_dfr(FFP_results_all, function(stab_list, height) {
  imap_dfr(stab_list, function(ffp, stability) {
    tibble(
      height = as.numeric(height),
      stability = stability,
      n = ffp$n
    )
  })
})

# fraction of each stability regime (from 100%)
fraction_table$n = fraction_table$n / (30 * 48)

# fraction of 30hour fluxes the footprint could be calculated for
fraction_table%>%
  group_by(height)%>%
  summarise(fraction = sum(n))

# compare with wind rose, as recommended in the README of KLjun 2015 R tool 
library(openair)
# prepare data
wind_data = data.frame('wd' = data$wind_dir, 
                       'ws' = data$u_mean)

# do for full data series or per month
polarFreq(wind_data)
# seems like no transpose needed here...


