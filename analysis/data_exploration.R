#### data exploration ####

# packages
library(tidyverse)
library(ggpmisc)

# load data
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/slow_profile_data.RData") # load the 14 level profile data for Ta and Humidity
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/Eco_data_30m.RData") # Ecosystem data 30m
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/slow_profile_data.RData") # 14 level profile data

##############################
##### EC data processing #####
##############################

# - compare the different heights
# - get u*, L 
# - get the K values, over time

# compare H from 3 heights to Eco data using a scatter plot 
sonic_profile_data %>%
  left_join(
    Eco_data_30m %>%
      rename(H_Wm2_Eco = H_Wm2) %>%
      select(datetime, H_Wm2_Eco),
    by = "datetime"
  ) %>%
  #filter(
  # between(date(datetime),
  #        ymd("2021-01-21"),
  #        ymd("2021-05-26"))
  #) %>%
  filter(`qc_H_[#]` != 2)%>%
  # filter out spikes very roughly
  filter(abs(H_ensemble_mean) < 750 )%>%
  ggplot(aes(x = H_Wm2_Eco, y = H_ensemble_mean)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 2) +
  geom_smooth(method = "lm", color = "red")+
  # add the equation of the linear model + R2
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE,
    size = 4, color = "black"
  )+
  coord_equal(xlim = c(-170, 750),
              ylim = c(-170, 750))+
  facet_grid(~height)+
  labs(y = "H [Wm2]", 
       x = "H [Wm2] 30m Ecosystem station")+
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

# compare the heights as time series plot
# time series plot with all the data processing options
height_comp <- sonic_profile_data %>%
  filter(
    between(date(datetime),
            ymd("2021-05-21"),
            ymd("2021-05-26"))
  ) %>%
  mutate(`H_[W+1m-2]` = 
           ifelse(`qc_H_[#]`==  2, yes = NA, no = `H_[W+1m-2]`))%>%
  ggplot() +
  geom_line(aes(y = `H_[W+1m-2]`, x = datetime, color = folder)) +
  theme_bw()

height_comp

### comparison of u_star, L etc from different heights ####
# calculate ensenble mean value for each height, averaging over the rotation, block etc.
# use only high quality data 
ensemble = sonic_profile_data%>%
  filter(`qc_H_[#]` != 2)%>%
  group_by(datetime, height)%>%
  summarise(
    u_star_mean = mean(`u*_[m+1s-1]`), 
    L_mean = mean(`L_[m]`), 
    TKE_mean = mean(`TKE_[m+2s-2]`),
    H_wm2_mean = mean(`H_[W+1m-2]`), 
    z_d_L_mean = mean(`(z-d)/L_[#]`),
    .groups = 'drop' # drop the grouping by datetime and height
  )%>%
  mutate(
    height = as.numeric(str_replace(height, "m", ""))
  )

ensemble = ensemble%>%
  left_join(Eco_data_30m%>%
              rename(H_Wm2_Eco = H_Wm2, 
                                  USTAR_Eco = USTAR)%>%
              select(H_Wm2_Eco, USTAR_Eco, datetime, P_ground_hPa), 
            by = "datetime")

# compare the different heights 
# ustar
ensemble%>%
  filter(abs(H_wm2_mean) < 800 )%>%
  ggplot()+
  geom_boxplot(aes(x = as.factor(height), y = u_star_mean))+
  labs(x = "height [m]", 
       y = "u*")+
  theme_bw()

# z_d_L_mean using boxplots
ensemble%>%
  filter(abs(H_wm2_mean) < 800 )%>%
  filter(abs(z_d_L_mean) <200)%>%
  ggplot()+
  geom_boxplot(aes(x = as.factor(height), y = z_d_L_mean))+
  labs(x = "height [m]", 
       y = "z-d/L")+
  theme_bw()

# time series plot
ob_ts = ensemble%>%
  filter(abs(H_wm2_mean) < 800 )%>%
  filter(abs(z_d_L_mean) < 200)%>%
  filter(between(date(datetime),
                 ymd("2021-06-01"),
                 ymd("2021-07-16")))%>%
  ggplot()+
  geom_line(aes(x = datetime, y = z_d_L_mean, 
                   color = as.factor(height)))+
  labs(x = "", 
       y = "z-d/L")+
  theme_bw()

ggplotly(ob_ts)

# calculate mean diurnal course of z-d/L during summer
ensemble%>%
  filter(abs(H_wm2_mean) < 800 )%>%
  filter(abs(z_d_L_mean) < 200)%>%
  # create hour variable
  mutate(hour = hour(datetime) + minute(datetime)/60)%>%
  mutate(month = month(datetime))%>%
  group_by(month, hour) %>%
  summarise(
    mean_val   = mean(z_d_L_mean, na.rm = TRUE),
    median_val = median(z_d_L_mean, na.rm = TRUE),
    p95_upper  = quantile(z_d_L_mean, 0.975, na.rm = TRUE),
    p95_lower  = quantile(z_d_L_mean, 0.025, na.rm = TRUE),
    .groups = 'drop'
  )%>%
  ggplot(aes(x = hour))+
  geom_ribbon(aes(ymin = p95_lower, ymax = p95_upper),
              fill = "grey70", alpha = 0.4) +
  geom_line(aes(y = mean_val), color = "blue", linewidth = 1) +
  geom_abline(slope = 0, intercept = 0, col = "black", linewidth = 0.5)+
  coord_cartesian(ylim = c(-25, 25))+
  theme_bw() +
  labs(x = "hour", y = "z-d/L")+
  facet_wrap(~month, ncol = 4, nrow = 3)


# Obhukov length L
ensemble%>%
  filter(abs(H_wm2_mean) < 800 )%>%
  ggplot()+
  geom_boxplot(aes(x = as.factor(height), y = L_mean))+
  labs(x = "height [m]", 
       y = "Obhukov length [m]")+
  theme_bw()

# as time series
ggplot(ensemble%>%
        filter(abs(H_wm2_mean) < 800 )%>%
         mutate(
           L_mean_filtered = L_mean,
           L_mean_filtered = ifelse(L_mean_filtered > 20000, 20000, L_mean_filtered),
           L_mean_filtered = ifelse(L_mean_filtered < -20000, -20000, L_mean_filtered)
         )%>%
        filter(between(date(datetime),
                         ymd("2021-04-21"),
                         ymd("2021-05-16")))
       )+
  geom_line(aes(x = datetime, y = L_mean_filtered, color = as.factor(height)))+
  theme_bw()

library(plotly)
ggplotly(L)
  
# do corr plot of L etc between different heights
# data prep
corr_data = ensemble %>%
  pivot_wider(
    names_from = height, 
    values_from = L_mean
  )%>%
  group_by(datetime)%>%
  select(`148`, `30`, `70`)%>%
  summarise(
    `148` = mean(`148`, na.rm = T), 
    `30` = mean(`30`, na.rm = T), 
    `70` = mean(`70`, na.rm = T), 
    .groups = "drop"
  )%>%
  select(-datetime)


library(psych)
# do the plot
psych::pairs.panels(corr_data)
# Oh, no correlation at all??
psych::pairs.panels(log(corr_data))
# do a log transformationn and it does not look too bad

##### eddy diffusivity #####

# define a function to get K_H from H (from different heights)
K_from_H = function(
    Ta_dgC_up, 
    Ta_dgC_down, 
    height_diff_m, 
    P_ground_hPa, 
    H_Wm2_EC_measured,
    u_star
){

  # calculate delta T
  delta_Ta_dgC = Ta_dgC_up - Ta_dgC_down
  # filter Ta difference by minimal difference
  threshold = 0.1 # K
  delta_Ta_dgC = ifelse(abs(delta_Ta_dgC) > threshold, 
                        yes = delta_Ta_dgC, 
                        no = NA)
  
  # prep for calcations
  c_p = 1004.834 # specific heat of air at constant pressure 
  
  # latent heat of vaporization is temperature dependent, use Foken 2012 Micrometeorology, page 38
  # use uppermost T as variable, although one could also use the mean (I could implement this later)
  lambda = 2500827-2360*Ta_dgC_up # in J/Kg
  
  # density of air, following Foken 2012, Micrometeorology, page 38
  # ATTENTION! I did not use the virtual Temperature!
  # Attention, I simply ue the uppermost temperature!
  R_L = 287.058655 #J/kg*K, from Foken 2012, Micrometeo, page 245
  rho = (P_ground_hPa *100)/ (R_L*Ta_dgC_up) # attention, uppermost Temperature, and also not virtual!
  
  ######### K-approach: use sensile heat flux to obtain K (eddy diffusivity) #########
  #latent heat:
  K_H = - H_Wm2_EC_measured / (rho * c_p * (delta_Ta_dgC / height_diff_m))
  
  
  ######### additional filtering criteria?? #########
  #filtering based on u*
  K_H = ifelse(u_star < 0.15, yes = NA, no = K_H)

  # return sensible heat as default
return(K_H)
}

K_data = ensemble%>%
  left_join(slow_profile_data%>%
              select(Ta_19m, Ta_40m, Ta_55m, Ta_85m, Ta_125m, Ta_148m, datetime), by= "datetime")%>%
  # for 30m height
  filter(height == 30)%>%
  mutate(K_30m = K_from_H(
    Ta_dgC_up = Ta_40m, 
    Ta_dgC_down = Ta_19m, 
    height_diff_m = 21, 
    P_ground_hPa = P_ground_hPa, 
    H_Wm2_EC_measured = H_wm2_mean, 
    u_star = u_star_mean
  ))

ggplot(K_data)+
  geom_line(aes(x = datetime, y = K_30m))
  


ensemble%>%
  filter(abs(H_wm2_mean) < 800 )%>%
  filter(abs(z_d_L_mean) < 200)%>%
  # create hour variable
  mutate(hour = hour(datetime) + minute(datetime)/60)%>%
  mutate(month = month(datetime))%>%
  group_by(month, hour) %>%
  summarise(
    mean_val   = mean(z_d_L_mean, na.rm = TRUE),
    median_val = median(z_d_L_mean, na.rm = TRUE),
    p95_upper  = quantile(z_d_L_mean, 0.975, na.rm = TRUE),
    p95_lower  = quantile(z_d_L_mean, 0.025, na.rm = TRUE),
    .groups = 'drop'
  )%>%
  ggplot(aes(x = hour))+
  geom_ribbon(aes(ymin = p95_lower, ymax = p95_upper),
              fill = "grey70", alpha = 0.4) +
  geom_line(aes(y = mean_val), color = "blue", linewidth = 1) +
  geom_abline(slope = 0, intercept = 0, col = "black", linewidth = 0.5)+
  coord_cartesian(ylim = c(-25, 25))+
  theme_bw() +
  labs(x = "hour", y = "z-d/L")+
  facet_wrap(~month, ncol = 4, nrow = 3)


##### gradients #####



  



  