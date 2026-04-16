#### data exploration ####

# packages
library(tidyverse)
library(ggpmisc)
library(plotly)
library(patchwork) # to combine plots

# load data
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/slow_profile_data.RData") # load the 14 level profile data for Ta and Humidity
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/Eco_data_30m.RData") # Ecosystem data 30m
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/sonic_profile_data.RData") # load profile data

# define two weeks (one in summer and one in winter) to use throughout the analysis
winter_week <- seq(
  from = as.POSIXct("2021-01-23 00:00:00", tz = "UTC"),
  to   = as.POSIXct("2021-01-30 00:00:00", tz = "UTC"),
  by   = "30 min")

summer_week = seq(
  from = as.POSIXct("2021-06-23 00:00:00", tz = "UTC"),
  to   = as.POSIXct("2021-06-30 00:00:00", tz = "UTC"),
  by   = "30 min")

# data filtering by R_net-based threshold
sonic_profile_data = sonic_profile_data %>%
  left_join(
    Eco_data_30m %>%
      select(datetime, H_Wm2, R_Net_Wm2) %>%
      rename(H_Wm2_Eco = H_Wm2),
    by = "datetime"
  ) %>%
  mutate(
    H_ensemble_mean = ifelse(
      abs(H_ensemble_mean) > abs(R_Net_Wm2), 
      yes = NA, 
      no = H_ensemble_mean))%>%
  mutate(`H_[W+1m-2]` = ifelse(
    abs(`H_[W+1m-2]`) > abs(`H_[W+1m-2]`), 
    yes = NA, 
    no = `H_[W+1m-2]`))

ggplot(sonic_profile_data) +
  geom_line(aes(x = datetime, y = `H_[W+1m-2]`), col = "red")

#ggplotly(filtering)

##############################
##### EC data processing #####
##############################

# - compare the different heights - done 
# - get u*, L 
# - get the K values, over time

#### compare H from 3 heights to Eco ####
sonic_profile_data%>%
  #filter(datetime %in% winter_week)%>%
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
  labs(title = "no additional filter")+
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), 
    strip.background = element_rect(fill = NA, color = 'black', linewidth = 0.5)
  )

#### compare for filtered by u* ####
sonic_profile_data %>%
  filter(`qc_H_[#]` != 2)%>%
  # u* classification
  mutate(
    u_star_class = case_when(
      `u*_[m+1s-1]` <= 0.15 ~ "u* ≤ 0.15",
      `u*_[m+1s-1]` > 0.15  ~ "u* > 0.15"
    )
  ) %>%
  # filter out spikes very roughly
  filter(abs(H_ensemble_mean) < 750 )%>%
  ggplot(aes(x = H_Wm2_Eco, y = H_ensemble_mean)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 1) +
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
  facet_grid(u_star_class~height)+
  labs(y = "H [Wm2]", 
       x = "H [Wm2] 30m Ecosystem station")+
  theme_classic() +
  labs(title = "U* and heights")+
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), 
    strip.background = element_rect(fill = NA, color = 'black', linewidth = 0.5)
  )


# stability, heights
sonic_profile_data %>%
  filter(`qc_H_[#]` != 2) %>%
  mutate(
    stability = case_when(
      `L_[m]` > 50  ~ "stable (L > 50)",
      `L_[m]` < -50 ~ "unstable (L < -50)",
      TRUE          ~ "neutral (abs(L) < 50)"
    )
  ) %>%
  filter(abs(H_ensemble_mean) < 750) %>%
  ggplot(aes(x = H_Wm2_Eco, y = H_ensemble_mean)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 1) +
  geom_smooth(aes(color = stability), method = "lm") +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label),
                      after_stat(rr.label),
                      sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 4,
    color = "black",
    label.y = 0.95
  ) +
  coord_equal(xlim = c(-170, 750),
              ylim = c(-170, 750)) +
  facet_grid(stability ~ height) +
  labs(
    y = "H [Wm2]",
    x = "H [Wm2] 30m Ecosystem station"
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), 
    strip.background = element_rect(fill = NA, color = 'black', linewidth = 0.5)
  )

#### daytime, heights ####
sonic_profile_data %>%
  mutate(
    day_night = factor(`daytime_[1=daytime]`,
                       levels = c(0, 1),
                       labels = c("Night", "Day"))
  ) %>%
  filter(abs(H_ensemble_mean) < 750 ) %>%
  ggplot(aes(x = H_Wm2_Eco, y = H_ensemble_mean)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 1) +
  geom_smooth(method = "lm", color = "darkgrey") +
  geom_smooth(aes(color = day_night), method = "lm")+
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    parse = TRUE,
    size = 3, color = "black"
  ) +
  coord_equal(xlim = c(-170, 750),
              ylim = c(-170, 750)) +
  facet_grid(day_night~height) + 
  labs(y = "H [Wm2]", 
       x = "H [Wm2] 30m Ecosystem station")+
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), 
    strip.background = element_rect(fill = NA, color = 'black', linewidth = 0.5)
  )

##### daytime and stability ####
sonic_profile_data%>%
  filter(`qc_H_[#]` != 2) %>%
  # create stability classes
  mutate(
    stability = case_when(
      `L_[m]` > 50  ~ "stable (L > 50)",
      `L_[m]` < -50 ~ "unstable (L < -50)",
      TRUE          ~ "neutral (abs(L) < 50)"
    )
  )%>%
  # classify day and nightime
  mutate(
    day_night = factor(`daytime_[1=daytime]`,
                       levels = c(0, 1),
                       labels = c("Night", "Day"))
  ) %>%
  filter(abs(H_ensemble_mean) < 750 ) %>%
  ggplot(aes(x = H_Wm2_Eco, y = H_ensemble_mean)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linewidth = 1) +
  # overall regression (ALL data in panel)
  geom_smooth(method = "lm",
              color = "darkgrey",
              linewidth = 1.5) +
  # eqn for all data
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 4,
    color = "black"
  ) +
  # day/night regressions
  geom_smooth(aes(color = day_night),
              method = "lm",
              linewidth = 1.2) +
  # eqn for day night time
  stat_poly_eq(
    aes(color = day_night,
        label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 4, 
    label.y = "bottom"
  ) +
  # some visuals 
  coord_equal(xlim = c(-170, 750),
              ylim = c(-170, 750)) +
  facet_grid(stability ~ height) +
  scale_color_manual(
    name = "Condition",
    values = c("Day" = "orange",
               "Night" = "blue")
  ) +
  labs(y = "H [Wm2]", 
       x = "H [Wm2] 30m Ecosystem station",
       title = "day, night, stability") +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = NA, color = 'black', linewidth = 0.5)
  )

#### u*, stability, heights ####
sonic_profile_data %>%
  filter(`qc_H_[#]` != 2) %>%
  # stability classes
  mutate(
    stability = case_when(
      `L_[m]` > 50  ~ "stable (L > 50)",
      `L_[m]` < -50 ~ "unstable (L < -50)",
      TRUE          ~ "neutral (abs(L) < 50)"
    )
  ) %>%
  # u* classification
  mutate(
    u_star_class = case_when(
      `u*_[m+1s-1]` <= 0.15 ~ "u* ≤ 0.15",
      `u*_[m+1s-1]` > 0.15  ~ "u* > 0.15"
    )
  ) %>%
  filter(abs(H_ensemble_mean) < 750) %>%
  ggplot(aes(x = H_Wm2_Eco, y = H_ensemble_mean)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linewidth = 1) +
  # overall regression
  geom_smooth(method = "lm",
              color = "darkgrey",
              linewidth = 1.5) +
  # + eqn
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label),
                      after_stat(rr.label),
                      sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 4,
    color = "black",
    label.y = 0.05
  ) +
  # u* group regressions
  geom_smooth(aes(color = u_star_class),
              method = "lm",
              linewidth = 1.2) +
  # + eqn
  stat_poly_eq(
    aes(color = u_star_class,
        label = paste(after_stat(eq.label),
                      after_stat(rr.label),
                      sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 4,
  ) +
  # some visuals
  coord_equal(xlim = c(-170, 750),
              ylim = c(-170, 750)) +
  facet_grid(stability ~ height) +
  scale_color_manual(
    name = "u* class",
    values = c("u* ≤ 0.15" = "red",
               "u* > 0.15" = "blue")) +
  labs(
    y = "H [Wm2]",
    x = "H [Wm2] 30m Ecosystem station",
    title = "u* classes + stability"
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), 
    strip.background = element_rect(fill = NA, color = 'black', linewidth = 0.5)
  )


# compare the heights as time series plot
# time series plot with all the data processing options 
# for winter and summer week

#summer week
height_comp_summer <- sonic_profile_data %>%
  filter(
    datetime %in% summer_week
    ) %>%
  mutate(`H_[W+1m-2]` = ifelse(`qc_H_[#]` == 2, NA, `H_[W+1m-2]`)) %>%
  ggplot() +
  geom_line(
    aes(
      x = datetime,
      y = `H_[W+1m-2]`,
      group = folder,
      color = factor(as.factor(str_replace(height, pattern = "m", "")))),alpha = 0.6) +
  scale_color_manual(
    name = "height", # legend title
    values = c("30" = "darkgreen",
               "70" = "blue",
               "148" = "red"),
    labels = c("30" = "30 m",
               "70" = "70 m",
               "148" = "148 m")) +
  labs(x = "", y = "H [Wm2]")+
  theme_classic()+
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), 
    strip.background = element_rect(fill = NA, color = 'black', linewidth = 0.5)
  )

height_comp_summer


# winter week
height_comp_winter <- sonic_profile_data %>%
  filter(
    datetime %in% winter_week
  ) %>%
  mutate(`H_[W+1m-2]` = ifelse(`qc_H_[#]` == 2, NA, `H_[W+1m-2]`)) %>%
  ggplot() +
  geom_line(
    aes(
      x = datetime,
      y = `H_[W+1m-2]`,
      group = folder,
      color = factor(as.factor(str_replace(height, pattern = "m", "")))),alpha = 0.6) +
  scale_color_manual(
    name = "height", # legend title
    values = c("30" = "darkgreen",
               "70" = "blue",
               "148" = "red"),
    labels = c("30" = "30 m",
               "70" = "70 m",
               "148" = "148 m")) +
  labs(x = "", y = "H [Wm2]")+
  theme_classic()+
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), 
    strip.background = element_rect(fill = NA, color = 'black', linewidth = 0.5)
  )

# combine
height_comp_winter / height_comp_summer

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

H_summer = ensemble %>%
  filter(datetime %in% summer_week) %>%
  ggplot() +
  geom_line(aes(x = datetime, y = H_wm2_mean,
                color = as.factor(height))) +
  labs(x = "", y = "H") +
  theme_bw()

# time series plot L
L_summer = ensemble%>%
  filter(datetime %in% summer_week)%>%
  filter(L_mean>-1000)%>%
  #filter(abs(z_d_L_mean) < 200)%>%
  filter(between(date(datetime),
                 ymd("2021-06-01"),
                 ymd("2021-07-16")))%>%
  ggplot()+
  geom_line(aes(x = datetime, y = L_mean, 
                   color = as.factor(height)))+
  labs(x = "", 
       y = "L")+
  theme_bw()

z_d_L_summer = ensemble%>%
  filter(datetime %in% summer_week)%>%
  filter(z_d_L_mean>-100)%>%
  #filter(abs(z_d_L_mean) < 200)%>%
  filter(between(date(datetime),
                 ymd("2021-06-01"),
                 ymd("2021-07-16")))%>%
  ggplot()+
  geom_line(aes(x = datetime, y = z_d_L_mean, 
                color = as.factor(height)))+
  labs(x = "", 
       y = "z-d/L")+
  theme_bw()

u_star_summer = ensemble%>%
  filter(datetime %in% summer_week)%>%
  ggplot()+
  geom_line(aes(x = datetime, y =u_star_mean, 
                color = as.factor(height)))+
  labs(x = "", 
       y = "u* [m/s]")+
  theme_bw()

H_summer / u_star_summer/z_d_L_summer / L_summer

#### for winter ####
H_winter = ensemble %>%
  filter(datetime %in% winter_week) %>%
  ggplot() +
  geom_line(aes(x = datetime, y = H_wm2_mean,
                color = as.factor(height))) +
  labs(x = "", y = "H") +
  theme_bw()

L_winter = ensemble %>%
  filter(datetime %in% winter_week) %>%
  filter(abs(L_mean) < 1000) %>%
  ggplot() +
  geom_line(aes(x = datetime, y = L_mean,
                color = as.factor(height))) +
  labs(x = "", y = "L") +
  theme_bw()

z_d_L_winter = ensemble %>%
  filter(datetime %in% winter_week) %>%
  filter(z_d_L_mean > -200) %>%
  ggplot() +
  geom_line(aes(x = datetime, y = z_d_L_mean,
                color = as.factor(height))) +
  labs(x = "", y = "z-d/L") +
  theme_bw()

u_star_winter = ensemble %>%
  filter(datetime %in% winter_week) %>%
  ggplot() +
  geom_line(aes(x = datetime, y = u_star_mean,
                color = as.factor(height))) +
  labs(x = "", y = "u* [m/s]") +
  theme_bw()

H_winter / u_star_winter / z_d_L_winter / L_winter


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


# as time series
ggplot(ensemble%>%
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
  K_H =  - H_Wm2_EC_measured / (rho * c_p * (delta_Ta_dgC / height_diff_m))
  
  
  ######### additional filtering criteria?? #########
  #filtering based on u*
  K_H = ifelse(u_star < 0.15, yes = NA, no = K_H)

  # return sensible heat as default
return(K_H)
}

# 
K_data_30 = ensemble%>%
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
  ))%>%
  mutate(
    delta_Ta_30 = Ta_40m-Ta_19m
  )

K_data_70 = ensemble%>%
  left_join(slow_profile_data%>%
              select(Ta_19m, Ta_40m, Ta_55m, Ta_85m, Ta_125m, Ta_148m, datetime), by= "datetime")%>%
  # for 70m height
  filter(height == 70)%>%
  mutate(K_70m = K_from_H(
    Ta_dgC_up = Ta_85m, 
    Ta_dgC_down = Ta_55m, 
    height_diff_m = 20, 
    P_ground_hPa = P_ground_hPa, 
    H_Wm2_EC_measured = H_wm2_mean, 
    u_star = u_star_mean
  ))%>%
  mutate(
    delta_Ta_70 = Ta_85m-Ta_55m
  )

K_data_148 = ensemble%>%
  left_join(slow_profile_data%>%
              select(Ta_19m, Ta_40m, Ta_55m, Ta_85m, Ta_125m, Ta_148m, datetime), by= "datetime")%>%
  # for 148m height
  filter(height == 148)%>%
  mutate(K_148m = K_from_H(
    Ta_dgC_up = Ta_148m, 
    Ta_dgC_down = Ta_125m, 
    height_diff_m = 23, 
    P_ground_hPa = P_ground_hPa, 
    H_Wm2_EC_measured = H_wm2_mean, 
    u_star = u_star_mean
  ))%>%
  mutate(
    delta_Ta_148 = Ta_148m-Ta_125m
  )

#plot the gradients - all together
grad = K_data_30%>%
  filter(datetime %in% summer_week)%>%
  ggplot()+
  #geom_line(aes(x = datetime, y = K_30m))+
  geom_line(aes(x = datetime, y = K_30m), col = "red")+
  geom_line(aes(x = datetime, y = -delta_Ta_30), col = "blue")+
  labs(y = "delta Ta (40-19)", x = "")+
  theme_bw()

#ggplotly(grad)

# calculate K from u_star (for the neutral case)
#K = K_data%>%
#  mutate(K_neutral = 0.4*u_star_mean * (30-12.6))%>%
#  ggplot()+
#  geom_line(aes(x = datetime, y = K_30m))+
#  geom_line(aes(x = datetime, y = K_neutral), col = "red")

#ggplotly(K)

# plot all the K's and gradients: summer
K_summer = K_data_30%>%
  filter(datetime %in% summer_week)%>%
  left_join(
    K_data_70%>%select(K_70m, delta_Ta_70, datetime), 
    by = "datetime"
  )%>%
  left_join(
    K_data_148%>%select(K_148m, delta_Ta_148, datetime), 
    by = "datetime"
  )%>%
  ggplot()+
  geom_line( aes(
    x = datetime, 
    y = K_30m), color = "red"
  )+
  geom_line( aes(
    x = datetime, 
    y = K_70m), color = "darkgreen"
  )+
  
  geom_line( aes(
    x = datetime, 
    y = K_148m), color = "blue"
  )+
  labs(y = "K", x = "")+
  theme_bw()

grad_summer = K_data_30%>%
  filter(datetime %in% summer_week)%>%
  left_join(
    K_data_70%>%select(K_70m, delta_Ta_70, datetime), 
    by = "datetime"
  )%>%
  left_join(
    K_data_148%>%select(K_148m, delta_Ta_148, datetime), 
    by = "datetime"
  )%>%
  ggplot()+
  geom_line( aes(
    x = datetime, 
    y = delta_Ta_30), color = "red"
  )+
  geom_line( aes(
    x = datetime, 
    y = delta_Ta_70), color = "darkgreen"
  )+
  
  geom_line( aes(
    x = datetime, 
    y = delta_Ta_148), color = "blue"
  )+
  labs(y = "delta Ta [K]", x = "")+
  theme_bw()

K_summer / grad_summer 

# plot all the K's and gradients: summer
K_winter = K_data_30%>%
  filter(datetime %in% winter_week)%>%
  left_join(
    K_data_70%>%select(K_70m, delta_Ta_70, datetime), 
    by = "datetime"
  )%>%
  left_join(
    K_data_148%>%select(K_148m, delta_Ta_148, datetime), 
    by = "datetime"
  )%>%
  ggplot()+
  geom_line( aes(
    x = datetime, 
    y = K_30m), color = "red"
  )+
  geom_line( aes(
    x = datetime, 
    y = K_70m), color = "darkgreen"
  )+
  
  geom_line( aes(
    x = datetime, 
    y = K_148m), color = "blue"
  )+
  labs(y = "K", x = "")+
  theme_bw()

grad_winter = K_data_30%>%
  filter(datetime %in% winter_week)%>%
  left_join(
    K_data_70%>%select(K_70m, delta_Ta_70, datetime), 
    by = "datetime"
  )%>%
  left_join(
    K_data_148%>%select(K_148m, delta_Ta_148, datetime), 
    by = "datetime"
  )%>%
  ggplot()+
  geom_line( aes(
    x = datetime, 
    y = delta_Ta_30), color = "red"
  )+
  geom_line( aes(
    x = datetime, 
    y = delta_Ta_70), color = "darkgreen"
  )+
  
  geom_line( aes(
    x = datetime, 
    y = delta_Ta_148), color = "blue"
  )+
  labs(y = "delta Ta [K]", x = "")+
  theme_bw()

K_winter / grad_winter / u_star_winter / L_winter


K_summer / grad_summer / u_star_summer / L_summer

 
#### calculate the fluxes ####




#### plot mean diurnal cycle of z-d/L ####
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


##### explore filtering of BREB and K-theory fluxes with u* and stability, day and night time ####

load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/sonic_profile_data.RData") # load the 14 level profile data for Ta and Humidity
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/fluxes_K_theory.RData") # load K theory results
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/fluxes_BREB.RData") # load BREB results

# get mean of ensemle
ensemble_K_BREB = sonic_profile_data%>%
  filter(height == "30m")%>%
  #filter(`qc_H_[#]` != 2)%>%
  group_by(datetime)%>%
  summarise(
    u_star_mean = mean(`u*_[m+1s-1]`), 
    L_mean = mean(`L_[m]`), 
    TKE_mean = mean(`TKE_[m+2s-2]`),
    H_wm2_mean = mean(`H_[W+1m-2]`), 
    z_d_L_mean = mean(`(z-d)/L_[#]`),
    daytime = mean(`daytime_[1=daytime]`), 
    .groups = 'drop' # drop the grouping by datetime and height
  )

# add BREB and K data
ensemble_K_BREB = ensemble_K_BREB%>%
  left_join(
    BREB %>% select(H_19_40_BREB, LE_19_40_BREB, datetime, LE_Wm2_truth, H_Wm2_truth), 
    by = "datetime"
  )%>%
  left_join(
    K_theory, 
    by = "datetime"
  )

# make daytime and stability column
ensemble_K_BREB = ensemble_K_BREB%>%
  # stability
  mutate(
    stability = case_when(
      L_mean > 50  ~ "stable (L > 50)",
      L_mean < -50 ~ "unstable (L < -50)",
      TRUE          ~ "neutral (abs(L) < 50)"))%>%
  # daytime
  mutate(
    day_night = factor(daytime,
                       levels = c(0, 1),
                       labels = c("Night", "Day"))
  )

# bring data in right format for plotting
ensemble_long = ensemble_K_BREB %>%
  pivot_longer(
    cols = c(H_19_40_BREB, LE_19_40_BREB,
             H_24_55_K,   LE_24_55_K),
    
    names_to = c("flux_type", "height", "model"),
    names_pattern = "(H|LE)_(.*)_(BREB|K)",
    
    values_to = "flux_value"
  )

obs_long = ensemble_K_BREB %>%
  pivot_longer(
    cols = c(H_Wm2_Eco, LE_Wm2_Eco),
    names_to = "flux_type",
    names_pattern = "(H|LE)_Wm2_Eco",
    values_to = "flux_obs"
  ) %>%
  mutate(
    height = NA,
    model = "EC"
  )

# join both:
ensemble_long = ensemble_long %>%
  left_join(
    obs_long %>% select(datetime, flux_type, flux_obs),
    by = c("datetime", "flux_type")
  )%>%
  filter(!is.na(day_night))

# plot altogether for BREB
BREB = ensemble_long %>%
  filter(model == "BREB") %>%
  ggplot(aes(x = flux_obs, y = flux_value)) +
  geom_point(alpha = 0.6, size = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  # stability-specific regression lines
  geom_smooth(aes(color = stability), method = "lm", se = FALSE) +
  # overall regression 
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 1
  ) +
  # stability-specific equations
  stat_poly_eq(
    aes(color = stability,
        label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 5
  ) +
  #equation
  stat_poly_eq(
    aes(group = 1,
        label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    color = "black",
    size = 5, label.y = 0.05
  ) +
  coord_equal(xlim = c(-170, 750),
              ylim = c(-170, 750)) +
  labs(y = "H [Wm2]", x = "H [Wm2] Eco", title = "BREB")+
  facet_grid(day_night ~ flux_type) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = NA, color = 'black', linewidth = 0.5)
  )


# plot altogether for K
K = ensemble_long %>%
  filter(model == "K") %>%
  ggplot(aes(x = flux_obs, y = flux_value)) +
  geom_point(alpha = 0.6, size = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  # stability-specific regression lines
  geom_smooth(aes(color = stability), method = "lm", se = FALSE) +
  # overall regression 
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 1
  ) +
  # stability-specific equations
  stat_poly_eq(
    aes(color = stability,
        label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 5
  ) +
  #equation
  stat_poly_eq(
    aes(group = 1,
        label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    color = "black",
    size = 5, label.y = 0.05
  ) +
  coord_equal(xlim = c(-170, 750),
              ylim = c(-170, 750)) +
  facet_grid(day_night ~ flux_type) +
  labs(y = "H [Wm2]", x = "H [Wm2] Eco", title = "K-theory")+
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_rect(fill = NA, color = 'black', linewidth = 0.5)
  )

K + BREB


  



  