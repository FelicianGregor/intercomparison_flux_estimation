#### landscape level fluxes ####

# load packages 
library(tidyverse)
library(ggpmisc)
library(patchwork)

# use here the function from the method comparison on 70m height, and aplly to 70m and 148m 
# define a function for the MBR method to estimate latent heat flux (LE) using eddy diffusivity K from eddy and profile data
MBR_LE = function(
    H2O_mmol_mol_up, 
    H2O_mmol_mol_down, 
    Ta_dgC_up, 
    Ta_dgC_down, 
    height_diff_m, 
    P_ground_hPa, 
    H_Wm2_EC_measured, 
    Rnet)
{
  
  # convert humidity from mmol/mol to kg/kg
  H2O_kg_kg_down = H2O_mmol_mol_down / 1000 * 0.622
  H2O_kg_kg_up = H2O_mmol_mol_up / 1000 * 0.622
  
  # clauclate delta q
  delta_q_kg_kg = H2O_kg_kg_up - H2O_kg_kg_down 
  
  # calculate delta T
  delta_Ta_dgC = Ta_dgC_up - Ta_dgC_down
  
  ##### prep for calculations #####
  c_p = 1004.834 # specific heat of air at constant pressure 
  
  # latent heat of vaporization is temperature dependent, use Foken 2012 Micrometeorology, page 38
  # use uppermost T as variable, although one could also use the mean (I could implement this later)
  lambda = 2500827-2360*Ta_dgC_up # in J/Kg
  
  ######### MBR approach LE calculation ###########
  LE_Wm2_MBR = (lambda*H_Wm2_EC_measured*delta_q_kg_kg) / (c_p*delta_Ta_dgC)
  #################################################
  
  ######### additional filtering criteria #########
  # absolute limits
  # use qc filter also used by Billesbach et al. 2024, but adapted 1000Wm2 used there to 800 (more plausible in temperate forest in Sweden)
  LE_Wm2_MBR <- ifelse(
    LE_Wm2_MBR > -200 & LE_Wm2_MBR < 800,
    yes = LE_Wm2_MBR,
    no = NA)
  
  # filter for minimal differences between the levels
  # calculate delta e to do the filtering: Foken Micrometeorology 2024, page 171
  # calculate e in hPa first
  e_hPa_up = (P_ground_hPa * H2O_kg_kg_up) / 0.622 
  e_hPa_down  = (P_ground_hPa * H2O_kg_kg_down) / 0.622
  delta_e_hPa = e_hPa_up-e_hPa_down
  #filter for minimal e difference
  LE_Wm2_MBR = ifelse(abs(delta_e_hPa) < 0.05, yes = NA, no = LE_Wm2_MBR) # 0.1 previously
  
  # filter Ta difference by minimal difference
  threshold = 0.05 # in K # previously 0.15
  LE_Wm2_MBR = ifelse(abs(delta_Ta_dgC) < threshold, yes = NA, no = LE_Wm2_MBR)
  
  #filtering based on u*, as suggested by Foken and Mauder 2024, page 168
  #LE_Wm2_MBR = ifelse(u_star < 0.07, yes = NA, no = LE_Wm2_MBR)
  
  # use filter as done by Billesbach et al. 2024 for MBR method
  # filter aut based on Bowen ratios
  #threshold_Bo = 0.2
  #Bo = (c_p/lambda)* (delta_Ta_dgC/delta_q_kg_kg)
  #LE_Wm2_MBR = ifelse(abs(Bo)<0.2, yes = NA, no = LE_Wm2_MBR)
  
  # use quality control by radiation data
  LE_Wm2_MBR = ifelse(abs(LE_Wm2_MBR) > (Rnet + 50), NA, LE_Wm2_MBR)
  
  
  # return sensible heat as default
  return(LE_Wm2_MBR)
}


#### apply function ####
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/sonic_profile_data.RData") # EC data output from different heights
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/slow_profile_data.RData") # load the 14 level profile data for Ta and Humidity
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/Eco_data_30m.RData") # Ecosystem data 30m

# some data prep for sonic profile data
H_sonic_70m = sonic_profile_data%>%
  filter(height == "70m" & rotation == "double" & detrending == "block")%>%
  mutate(`H_[W+1m-2]` = ifelse(`qc_H_[#]`== 0, yes = `H_[W+1m-2]`, no = NA))%>% # filter lower quality data! 
  select(datetime, `H_[W+1m-2]`, `u*_[m+1s-1]`)%>% # get the sonic H measured by EC for calculating K
  rename(H_EC_measured_sonic_70m = `H_[W+1m-2]`, 
         u_star = `u*_[m+1s-1]`)

# data prep for slow profile data
slow_profile_data = slow_profile_data%>%
  filter(qc_Ta_55m  != 9 | qc_H2O_55m != 9 | qc_Ta_85m != 9 | qc_H2O_85m != 9) # quickly filter out data with low quality


# data prep for ETC Icos Ecosystem data:
# I filtered already out gap filtered fluxes in the pre-processing script: "ecosystem_station_data.R"

# join in sonic profile dataset to slow profile 
slow_profile_data = slow_profile_data%>%
  left_join(H_sonic_70m, by = "datetime")%>%
  left_join(Eco_data_30m%>%select(datetime, P_ground_hPa, LE_Wm2, H_Wm2), by = "datetime")%>%
  rename(LE_Wm2_Eco = LE_Wm2, 
         H_Wm2_Eco = H_Wm2)

#join Ecosystem station data for the net radiation measurements
slow_profile_data = slow_profile_data%>%
  left_join(Eco_data_30m%>%select(datetime, R_Net_Wm2), by = "datetime")

# filter out unrealistic high H fluxes (spikes) with Rnet and 50Wm2 buffer
slow_profile_data = slow_profile_data%>%
  mutate(H_EC_measured_sonic_70m = ifelse(H_EC_measured_sonic_70m > (R_Net_Wm2 + 50), NA, H_EC_measured_sonic_70m))


# apply the function for LE
slow_profile_data$LE_Wm2_MBR = MBR_LE(H2O_mmol_mol_up = slow_profile_data$H2O_85m, 
                                      H2O_mmol_mol_down = slow_profile_data$H2O_55m, 
                                      Ta_dgC_up = slow_profile_data$Ta_85m, 
                                      Ta_dgC_down = slow_profile_data$Ta_55m, 
                                      height_diff_m = 20, 
                                      P_ground_hPa = slow_profile_data$P_ground_hPa, 
                                      H_Wm2_EC_measured = slow_profile_data$H_EC_measured_sonic_70m, 
                                      Rnet = slow_profile_data$R_Net_Wm2
)


# 1. MBR vs EC comparison plots ####
# plot H from MBR
H_MBR = slow_profile_data %>%
  ggplot(aes(x = H_Wm2_Eco, y = H_EC_measured_sonic_70m)) +
  geom_point(size = 0.6, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 1) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  # Add equation and R2
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 4
  ) +
  ylab(expression("H MBR system 70m ["*W~m^{-2}*"]")) +
  xlab(expression("LE EC ICOS Ecosystem station ["*W~m^{-2}*"]")) +
  coord_cartesian(xlim = c(-300, 850), ylim = c(-300, 850))+
  theme_bw()

H_MBR

# plot LE from MBR
LE_MBR = slow_profile_data %>%
  ggplot(aes(x = LE_Wm2_Eco, y = LE_Wm2_MBR)) +
  geom_point(size = 0.6, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 1) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  # Add equation and R2
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 4
  ) +
  ylab(expression("LE MBR system 70m ["*W~m^{-2}*"]")) +
  xlab(expression("LE EC ICOS Ecosystem station ["*W~m^{-2}*"]")) +
  coord_cartesian(xlim = c(-200, 600), ylim = c(-200, 600))+
  theme_bw()

H_MBR + LE_MBR


# save as pdf
ggsave(
  filename = "C:/Users/Lenovo/Downloads/MBR_70m_comparison.pdf",
  plot = H_MBR + LE_MBR,
  width = 21, height = 11, units = "cm", dpi = 300
)

#### 2. plot: timeseries and daily mean course ####

# plot the whole (kind of predicted) time series  for LE
LE_2021_70m= slow_profile_data %>%
  ggplot() +
  geom_line(aes(x = datetime,y = LE_Wm2_MBR,color = "MBR")) +
  #geom_line(aes(x = datetime,
  #             y = LE_Wm2_Eco,
  #              color = "EC")) +
  scale_color_manual(name = NULL,values = c(
    "MBR" = "darkblue",
    "EC"   = "darkgrey")) +
  scale_x_datetime(
    breaks = scales::date_breaks("1 month"),
    labels = scales::date_format("%b")
  ) +
  labs(y = expression("LE 70m [" * W~m^{-2} * "]"),x = "") +
  theme_bw() +
  theme(
    legend.position = c(0.1, 0.75), legend.background = element_rect(
      fill = scales::alpha("white", 0.7),
      color = "black"
    )
  )

# mean daily course of LE
LE_2021_70m_mean =
  slow_profile_data %>%
  group_by(hour(datetime)) %>%
  mutate(
    # calculate mean daily course 
    mean_daily_LE_EC_measured_sonic_70m =mean(LE_Wm2_MBR, na.rm = TRUE),
    upper_mean_daily_LE_EC_measured_sonic_70m =quantile(LE_Wm2_MBR, 0.75, na.rm = TRUE),
    lower_mean_daily_LE_EC_measured_sonic_70m =quantile(LE_Wm2_MBR, 0.25, na.rm = TRUE),
    #Eco data
    mean_daily_LE_Wm2_Eco =mean(LE_Wm2_Eco, na.rm = TRUE),
    upper_mean_daily_LE_Wm2_Eco = quantile(LE_Wm2_Eco, 0.75, na.rm = TRUE),
    lower_mean_daily_LE_Wm2_Eco =quantile(LE_Wm2_Eco, 0.25, na.rm = TRUE)) %>%
  #plot
  ggplot(aes(x = hour(datetime))) +
  geom_line(aes(y = mean_daily_LE_EC_measured_sonic_70m,color = "MBR 70m")) +
  geom_ribbon(
    aes(ymin = lower_mean_daily_LE_EC_measured_sonic_70m,
        ymax = upper_mean_daily_LE_EC_measured_sonic_70m,fill = "MBR 70m"),alpha = 0.1 ) +
  geom_line(aes(y = mean_daily_LE_Wm2_Eco,color = "EC 30m")) +
  geom_ribbon(aes(ymin = lower_mean_daily_LE_Wm2_Eco, ymax = upper_mean_daily_LE_Wm2_Eco, fill = "EC 30m"),alpha = 0.2 ) +
  scale_color_manual(
    values = c("MBR 70m" = "darkblue",
               "EC 30m" = "darkgrey")) +
  scale_fill_manual(
    values = c("MBR 70m" = "darkblue",
               "EC 30m" = "darkgrey")) +
  labs(
    y = expression("mean LE [" * W ~ m^{-2} * "]"),
    x = "Hour of the day",
    color = NULL,
    fill = NULL) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.8), 
        legend.background = element_rect(
          colour = "black",
          fill = "white"), legend.box.background = element_rect(
            colour = "black" ))





# do the prediction for H MBR
H_2021_70m= slow_profile_data %>%
  ggplot() +
  geom_line(aes(x = datetime,y = H_EC_measured_sonic_70m,color = "MBR")) +
  #geom_line(aes(x = datetime,
  #              y = H_Wm2_Eco,
   #             color = "EC")) +
  scale_color_manual(name = NULL,values = c(
    "MBR" = "darkred",
    "EC"   = "darkgrey")) +
  scale_x_datetime(
    breaks = scales::date_breaks("1 month"),
    labels = scales::date_format("%b")
  ) +
  labs(y = expression("H 70m [" * W~m^{-2} * "]"),x = "") +
  theme_bw() +
  theme(
    legend.position = c(0.9, 0.75), legend.background = element_rect(
      fill = scales::alpha("white", 0.7),
      color = "black"
    )
  )

# mean daily course of H
H_2021_70m_mean =
  slow_profile_data %>%
  group_by(hour(datetime)) %>%
  mutate(
    # calculate mean daily course 
    mean_daily_H_EC_measured_sonic_70m =mean(H_EC_measured_sonic_70m, na.rm = TRUE),
    upper_mean_daily_H_EC_measured_sonic_70m =quantile(H_EC_measured_sonic_70m, 0.75, na.rm = TRUE),
    lower_mean_daily_H_EC_measured_sonic_70m =quantile(H_EC_measured_sonic_70m, 0.25, na.rm = TRUE),
    #Eco data
    mean_daily_H_Wm2_Eco =mean(H_Wm2_Eco, na.rm = TRUE),
    upper_mean_daily_H_Wm2_Eco = quantile(H_Wm2_Eco, 0.75, na.rm = TRUE),
    lower_mean_daily_H_Wm2_Eco =quantile(H_Wm2_Eco, 0.25, na.rm = TRUE)) %>%
  #plot
  ggplot(aes(x = hour(datetime))) +
  geom_line(aes(y = mean_daily_H_EC_measured_sonic_70m,color = "MBR EC 70m")) +
  geom_ribbon(
    aes(ymin = lower_mean_daily_H_EC_measured_sonic_70m,
        ymax = upper_mean_daily_H_EC_measured_sonic_70m,fill = "MBR EC 70m"),alpha = 0.1 ) +
  geom_line(aes(y = mean_daily_H_Wm2_Eco,color = "EC 30m")) +
  geom_ribbon(aes(ymin = lower_mean_daily_H_Wm2_Eco, ymax = upper_mean_daily_H_Wm2_Eco, fill = "EC 30m"),alpha = 0.2 ) +
  scale_color_manual(
    values = c("MBR EC 70m" = "darkred",
               "EC 30m" = "darkgrey")) +
  scale_fill_manual(
    values = c("MBR EC 70m" = "darkred",
               "EC 30m" = "darkgrey")) +
  labs(
    y = expression("mean H [" * W ~ m^{-2} * "]"),
    x = "Hour of the day",
    color = NULL,
    fill = NULL) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.8), 
        legend.background = element_rect(
          colour = "black",
          fill = "white"), legend.box.background = element_rect(
            colour = "black" ))


# combine everything in one plot 
MBR_preds_H_LE_70m = H_2021_70m + H_2021_70m_mean +
  plot_layout(widths = c(4, 1)) + 
  LE_2021_70m + LE_2021_70m_mean +
  plot_layout(widths = c(4, 1))

# save:
# save the plot
ggsave(
  filename = "C:/Users/Lenovo/Downloads/MBR_LE_H_70m_timeseries.pdf",
  plot = MBR_preds_H_LE_70m,
  width = 30, height = 10, units = "cm",
  dpi = 300
)

# get information on the number of missing values ####
missing = slow_profile_data%>%
  mutate(missing_values_LE_MBR_Eco_together = 
           ifelse(is.na(LE_Wm2_MBR) | is.na(LE_Wm2_Eco), no = "value", yes = NA))
1-colSums(is.na(missing))/nrow(missing)

# save the result to df to use later
MBR_data_70m = slow_profile_data%>%
  select(datetime, 
         LE_Wm2_Eco, 
         H_Wm2_Eco, 
         H_EC_measured_sonic_70m, 
         LE_Wm2_MBR,
         u_star)

# save
save(x = MBR_data_70m, file = "data/processed/fluxes_MBR_70m.RData")


################################################################################
############################## 148m ############################################
################################################################################

load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/sonic_profile_data.RData") # EC data output from different heights
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/slow_profile_data.RData") # load the 14 level profile data for Ta and Humidity
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/Eco_data_30m.RData") # Ecosystem data 30m

# some data prep for sonic profile data
H_sonic_148m = sonic_profile_data%>%
  filter(height == "148m" & rotation == "double" & detrending == "block")%>%
  mutate(`H_[W+1m-2]` = ifelse(`qc_H_[#]`== 0, yes = `H_[W+1m-2]`, no = NA))%>% # filter lower quality data! 
  select(datetime, `H_[W+1m-2]`, `u*_[m+1s-1]`)%>% # get the sonic H measured by EC for calculating K
  rename(H_EC_measured_sonic_148m = `H_[W+1m-2]`, 
         u_star = `u*_[m+1s-1]`)

# data prep for slow profile data
slow_profile_data = slow_profile_data%>%
  filter(qc_Ta_55m  != 9 | qc_H2O_55m != 9 | qc_Ta_85m != 9 | qc_H2O_85m != 9) # quickly filter out data with low quality


# data prep for ETC Icos Ecosystem data:
# I filtered already out gap filtered fluxes in the pre-processing script: "ecosystem_station_data.R"

# join in sonic profile dataset to slow profile 
slow_profile_data = slow_profile_data%>%
  left_join(H_sonic_148m, by = "datetime")%>%
  left_join(Eco_data_30m%>%select(datetime, P_ground_hPa, LE_Wm2, H_Wm2), by = "datetime")%>%
  rename(LE_Wm2_Eco = LE_Wm2, 
         H_Wm2_Eco = H_Wm2)

#join Ecosystem station data for the net radiation measurements
slow_profile_data = slow_profile_data%>%
  left_join(Eco_data_30m%>%select(datetime, R_Net_Wm2), by = "datetime")

# filter out unrealistic high H fluxes (spikes) with Rnet and 50Wm2 buffer
slow_profile_data = slow_profile_data%>%
  mutate(H_EC_measured_sonic_148m = ifelse(abs(H_EC_measured_sonic_148m) > (R_Net_Wm2 + 50), NA, H_EC_measured_sonic_148m))


# apply the function for LE
slow_profile_data$LE_Wm2_MBR = MBR_LE(H2O_mmol_mol_up = slow_profile_data$H2O_148m, 
                                      H2O_mmol_mol_down = slow_profile_data$H2O_85m, 
                                      Ta_dgC_up = slow_profile_data$Ta_148m, 
                                      Ta_dgC_down = slow_profile_data$Ta_85m, 
                                      height_diff_m = 63, 
                                      P_ground_hPa = slow_profile_data$P_ground_hPa, 
                                      H_Wm2_EC_measured = slow_profile_data$H_EC_measured_sonic_148m, 
                                      Rnet = slow_profile_data$R_Net_Wm2
)


# 1. MBR vs EC comparison plots ####
# plot H from MBR
H_MBR = slow_profile_data %>%
  ggplot(aes(x = H_Wm2_Eco, y = H_EC_measured_sonic_148m)) +
  geom_point(size = 0.6, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 1) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  # Add equation and R2
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 4
  ) +
  ylab(expression("H MBR system 148m ["*W~m^{-2}*"]")) +
  xlab(expression("LE EC ICOS Ecosystem station ["*W~m^{-2}*"]")) +
  coord_cartesian(xlim = c(-300, 850), ylim = c(-300, 850))+
  theme_bw()

H_MBR

# plot LE from MBR
LE_MBR = slow_profile_data %>%
  ggplot(aes(x = LE_Wm2_Eco, y = LE_Wm2_MBR)) +
  geom_point(size = 0.6, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 1) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  # Add equation and R2
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 4
  ) +
  ylab(expression("LE MBR system 148m ["*W~m^{-2}*"]")) +
  xlab(expression("LE EC ICOS Ecosystem station ["*W~m^{-2}*"]")) +
  coord_cartesian(xlim = c(-200, 600), ylim = c(-200, 600))+
  theme_bw()

H_MBR + LE_MBR


# save as pdf
ggsave(
  filename = "C:/Users/Lenovo/Downloads/MBR_148m_comparison.pdf",
  plot = H_MBR + LE_MBR,
  width = 21, height = 11, units = "cm", dpi = 300
)

#### 2. plot: timeseries and daily mean course ####

# plot the whole (kind of predicted) time series  for LE
LE_2021_148m= slow_profile_data %>%
  ggplot() +
  #geom_line(aes(x = datetime,
  #             y = LE_Wm2_Eco,
  #            color = "EC")) +
  geom_line(aes(x = datetime,y = LE_Wm2_MBR,color = "MBR")) +
  
  scale_color_manual(name = NULL,values = c(
    "MBR" = "darkblue",
    "EC"   = "darkgrey")) +
  scale_x_datetime(
    breaks = scales::date_breaks("1 month"),
    labels = scales::date_format("%b")
  ) +
  labs(y = expression("LE 148m [" * W~m^{-2} * "]"),x = "") +
  theme_bw() +
  theme(
    legend.position = c(0.1, 0.75), legend.background = element_rect(
      fill = scales::alpha("white", 0.7),
      color = "black"
    )
  )

# mean daily course of LE
LE_2021_148m_mean =
  slow_profile_data %>%
  group_by(hour(datetime)) %>%
  mutate(
    # calculate mean daily course 
    mean_daily_LE_EC_measured_sonic_148m =mean(LE_Wm2_MBR, na.rm = TRUE),
    upper_mean_daily_LE_EC_measured_sonic_148m =quantile(LE_Wm2_MBR, 0.75, na.rm = TRUE),
    lower_mean_daily_LE_EC_measured_sonic_148m =quantile(LE_Wm2_MBR, 0.25, na.rm = TRUE),
    #Eco data
    mean_daily_LE_Wm2_Eco =mean(LE_Wm2_Eco, na.rm = TRUE),
    upper_mean_daily_LE_Wm2_Eco = quantile(LE_Wm2_Eco, 0.75, na.rm = TRUE),
    lower_mean_daily_LE_Wm2_Eco =quantile(LE_Wm2_Eco, 0.25, na.rm = TRUE)) %>%
  #plot
  ggplot(aes(x = hour(datetime))) +
  geom_line(aes(y = mean_daily_LE_EC_measured_sonic_148m,color = "MBR 148m")) +
  geom_ribbon(
    aes(ymin = lower_mean_daily_LE_EC_measured_sonic_148m,
        ymax = upper_mean_daily_LE_EC_measured_sonic_148m,fill = "MBR 148m"),alpha = 0.1 ) +
  geom_line(aes(y = mean_daily_LE_Wm2_Eco,color = "EC 30m")) +
  geom_ribbon(aes(ymin = lower_mean_daily_LE_Wm2_Eco, ymax = upper_mean_daily_LE_Wm2_Eco, fill = "EC 30m"),alpha = 0.2 ) +
  scale_color_manual(
    values = c("MBR 148m" = "darkblue",
               "EC 30m" = "darkgrey")) +
  scale_fill_manual(
    values = c("MBR 148m" = "darkblue",
               "EC 30m" = "darkgrey")) +
  labs(
    y = expression("mean LE [" * W ~ m^{-2} * "]"),
    x = "Hour of the day",
    color = NULL,
    fill = NULL) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.8), 
        legend.background = element_rect(
          colour = "black",
          fill = "white"), legend.box.background = element_rect(
            colour = "black" ))


# do the prediction for H MBR
H_2021_148m= slow_profile_data %>%
  ggplot() +
  geom_line(aes(x = datetime,y = H_EC_measured_sonic_148m,color = "MBR")) +
  #geom_line(aes(x = datetime,
  #              y = H_Wm2_Eco,
  #              color = "EC")) +
  scale_color_manual(name = NULL,values = c(
    "MBR" = "darkred",
    "EC"   = "darkgrey")) +
  scale_x_datetime(
    breaks = scales::date_breaks("1 month"),
    labels = scales::date_format("%b")
  ) +
  labs(y = expression("H 148m [" * W~m^{-2} * "]"),x = "") +
  theme_bw() +
  theme(
    legend.position = c(0.9, 0.75), legend.background = element_rect(
      fill = scales::alpha("white", 0.7),
      color = "black"
    )
  )

H_2021_148m_mean =
  slow_profile_data %>%
  group_by(hour(datetime)) %>%
  mutate(
    # calculate mean daily course 
    mean_daily_H_EC_measured_sonic_148m =mean(H_EC_measured_sonic_148m, na.rm = TRUE),
    upper_mean_daily_H_EC_measured_sonic_148m =quantile(H_EC_measured_sonic_148m, 0.75, na.rm = TRUE),
    lower_mean_daily_H_EC_measured_sonic_148m =quantile(H_EC_measured_sonic_148m, 0.25, na.rm = TRUE),
    #Eco data
    mean_daily_H_Wm2_Eco =mean(H_Wm2_Eco, na.rm = TRUE),
    upper_mean_daily_H_Wm2_Eco = quantile(H_Wm2_Eco, 0.75, na.rm = TRUE),
    lower_mean_daily_H_Wm2_Eco =quantile(H_Wm2_Eco, 0.25, na.rm = TRUE)) %>%
  #plot
  ggplot(aes(x = hour(datetime))) +
  geom_line(aes(y = mean_daily_H_EC_measured_sonic_148m,color = "MBR EC 148m")) +
  geom_ribbon(
    aes(ymin = lower_mean_daily_H_EC_measured_sonic_148m,
        ymax = upper_mean_daily_H_EC_measured_sonic_148m,fill = "MBR EC 148m"),alpha = 0.1 ) +
  geom_line(aes(y = mean_daily_H_Wm2_Eco,color = "EC 30m")) +
  geom_ribbon(aes(ymin = lower_mean_daily_H_Wm2_Eco, ymax = upper_mean_daily_H_Wm2_Eco, fill = "EC 30m"),alpha = 0.2 ) +
  scale_color_manual(
    values = c("MBR EC 148m" = "darkred",
               "EC 30m" = "darkgrey")) +
  scale_fill_manual(
    values = c("MBR EC 148m" = "darkred",
               "EC 30m" = "darkgrey")) +
  labs(
    y = expression("mean H [" * W ~ m^{-2} * "]"),
    x = "Hour of the day",
    color = NULL,
    fill = NULL) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.8), 
        legend.background = element_rect(
          colour = "black",
          fill = "white"), legend.box.background = element_rect(
            colour = "black" ))



# combine everything in one plot 
MBR_preds_H_LE_148m = H_2021_148m + H_2021_148m_mean +
  plot_layout(widths = c(4, 1)) + 
  LE_2021_148m + LE_2021_148m_mean +
  plot_layout(widths = c(4, 1))

# save:
# save the plot
ggsave(
  filename = "C:/Users/Lenovo/Downloads/MBR_LE_H_148m_timeseries.pdf",
  plot = MBR_preds_H_LE_148m,
  width = 30, height = 10, units = "cm",
  dpi = 300
)

# get information on the number of missing values ####
missing = slow_profile_data%>%
  mutate(missing_values_LE_MBR_Eco_together = 
           ifelse(is.na(LE_Wm2_MBR) | is.na(LE_Wm2_Eco), no = "value", yes = NA))
1-colSums(is.na(missing))/nrow(missing)

# save the result to df to use later
MBR_data_148m = slow_profile_data%>%
  select(datetime, 
         LE_Wm2_Eco, 
         H_Wm2_Eco, 
         H_EC_measured_sonic_148m , 
         LE_Wm2_MBR,
         u_star)

# save
save(x = MBR_data_148m, file = "data/processed/fluxes_MBR_148m.RData")



