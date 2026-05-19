### Modified Bowen-ratio (MBR) approach ####

# load packages
library(tidyverse)
library(ggpmisc) # for adding equations to plots
library(patchwork) # for combining plots

# define a function for the MBR method to estimate latent heat flux (LE) using eddy diffusivity K from eddy and profile data
MBR_LE = function(
                H2O_mmol_mol_up, 
                H2O_mmol_mol_down, 
                Ta_dgC_up, 
                Ta_dgC_down, 
                height_diff_m, 
                P_ground_hPa, 
                H_Wm2_EC_measured)
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
  LE_Wm2_MBR = ifelse(abs(delta_e_hPa) < 0.1, yes = NA, no = LE_Wm2_MBR)
  
  # filter Ta difference by minimal difference
  threshold = 0.15 # in K
  LE_Wm2_MBR = ifelse(abs(delta_Ta_dgC) < threshold, yes = NA, no = LE_Wm2_MBR)
  
  #filtering based on u*, as suggested by Foken and Mauder 2024, page 168
  #LE_Wm2_MBR = ifelse(u_star < 0.07, yes = NA, no = LE_Wm2_MBR)
  
  # use filter as done by Billesbach et al. 2024 for MBR method
  # filter aut based on Bowen ratios
  #threshold_Bo = 0.2
  #Bo = (c_p/lambda)* (delta_Ta_dgC/delta_q_kg_kg)
  #LE_Wm2_MBR = ifelse(abs(Bo)<0.2, yes = NA, no = LE_Wm2_MBR)
  
  
  # return sensible heat as default
  return(LE_Wm2_MBR)
}


#### apply function for the first time ####
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/sonic_profile_data.RData") # EC data output from different heights
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/slow_profile_data.RData") # load the 14 level profile data for Ta and Humidity
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/Eco_data_30m.RData") # Ecosystem data 30m

# some data prep for sonic profile data
H_sonic_30m = sonic_profile_data%>%
  filter(height == "30m" & rotation == "double" & detrending == "block")%>%
  mutate(`H_[W+1m-2]` = ifelse(`qc_H_[#]`== 0, yes = `H_[W+1m-2]`, no = NA))%>% # filter lower quality data! 
  select(datetime, `H_[W+1m-2]`, `u*_[m+1s-1]`)%>% # get the sonic H measured by EC for calculating K
  rename(H_EC_measured_sonic_30m = `H_[W+1m-2]`, 
         u_star = `u*_[m+1s-1]`)

# data prep for slow profile data
slow_profile_data = slow_profile_data%>%
  filter(qc_Ta_40m  != 9 | qc_H2O_40m != 9 | qc_Ta_19m != 9 | qc_H2O_19m != 9) # quickly filter out data with low quality


# data prep for ETC Icos Ecosystem data:
# I filtered already out gap filtered fluxes in the pre-processing script: "ecosystem_station_data.R"
  
# join in sonic profile dataset to slow profile 
slow_profile_data = slow_profile_data%>%
  left_join(H_sonic_30m, by = "datetime")%>%
  left_join(Eco_data_30m%>%select(datetime, P_ground_hPa, LE_Wm2, H_Wm2), by = "datetime")%>%
  rename(LE_Wm2_Eco = LE_Wm2, 
         H_Wm2_Eco = H_Wm2)

#join Ecosystem station data for the net radiation measurements
slow_profile_data = slow_profile_data%>%
  left_join(Eco_data_30m%>%select(datetime, R_Net_Wm2), by = "datetime")

# apply the function for LE
slow_profile_data$LE_Wm2_MBR = MBR_LE(H2O_mmol_mol_up = slow_profile_data$H2O_40m, 
                    H2O_mmol_mol_down = slow_profile_data$H2O_19m, 
                    Ta_dgC_up = slow_profile_data$Ta_40m, 
                    Ta_dgC_down = slow_profile_data$Ta_19m, 
                    height_diff_m = 21, 
                    P_ground_hPa = slow_profile_data$P_ground_hPa, 
                    H_Wm2_EC_measured = slow_profile_data$H_EC_measured_sonic_30m
                    )

# plot and compare
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
  ylab(expression("LE MBR system ["*W~m^{-2}*"]")) + 
  xlab(expression("LE EC ICOS Ecosystem station ["*W~m^{-2}*"]")) +
  coord_cartesian(xlim = c(-150, 500), ylim = c(-150, 500))+
  theme_bw()

LE_MBR 

# just to double check, but calculating H this way does not make sense, results are exactly similar to measured ones
H_MBR = slow_profile_data %>%
  ggplot(aes(x = H_Wm2_Eco, y = H_EC_measured_sonic_30m)) +
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
  ylab(expression("H MBR system [" *W~m^{-2}*"]")) +
  xlab(expression("H EC ICOS Ecosystem station ["*W~m^{-2}*"]")) +
  coord_cartesian(xlim = c(-200, 600), ylim = c(-200, 600))+
  theme_bw()



row_count = slow_profile_data%>%
  select(datetime, LE_Wm2_MBR, LE_Wm2_Eco)%>%
  mutate(LE_Wm2_MBR = ifelse(is.na(LE_Wm2_Eco), yes = NA, no = LE_Wm2_MBR ))%>%
  drop_na()

nrow(row_count)
  

# plot the whole (kind of predicted) time series 
slow_profile_data%>%
  select(datetime, LE_Wm2_MBR, LE_Wm2_Eco)%>%
  mutate(LE_Wm2_MBR = ifelse(is.na(LE_Wm2_Eco), yes = NA, no = LE_Wm2_MBR ))%>%
  ggplot()+
  geom_line(aes(x = datetime, y = LE_Wm2_MBR), col = "blue")+
  geom_line(aes(x = datetime, y = LE_Wm2_Eco), col = "lightblue")+
  labs(y = expression("LE MBR system ["*W~m^{-2}*"]"), x = "")+
  theme_bw()

# mean daily course of LE
slow_profile_data%>%
  select(datetime, LE_Wm2_MBR, LE_Wm2_Eco)%>%
  drop_na()%>%
  group_by(hour(datetime))%>%
  # calculate mean daily values and percentiles
  mutate(mean_daily_LE_Wm2_MBR = mean(LE_Wm2_MBR, na.rm = T), 
         upper_mean_daily_LE_Wm2_MBR = quantile(LE_Wm2_MBR, probs = c(0.75), na.rm = T), 
         lower_mean_daily_LE_Wm2_MBR = quantile(LE_Wm2_MBR, probs = c(0.25), na.rm = T), 
         # for reference Eco data
         mean_daily_LE_Wm2_Eco = mean(LE_Wm2_Eco, na.rm = T),
         upper_mean_daily_LE_Wm2_Eco = quantile(LE_Wm2_Eco, probs = c(0.75), na.rm = T), 
         lower_mean_daily_LE_Wm2_Eco = quantile(LE_Wm2_Eco, probs = c(0.25), na.rm = T)
         )%>%
  ggplot(aes(x = hour(datetime)))+
  geom_line(aes(y = mean_daily_LE_Wm2_MBR), color = "darkred")+
  geom_ribbon(aes(ymin=lower_mean_daily_LE_Wm2_MBR, 
                  ymax=upper_mean_daily_LE_Wm2_MBR), alpha=0.05, fill = "darkred", 
              color = "darkred", linetype = "dotted")+
  geom_line(aes(y = mean_daily_LE_Wm2_Eco), color = "darkgreen")+
  geom_ribbon(aes(ymin=lower_mean_daily_LE_Wm2_Eco, 
                  ymax=upper_mean_daily_LE_Wm2_Eco), alpha=0.05, fill = "darkgreen", 
              color = "darkgreen", linetype = "dotted")+
  labs(y = expression("LE MBR system ["*W~m^{-2}*"]"), x = "Hour of the day")+
  theme_bw()
  

# get information on the number of missing values
missing = slow_profile_data%>%
  mutate(missing_values_LE_MBR_Eco_together = 
           ifelse(is.na(LE_Wm2_MBR) | is.na(LE_Wm2_Eco), no = "value", yes = NA))
1-colSums(is.na(missing))/nrow(missing)

H_MBR + LE_MBR


# save as png
ggsave(
  filename = "C:/Users/Lenovo/Downloads/MBR_result_overall.png",
  plot = H_MBR + LE_MBR,
  width = 21, height = 11, units = "cm", dpi = 300
)

# save the result to use later
MBR_data = slow_profile_data%>%
  select(datetime, 
         LE_Wm2_Eco, 
         H_Wm2_Eco, 
         H_EC_measured_sonic_30m, 
         LE_Wm2_MBR,
         u_star)

# save
save(x = MBR_data, file = "data/processed/fluxes_MBR.RData")
  
#ggplotly(plot)


