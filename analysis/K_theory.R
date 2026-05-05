### K-Theory approach ####

# load packages
library(tidyverse)
library(ggpmisc) # for adding equations to plots

# define a function for the K-theory to estimate fluxes using the K from eddy and profile data
# define a function fro sensible and latent heat using BREB approach
K_theory = function(
                H2O_mmol_mol_up, 
                H2O_mmol_mol_down, 
                Ta_dgC_up, 
                Ta_dgC_down, 
                height_diff_m, 
                P_ground_hPa, 
                H_Wm2_EC_measured,
                u_star, 
                R_Net_Wm2, 
                type = c("latent", "sensible")
){
  
  # get the type of turbulent flux to be calculated
  type = match.arg(type)
  
  # convert from mmol/mol to kg/kg
  H2O_kg_kg_down = H2O_mmol_mol_down / 1000 * 0.622
  H2O_kg_kg_up = H2O_mmol_mol_up / 1000 * 0.622
  delta_q_kg_kg = H2O_kg_kg_up - H2O_kg_kg_down 
  
  # calculate delta e to do the filtering: Foken Micrometeorology 2024, page 171
  # calculate e in hPa first
  e_hPa_up = (P_ground_hPa * H2O_kg_kg_up) / 0.622 
  e_hPa_down  = (P_ground_hPa * H2O_kg_kg_down) / 0.622
  delta_e_hPa = e_hPa_up-e_hPa_down
  
  hist(delta_e_hPa, xlim = c(-2, 1.75))

  #filter
  delta_q_kg_kg = ifelse(abs(delta_e_hPa)<0.2, yes = NA, no = delta_q_kg_kg)
  
  # calculate delta T
  delta_Ta_dgC = Ta_dgC_up - Ta_dgC_down
  # filter Ta difference by minimal difference
  threshold = 0.2 # in K
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
  
  ######### K-approach sensible and latent heat calculation #########
  #latent heat:
  K_H = - H_Wm2_EC_measured / (rho * c_p * (delta_Ta_dgC / height_diff_m))
  H_Wm2_K_theory = - c_p * rho* K_H * (delta_Ta_dgC/height_diff_m)
  LE_Wm2_K_theory = - lambda * rho * K_H * (delta_q_kg_kg/height_diff_m)
  
  
  ######### additional filtering criteria?? #########
  #filtering based on u*
  LE_Wm2_K_theory = ifelse(u_star < 0.15, yes = NA, no = LE_Wm2_K_theory)
  H_Wm2_K_theory = ifelse(u_star < 0.15, yes = NA, no = H_Wm2_K_theory)
  
  
  # return sensible heat as default
  if (type == "latent") {
    return(LE_Wm2_K_theory)
  } else {
    return(H_Wm2_K_theory)
  }
}


#### apply function for the first time ####
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/sonic_profile_data.RData") # EC data output from different heights
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/slow_profile_data.RData") # load the 14 level profile data for Ta and Humidity
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/Eco_data_30m.RData") # Ecosystem data 30m

# some data prep
H_sonic_30m = sonic_profile_data%>%
  filter(height == "30m" & rotation == "double" & detrending == "block")%>%
  select(datetime, `H_[W+1m-2]`, `u*_[m+1s-1]`)%>% # get the sonic H measured by EC for calculating K
  rename(H_EC_measured_sonic_30m = `H_[W+1m-2]`, 
         u_star = `u*_[m+1s-1]`)
  
# join in profile dataset
slow_profile_data = slow_profile_data%>%
  left_join(H_sonic_30m, by = "datetime")%>%
  left_join(Eco_data_30m%>%select(datetime, P_ground_hPa, LE_Wm2, H_Wm2), by = "datetime")%>%
  rename(LE_Wm2_Eco = LE_Wm2, 
         H_Wm2_Eco = H_Wm2)

#join Ecosystem station data for the net radiation measurements
slow_profile_data = slow_profile_data%>%
  left_join(Eco_data_30m%>%select(datetime, R_Net_Wm2), by = "datetime")

# apply the function for LE
slow_profile_data$LE_Wm2_K_theory = K_theory(H2O_mmol_mol_up = slow_profile_data$H2O_40m, 
                    H2O_mmol_mol_down = slow_profile_data$H2O_19m, 
                    Ta_dgC_up = slow_profile_data$Ta_40m, 
                    Ta_dgC_down = slow_profile_data$Ta_19m, 
                    height_diff_m = 21, 
                    P_ground_hPa = slow_profile_data$P_ground_hPa, 
                    H_Wm2_EC_measured = slow_profile_data$H_EC_measured_sonic_30m,
                    u_star = slow_profile_data$u_star,
                    R_Net_Wm2 = slow_profile_data$R_Net_Wm2, 
                    type = "latent")

# plot and compare
slow_profile_data %>%
  #filter(LE_Wm2_K_theory > -700 & LE_Wm2_K_theory < 700) %>%
  ggplot(aes(x = LE_Wm2_Eco, y = LE_Wm2_K_theory)) +
  geom_point(size = 0.6, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 1) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  # Add equation and R2
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 3
  ) +
  ylab("LE K-theory [Wm2]") +
  xlab("LE EC measured 30m Eco [Wm2]") +
  coord_cartesian(xlim = c(-100, 500), ylim = c(-100, 500))+
  theme_bw()
  

# just to double check, but calculating H this way does not make sense, results are exactly similar to measured ones
slow_profile_data$H_Wm2_K_theory = K_theory(
  H2O_mmol_mol_up = slow_profile_data$H2O_40m, 
  H2O_mmol_mol_down = slow_profile_data$H2O_19m, 
  Ta_dgC_up = slow_profile_data$Ta_40m, 
  Ta_dgC_down = slow_profile_data$Ta_19m, 
  height_diff_m = 21, 
  P_ground_hPa = slow_profile_data$P_ground_hPa, 
  H_Wm2_EC_measured = slow_profile_data$H_EC_measured_sonic_30m,
  u_star = slow_profile_data$u_star,
  R_Net_Wm2 = slow_profile_data$R_Net_Wm2, 
  type = "sensible")

H = slow_profile_data %>%
  #filter(LE_Wm2_K_theory > -700 & LE_Wm2_K_theory < 700) %>%
  ggplot(aes(x = H_Wm2_Eco, y = H_Wm2_K_theory)) +
  geom_point(size = 0.6, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 1) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  # Add equation and R2
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 3
  ) +
  ylab("H K-theory [Wm2]") +
  xlab("H EC measured 30m Eco [Wm2]") +
  coord_cartesian(xlim = c(-100, 600), ylim = c(-100, 600))+
  theme_bw()

H
    


# save the result to use later
K_theory = slow_profile_data%>%
  select(datetime, 
         LE_Wm2_Eco, 
         H_Wm2_Eco, 
         H_EC_measured_sonic_30m, 
         u_star, 
         H_Wm2_K_theory, 
         LE_Wm2_K_theory)%>%
  rename(LE_19_40_K = LE_Wm2_K_theory, 
         H_19_40_K = H_Wm2_K_theory)

# save
save(x = K_theory, file = "data/processed/fluxes_K_theory.RData")
  
#ggplotly(plot)
