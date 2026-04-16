#BREB script

# define a function for sensible and latent heat using BREB approach
BREB = function(H2O_mmol_mol_up, 
                H2O_mmol_mol_down, 
                Ta_dgC_up, 
                Ta_dgC_down, 
                P_ground_hPa, 
                R_net_Wm2, 
                G_Wm2, 
                type = c("latent", "sensible")
){
  
  # get the type of turbulent flux to be calculated
  type = match.arg(type)
  
  # convert from mmol/mol to kg/kg
  H2O_kg_kg_down = H2O_mmol_mol_down / 1000 * 0.622
  H2O_kg_kg_up = H2O_mmol_mol_up / 1000 * 0.622
  
  # calculate e in hPa
  e_hPa_up = (P_ground_hPa * H2O_kg_kg_up) / 0.622 
  e_hPa_down  = (P_ground_hPa * H2O_kg_kg_down) / 0.622
  
  # delta e
  delta_e_hPa = e_hPa_up - e_hPa_down 
  
  # calculate delta T
  delta_Ta_dgC = Ta_dgC_up - Ta_dgC_down
  
  ######### BREB sensible and latent heat calculation #########
  Bowen_ratio = 0.667 * (delta_Ta_dgC/delta_e_hPa)
  H_Wm2_BREB = (R_net_Wm2-G_Wm2)*((Bowen_ratio)/(1+Bowen_ratio))
  LE_Wm2_BREB = (R_net_Wm2 - G_Wm2)/(1+Bowen_ratio)
  
  ######### Quality filtering based on Foken, Micrometeorology, page 65 #########
  H_Wm2_BREB  = ifelse(Bowen_ratio > -1.25 
                       & Bowen_ratio < -0.75, 
                       yes = NA, 
                       no = H_Wm2_BREB)
  LE_Wm2_BREB = ifelse(Bowen_ratio > -1.25 
                       & Bowen_ratio < -0.75, 
                       yes = NA, 
                       no = LE_Wm2_BREB)
  
  ######### additional filtering criteria based on Ohmura 1982 #########
  # calculate delta q in kg/kg
  q_up_kg_kg = 0.622*((e_hPa_up)/(P_ground_hPa-0.378*e_hPa_up)) 
  q_down_kg_kg = 0.622*((e_hPa_down)/(P_ground_hPa-0.378*e_hPa_down))
  delta_q_kg_kg = q_up_kg_kg - q_down_kg_kg
  
  # set some variables
  lambda = 2.25e6 #in Joule per kg (although in reality dependent on T!)
  c_p = 1005 #J/kg*K
  
  valid =
    # one direction: both larger 0: >
    ((-R_net_Wm2 - G_Wm2) > 0 
     & (lambda*delta_q_kg_kg + c_p*delta_Ta_dgC) > 0) | # or
    # now other direction <
    ((-R_net_Wm2 - G_Wm2) < 0 
     & (lambda*delta_q_kg_kg + c_p*delta_Ta_dgC) < 0)
  
  # apply
  H_Wm2_BREB  = if_else(valid, true  = H_Wm2_BREB, false = NA)
  LE_Wm2_BREB = if_else(valid, true = LE_Wm2_BREB, false = NA)
  
  # return sensible heat as default
  if (type == "latent") {
    return(LE_Wm2_BREB)
  } else {
    return(H_Wm2_BREB)
  }
}

##### prepare the data #####
# load data
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/sonic_profile_data.RData") # EC data output from different heights
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/slow_profile_data.RData") # load the 14 level profile data for Ta and Humidity
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/Eco_data_30m.RData") # Ecosystem data 30m

# join the Ecosystem data set for getting radiation measurements for BREB
# use H and LE from Ecosystem station 27m as "truth" for comparison 
BREB_data = slow_profile_data%>%
  left_join(Eco_data_30m%>%select(datetime, P_ground_hPa, LE_Wm2, H_Wm2, R_Net_Wm2, G_Wm2), by = "datetime")%>%
  rename(
    LE_Wm2_Eco = LE_Wm2, 
    H_Wm2_Eco = H_Wm2
  )

# select only needed vars (here all heights) for 30m 
BREB_data = BREB_data %>%
  select(Ta_19m, Ta_40m, qc_Ta_19m, qc_Ta_40m, # Ta data from two heights
         H2O_19m, H2O_40m, qc_H2O_19m, qc_H2O_40m, # H20 data from two heights
         datetime, P_ground_hPa, H_Wm2_Eco, LE_Wm2_Eco, G_Wm2, R_Net_Wm2)%>% # other Ecosystem measurements
  filter(qc_Ta_40m  != 9| qc_H2O_40m != 9 | qc_Ta_19m != 9 , qc_H2O_19m != 9) # quickly filter 


##### apply the function ####
# for H
BREB_data = BREB_data%>%
  mutate(
    H_Wm2_BREB = BREB(
      H2O_mmol_mol_up = H2O_40m, 
      H2O_mmol_mol_down = H2O_19m, 
      Ta_dgC_up = Ta_40m, 
      Ta_dgC_down = Ta_19m, 
      P_ground_hPa = P_ground_hPa, 
      R_net_Wm2 = R_Net_Wm2, 
      G_Wm2 = G_Wm2, 
      type = "sensible"
    ))

# for LE
BREB_data = BREB_data%>%
  mutate(
    LE_Wm2_BREB = BREB(
      H2O_mmol_mol_up = H2O_40m, 
      H2O_mmol_mol_down = H2O_19m, 
      Ta_dgC_up = Ta_40m, 
      Ta_dgC_down = Ta_19m, 
      P_ground_hPa = P_ground_hPa, 
      R_net_Wm2 = R_Net_Wm2, 
      G_Wm2 = G_Wm2, 
      type = "latent"
    ))

# plot H from BREB
BREB_data %>%
  ggplot(aes(x = H_Wm2_Eco, y = H_Wm2_BREB)) +
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
  ylab("H BREB [Wm2]") +
  xlab("H EC measured 30m Eco [Wm2]") +
  coord_cartesian(xlim = c(-200, 700), ylim = c(-200, 700))+
  theme_bw()

# plot LE from BREB
BREB_data %>%
  ggplot(aes(x = LE_Wm2_Eco, y = LE_Wm2_BREB)) +
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
  ylab("LE BREB [Wm2]") +
  xlab("LE EC measured 30m Eco [Wm2]") +
  coord_cartesian(xlim = c(-200, 600), ylim = c(-200, 600))+
  theme_bw()

# save the result to use later during analysis
BREB = BREB_data%>%
  select(datetime, 
         LE_Wm2_Eco, 
         H_Wm2_Eco,
         LE_Wm2_BREB, 
         H_Wm2_BREB)%>%
  rename(LE_19_40_BREB = LE_Wm2_BREB, 
         H_19_40_BREB = H_Wm2_BREB)

# save
save(x = BREB, file = "data/processed/fluxes_BREB.RData")
