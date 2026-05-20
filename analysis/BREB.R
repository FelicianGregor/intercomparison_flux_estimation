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
  
  # calculate delta q
  delta_q_kg_kg = H2O_kg_kg_up - H2O_kg_kg_down
  
  # calculate delta T
  delta_Ta_dgC = Ta_dgC_up - Ta_dgC_down
  
  # latent heat of vaporization is temperature dependent, use Foken 2012 Micrometeorology, page 38
  # use uppermost T as variable, although one could also use the mean (I could implement this later)
  lambda = 2500827-2360*Ta_dgC_up # in J/Kg
  
  # specific heat capacity of air
  c_p = 1004.834 # specific heat of air at constant pressure 
  
  ######### BREB sensible and latent heat calculation #########
  Bowen_ratio = (c_p/lambda)* (delta_Ta_dgC/delta_q_kg_kg)
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
  
  # filter for physically plausable values
  # use qc filter also used by Billesbach et al. 2024, but adapted 1000Wm2 used there to 800 (more plausible in temperate forest in Sweden)
  LE_Wm2_BREB <- ifelse(
    LE_Wm2_BREB > -200 & LE_Wm2_BREB < 800,
    yes = LE_Wm2_BREB,
    no = NA)
  
  # return sensible heat as default
  if (type == "latent") {
    return(LE_Wm2_BREB)
  } else {
    return(H_Wm2_BREB)
  }
}

##### prepare the data #####
library(tidyverse)
library(ggpmisc)
library(lubridate)

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
  filter(qc_Ta_40m  != 9| qc_H2O_40m != 9 | qc_Ta_19m != 9 , qc_H2O_19m != 9) # quickly filter out data with low quality


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

####do the plots ####

# 1. BREB vs EC comparison plots ####
# plot H from BREB
H_BREB = BREB_data %>%
  ggplot(aes(x = H_Wm2_Eco, y = H_Wm2_BREB)) +
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
  ylab(expression("H BREB system ["*W~m^{-2}*"]")) +
  xlab(expression("LE EC ICOS Ecosystem station ["*W~m^{-2}*"]")) +
  coord_cartesian(xlim = c(-300, 850), ylim = c(-300, 850))+
  theme_bw()

H_BREB

# plot LE from BREB
LE_BREB = BREB_data %>%
  ggplot(aes(x = LE_Wm2_Eco, y = LE_Wm2_BREB)) +
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
  ylab(expression("LE BREB system ["*W~m^{-2}*"]")) +
  xlab(expression("LE EC ICOS Ecosystem station ["*W~m^{-2}*"]")) +
  coord_cartesian(xlim = c(-200, 600), ylim = c(-200, 600))+
  theme_bw()

H_BREB + LE_BREB

# save as pdf
ggsave(
  filename = "C:/Users/Lenovo/Downloads/BREB_result_overall.pdf",
  plot = H_BREB + LE_BREB,
  width = 21, height = 11, units = "cm",
  dpi = 300
)

#### 2. plot: timeseries and daily mean course ####

# plot the whole (kind of predicted) time series  for LE
LE_2021= BREB_data %>%
  ggplot() +
  geom_line(aes(x = datetime,y = LE_Wm2_BREB,color = "BREB")) +
  geom_line(aes(x = datetime,
                y = LE_Wm2_Eco,
                color = "EC")) +
  scale_color_manual(name = NULL,values = c(
    "BREB" = "darkblue",
    "EC"   = "darkgrey")) +
  scale_x_datetime(
    breaks = scales::date_breaks("1 month"),
    labels = scales::date_format("%b")
  ) +
  labs(y = expression("LE [" * W~m^{-2} * "]"),x = "") +
  theme_bw() +
  theme(
    legend.position = c(0.1, 0.75), legend.background = element_rect(
      fill = scales::alpha("white", 0.7),
      color = "black"
    )
  )

# mean daily course of LE
LE_2021_mean = BREB_data%>%
  group_by(hour(datetime))%>%
  # calculate mean daily values and percentiles
  mutate(mean_daily_LE_Wm2_BREB = mean(LE_Wm2_BREB, na.rm = T), 
         upper_mean_daily_LE_Wm2_BREB = quantile(LE_Wm2_BREB, probs = c(0.75), na.rm = T), 
         lower_mean_daily_LE_Wm2_BREB = quantile(LE_Wm2_BREB, probs = c(0.25), na.rm = T), 
         # for reference Eco data
         mean_daily_LE_Wm2_Eco = mean(LE_Wm2_Eco, na.rm = T),
         upper_mean_daily_LE_Wm2_Eco = quantile(LE_Wm2_Eco, probs = c(0.75), na.rm = T), 
         lower_mean_daily_LE_Wm2_Eco = quantile(LE_Wm2_Eco, probs = c(0.25), na.rm = T)
  )%>%
  ggplot(aes(x = hour(datetime)))+
  geom_line(aes(y = mean_daily_LE_Wm2_BREB), color = "darkblue")+
  geom_ribbon(aes(ymin=lower_mean_daily_LE_Wm2_BREB, 
                  ymax=upper_mean_daily_LE_Wm2_BREB), alpha=0.1, fill = "darkblue", 
              color = "darkblue", linetype = "dotted")+
  geom_line(aes(y = mean_daily_LE_Wm2_Eco), color = "darkgrey")+
  geom_ribbon(aes(ymin=lower_mean_daily_LE_Wm2_Eco, 
                  ymax=upper_mean_daily_LE_Wm2_Eco), alpha=0.2, fill = "darkgrey", 
              color = "darkgrey", linetype = "dotted")+
  labs(y = expression("LE ["*W~m^{-2}*"]"), x = "Hour of the day")+
  theme_bw()

# do the prediction for H BREB
H_2021= BREB_data %>%
  ggplot() +
  geom_line(aes(x = datetime,y = H_Wm2_BREB,color = "BREB")) +
  geom_line(aes(x = datetime,
        y = H_Wm2_Eco,
        color = "EC")) +
  scale_color_manual(name = NULL,values = c(
      "BREB" = "darkred",
      "EC"   = "darkgrey")) +
  scale_x_datetime(
    breaks = scales::date_breaks("1 month"),
    labels = scales::date_format("%b")
  ) +
  labs(y = expression("H [" * W~m^{-2} * "]"),x = "") +
  theme_bw() +
  theme(
    legend.position = c(0.9, 0.75), legend.background = element_rect(
      fill = scales::alpha("white", 0.7),
      color = "black"
    )
  )

# mean daily course of H
H_2021_mean = BREB_data%>%
  group_by(hour(datetime))%>%
  # calculate mean daily values and percentiles
  mutate(mean_daily_H_Wm2_BREB = mean(H_Wm2_BREB, na.rm = T), 
         upper_mean_daily_H_Wm2_BREB = quantile(H_Wm2_BREB, probs = c(0.75), na.rm = T), 
         lower_mean_daily_H_Wm2_BREB = quantile(H_Wm2_BREB, probs = c(0.25), na.rm = T), 
         # for reference Eco data
         mean_daily_H_Wm2_Eco = mean(H_Wm2_Eco, na.rm = T),
         upper_mean_daily_H_Wm2_Eco = quantile(H_Wm2_Eco, probs = c(0.75), na.rm = T), 
         lower_mean_daily_H_Wm2_Eco = quantile(H_Wm2_Eco, probs = c(0.25), na.rm = T)
  )%>%
  ggplot(aes(x = hour(datetime)))+
  geom_line(aes(y = mean_daily_H_Wm2_BREB), color = "darkred")+
  geom_ribbon(aes(ymin=lower_mean_daily_H_Wm2_BREB, 
                  ymax=upper_mean_daily_H_Wm2_BREB), alpha=0.1, fill = "darkred", 
              color = "darkred", linetype = "dotted")+
  geom_line(aes(y = mean_daily_H_Wm2_Eco), color = "darkgrey")+
  geom_ribbon(aes(ymin=lower_mean_daily_H_Wm2_Eco, 
                  ymax=upper_mean_daily_H_Wm2_Eco), alpha=0.2, fill = "darkgrey", 
              color = "darkgrey", linetype = "dotted")+
  labs(y = expression("H ["*W~m^{-2}*"]"), x = "Hour of the day")+
  theme_bw()


# combine everything in one plot 
BREB_preds_H_LE = H_2021 + H_2021_mean +
  plot_layout(widths = c(4, 1)) + 
  LE_2021 + LE_2021_mean +
  plot_layout(widths = c(4, 1))

# save the plot
ggsave(
  filename = "C:/Users/Lenovo/Downloads/BREB_LE_H_timeseries.pdf",
  plot = BREB_preds_H_LE,
  width = 30, height = 10, units = "cm",
  dpi = 300
)

# get information on the number of missing values ####
missing = BREB_data%>%
  mutate(missing_values_LE_BREB_Eco_together = 
           ifelse(is.na(LE_Wm2_BREB) | is.na(LE_Wm2_Eco), no = "value", yes = NA), 
         missing_values_H_BREB_Eco_together = 
           ifelse(is.na(H_Wm2_BREB) | is.na(H_Wm2_Eco), no = "value", yes = NA))
1-colSums(is.na(missing))/nrow(missing)


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

