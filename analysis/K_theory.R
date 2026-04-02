
### K-Theory approach ####

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
  
  # obtain specific humidity q from e (following Foken Micrometeorology 2012, page 41)
  q_kg_kg_up = 0.622 * (e_hPa_up/(P_ground_hPa-0.378*e_hPa_up))
  q_kg_kg_down = 0.622 * (e_hPa_down/(P_ground_hPa-0.378*e_hPa_down))
  
  # delta e
  delta_q_kg_kg =q_kg_kg_up - q_kg_kg_down 
  
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
  
  ######### K-approach sensible and latent heat calculation #########
  #latent heat:
  K_H = K_H = - H_Wm2_EC_measured / (rho * c_p * (delta_Ta_dgC / height_diff_m))
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
  
# join in profle dataset
slow_profile_data = slow_profile_data%>%
  left_join(H_sonic_30m, by = "datetime")%>%
  left_join(Eco_data_30m%>%select(datetime, P_ground_hPa, LE_Wm2), by = "datetime")%>%
  rename(LE_Wm2_Eco = LE_Wm2)

# apply the function for LE
slow_profile_data$LE_Wm2_K_theory = K_theory(H2O_mmol_mol_up = slow_profile_data$H2O_55m, 
                    H2O_mmol_mol_down = slow_profile_data$H2O_30m, 
                    Ta_dgC_up = slow_profile_data$Ta_55m, 
                    Ta_dgC_down = slow_profile_data$Ta_30m, 
                    height_diff_m = 25, 
                    P_ground_hPa = slow_profile_data$P_ground_hPa, 
                    H_Wm2_EC_measured = slow_profile_data$H_EC_measured_sonic_30m,
                    u_star = slow_profile_data$u_star,
                    type = "latent"
)

# plot and compare
slow_profile_data %>%
  #filter(LE_Wm2_K_theory > -700 & LE_Wm2_K_theory < 700) %>%
  ggplot(aes(x = LE_Wm2_Eco, y = LE_Wm2_K_theory)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 2) +
  geom_smooth(method = "lm", color = "red") +
  ylab("LE K-theory [Wm2]") +
  xlab("LE EC measured 30m Eco [Wm2]") +
  theme_bw()
  

# just to double check, but calculating H this way does not make sense, results are exactly similar to measured ones
slow_profile_data$H_Wm2_K_theory = K_theory(
  H2O_mmol_mol_up = slow_profile_data$H2O_55m, 
  H2O_mmol_mol_down = slow_profile_data$H2O_19m, 
  Ta_dgC_up = slow_profile_data$Ta_55m, 
  Ta_dgC_down = slow_profile_data$Ta_19m, 
  height_diff_m = 26, 
  P_ground_hPa = slow_profile_data$P_ground_hPa, 
  H_Wm2_EC_measured = slow_profile_data$H_EC_measured_sonic_30m,
  u_star = slow_profile_data$u_star,
  type = "sensible"
)

# plot and compare
slow_profile_data %>%
  #filter(LE_Wm2_K_theory > -700 & LE_Wm2_K_theory < 700) %>%
  ggplot(aes(x = H_EC_measured_sonic_30m, y = H_Wm2_K_theory)) +
  geom_point(size = 3, color = "green") +
  geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 2) +
  geom_smooth(method = "lm", color = "red") +
  ylab("H K-theory [Wm2]") +
  xlab("H EC measured 30m Eco [Wm2]") +
  theme_bw()
    


#### some data prep #####
# merge 
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/sonic_profile_data.RData") # EC data output from different heights
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/slow_profile_data.RData") # load the 14 level profile data for Ta and Humidity
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/Eco_data_30m.RData") # Ecosystem data 30m

# some data prep
H_sonic_30m = sonic_profile_data%>%
  filter(height == "30m" & rotation == "double" & detrending == "linear")%>%
  select(datetime, `H_[W+1m-2]`, `u*_[m+1s-1]`)%>% # get the sonic H measured by EC for calculating K
  rename(H_EC_measured_sonic_30m = `H_[W+1m-2]`, 
         u_star = `u*_[m+1s-1]`)

# join in profle dataset
K_data_tuning = slow_profile_data%>%
  left_join(H_sonic_30m, by = "datetime")%>%
  left_join(Eco_data_30m%>%select(datetime, P_ground_hPa, LE_Wm2, H_Wm2), by = "datetime")%>%
  rename(
    LE_Wm2_Eco = LE_Wm2, 
    H_Wm2_Eco = H_Wm2
  )

# select only needed vars
K_data_tuning = K_data_tuning %>%
  select(-contains('qc')) %>%
  select(matches("^(Ta|H2O)_[0-9]{1,3}m$"), 
         datetime, P_ground_hPa, H_Wm2_Eco, LE_Wm2_Eco, H_EC_measured_sonic_30m, u_star)


#####begin the heights tuning####
# heights
heights <- c(1,4,9,14,19,24,30,40,55,70,85,100,125,148)
pairs <- combn(heights, m = 2, simplify = FALSE) # get combos, result: c(down, up)
# create plot?
plot = FALSE

# storage
result = data.frame(datetime = K_data_tuning$datetime, 
                    LE_Wm2_Eco = K_data_tuning$LE_Wm2_Eco, 
                    H_Wm2_Eco = K_data_tuning$H_Wm2_Eco, 
                    H_EC_measured_sonic_30m = K_data_tuning$H_EC_measured_sonic_30m, 
                    u_star = K_data_tuning$u_star)
rmse_mean = c()
rmse_H = c()
rmse_LE = c()



for(i in 1:length(pairs)){
  
  h_down = pairs[[i]][1]
  h_up = pairs[[i]][2]
  
  
  # APPLY FUNCTION for H for 
  result[[paste0("H_", h_down, "_" , h_up)]] = 
    K_theory(
      H2O_mmol_mol_up = K_data_tuning[[paste0("H2O_", h_up, "m")]], 
      H2O_mmol_mol_down = K_data_tuning[[paste0("H2O_", h_down, "m")]], 
      Ta_dgC_up = K_data_tuning[[paste0("Ta_", h_up, "m")]], 
      Ta_dgC_down = K_data_tuning[[paste0("Ta_", h_down, "m")]], 
      height_diff_m = h_up-h_down, 
      P_ground_hPa = K_data_tuning$P_ground_hPa, 
      H_Wm2_EC_measured = K_data_tuning$H_EC_measured_sonic_30m,
      u_star = K_data_tuning$u_star, 
      type = "sensible"
    )
  
  # apply function for LE
  # apply the function for LE
  result[[paste0("LE_", h_down, "_" , h_up)]] = 
    K_theory(
      H2O_mmol_mol_up = K_data_tuning[[paste0("H2O_", h_up, "m")]], 
      H2O_mmol_mol_down = K_data_tuning[[paste0("H2O_", h_down, "m")]], 
      Ta_dgC_up = K_data_tuning[[paste0("Ta_", h_up, "m")]], 
      Ta_dgC_down = K_data_tuning[[paste0("Ta_", h_down, "m")]], 
      height_diff_m = h_up-h_down, 
      P_ground_hPa = K_data_tuning$P_ground_hPa, 
      H_Wm2_EC_measured = K_data_tuning$H_EC_measured_sonic_30m,
      u_star = K_data_tuning$u_star, 
      type = "latent"
    )
  
  # plot the results and compare to EC Eco 30m 
  model_LE = lm(
    as.formula(paste0("LE_", h_down, "_", h_up, " ~ LE_Wm2_Eco")),
    data = result)
  
  # get results & calculate RMSE LE
  y <- result[[paste0("LE_", h_down, "_", h_up)]]
  x <- result$LE_Wm2_Eco
  rmse_LE <- append(rmse_LE, sqrt(mean((y - x)^2, na.rm = TRUE)))
  
  ###### for H #######
  model_H = lm(
    as.formula(paste0("H_", h_down, "_", h_up, " ~ H_Wm2_Eco")),
    data = result)
  
  # get results and calculate RMSE for H
  y <- result[[paste0("H_", h_down, "_", h_up)]]
  x <- result$H_Wm2_Eco
  rmse_H <- append(rmse_H, sqrt(mean((y - x)^2, na.rm = TRUE)))
  
  # store mean
  rmse_mean = append(rmse_mean, mean(c(rmse_H[i], rmse_LE[i])))
  
  
  if(plot == TRUE){
    par(mfrow = c(1, 2))
    
    # plot H
    plot(
      x = result$H_Wm2_Eco,
      y = result[[paste0("H_", h_down, "_", h_up)]],
      ylab = paste0("LE modeled K-theory", h_down, "m & ", h_up, "m [Wm2]"),
      xlab = "H EC eco 30m [Wm2]",
      main = paste("H", h_down, "-", h_up), 
      cex = 0.5,
      ylim = c(-250, 600)
    )
    
    # grid, legend abline etc
    grid()
    abline(0, 1, lwd = 2, lty = 2)
    abline(model_H, lwd = 2, col = "red")
    legend(
      "topleft",
      legend = paste("R2 =", round(summary(model_H)$r.squared, 3)),
      bty = "n")
    
    # plot LE
    plot(
      x = result$LE_Wm2_Eco,
      y = result[[paste0("LE_", h_down, "_", h_up)]],
      ylab = paste0("LE modeled K-theory ", h_down, "m & ", h_up, "m [Wm2]"),
      xlab = "LE EC eco 30m [Wm2]",
      main = paste("LE", h_down, "-", h_up),
      cex = 0.5,
      ylim = c(-250, 600)
    )
    
    # grid, legend abline etc
    grid()
    abline(0, 1, lwd = 2, lty = 2)
    abline(model_LE, lwd = 2, col = "red")
    legend(
      "topleft",
      legend = paste("R2 =", round(summary(model_LE)$r.squared, 3)),
      bty = "n")
    
    par(mfrow = c(1, 1))
  }
  
}

# extract results

h_down_vec <- sapply(pairs, `[`, 1)
h_up_vec   <- sapply(pairs, `[`, 2)

res <- data.frame(
  h_down = h_down_vec,
  h_up   = h_up_vec,
  rmse_mean = rmse_mean, 
  rmse_H = rmse_H, 
  rmse_LE = rmse_LE
)

# get mininum rmse
res[which(res$rmse_mean == min(res$rmse_mean)),] # mean
res[which(res$rmse_H == min(res$rmse_H)),] # H
res[which(res$rmse_LE == min(res$rmse_LE)),] # LE


# 19 and 40m for overall min(rmse_H & rmse_LE)
# 14 and 40m for H
# 19 & 125 for LE


# which one is worst? (Thomas said that the 30m and 148m would be the worst and I shall proof him wrong)
# well, lets see!
res_sorted = res%>%
  arrange(rmse_LE)
# well, its 1m and 14m (which is within the canopy)
# however, 30m and 148m is not too bad (overall)


#### plot the first three best results
plot = TRUE

for(i in 1:3){
  
  h_down = res_sorted$h_down[i]
  h_up = res_sorted$h_up[i]
  
  if(plot == TRUE){
    par(mfrow = c(1, 2))
    
    ###### for H
    model_H = lm(
      as.formula(paste0("H_", h_down, "_", h_up, " ~ H_Wm2_Eco")),
      data = result)
    
    # plot H
    plot(
      x = result$H_Wm2_Eco,
      y = result[[paste0("H_", h_down, "_", h_up)]],
      ylab = paste0("H modeled K-theory", h_down, "m & ", h_up, "m [Wm2]"),
      xlab = "H EC eco 30m [Wm2]",
      main = paste("H", h_down, "-", h_up), 
      cex = 0.5,
      ylim = c(-250, 600)
    )
    
    # grid, legend abline etc
    grid()
    abline(0, 1, lwd = 2, lty = 2)
    abline(model_H, lwd = 2, col = "red")
    legend(
      "topleft",
      legend = paste("R2 =", round(summary(model_H)$r.squared, 3)),
      bty = "n")
    
    
    ######### LE 
    # get the lm for trend line
    model_LE = lm(
      as.formula(paste0("LE_", h_down, "_", h_up, " ~ LE_Wm2_Eco")),
      data = result)
    
    # plot LE
    plot(
      x = result$LE_Wm2_Eco,
      y = result[[paste0("LE_", h_down, "_", h_up)]],
      ylab = paste0("LE modeled K-theory ", h_down, "m & ", h_up, "m [Wm2]"),
      xlab = "LE EC eco 30m [Wm2]",
      main = paste("LE", h_down, "-", h_up),
      cex = 0.5,
      ylim = c(-250, 600)
    )
    
    # grid, legend abline etc
    grid()
    abline(0, 1, lwd = 2, lty = 2)
    abline(model_LE, lwd = 2, col = "red")
    legend(
      "topleft",
      legend = paste("R2 =", round(summary(model_LE)$r.squared, 3)),
      bty = "n")
    
    par(mfrow = c(1, 1))
  } 
}





plot = ggplot(result)+
  geom_line(aes(x = datetime, y = LE_24_55), color = "darkred")+
  geom_line(aes(x = datetime, y = LE_Wm2_Eco), color = "black")

  
#ggplotly(plot)
