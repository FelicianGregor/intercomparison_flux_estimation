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
  lambda = 2.25e6 #in Joule per kg (although in reality dependend on T!)
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

##### Tune the heights #####
# prep data
# merge 
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/sonic_profile_data.RData") # EC data output from different heights
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/slow_profile_data.RData") # load the 14 level profile data for Ta and Humidity
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/Eco_data_30m.RData") # Ecosystem data 30m

# some data pr

# join in profile dataset
BREB_data_tuning = slow_profile_data%>%
  left_join(Eco_data_30m%>%select(datetime, P_ground_hPa, LE_Wm2, H_Wm2, R_Net_Wm2, G_Wm2), by = "datetime")%>%
  rename(
    LE_Wm2_Eco = LE_Wm2, 
    H_Wm2_Eco = H_Wm2
  )

# select only needed vars
BREB_data_tuning = BREB_data_tuning %>%
  select(-contains('qc')) %>%
  select(matches("^(Ta|H2O)_[0-9]{1,3}m$"), 
         datetime, P_ground_hPa, H_Wm2_Eco, LE_Wm2_Eco, G_Wm2, R_Net_Wm2)





# heights
heights <- c(1,4,9,14,19,24,30,40,55,70,85,100,125,148)
pairs <- combn(heights, m = 2, simplify = FALSE) # get combos, result: c(down, up)
# create plot?
plot = FALSE

# storage for predictions
result = data.frame(datetime = BREB_data_tuning$datetime, 
                    LE_Wm2_truth = BREB_data_tuning$LE_Wm2_Eco, 
                    H_Wm2_truth = BREB_data_tuning$H_Wm2_Eco)
rmse_mean = c()
rmse_H = c()
rmse_LE = c()



for(i in 1:length(pairs)){
  
  h_down = pairs[[i]][1]
  h_up = pairs[[i]][2]
  
  
  # APPLY FUNCTION for H for 
  result[[paste0("H_", h_down, "_", h_up)]] = 
    BREB(
      H2O_mmol_mol_up = BREB_data_tuning[[paste0("H2O_", h_up, "m")]], 
      H2O_mmol_mol_down = BREB_data_tuning[[paste0("H2O_", h_down, "m")]], 
      Ta_dgC_up = BREB_data_tuning[[paste0("Ta_", h_up, "m")]], 
      Ta_dgC_down = BREB_data_tuning[[paste0("Ta_", h_down, "m")]], 
      P_ground_hPa = BREB_data_tuning$P_ground_hPa, 
      R_net_Wm2 = BREB_data_tuning$R_Net_Wm2, 
      G_Wm2 = BREB_data_tuning$G_Wm2, 
      type = "sensible"
    )
  
  # apply function for LE
  result[[paste0("LE_", h_down, "_" , h_up)]] = 
    BREB(
      H2O_mmol_mol_up = BREB_data_tuning[[paste0("H2O_", h_up, "m")]], 
      H2O_mmol_mol_down = BREB_data_tuning[[paste0("H2O_", h_down, "m")]], 
      Ta_dgC_up = BREB_data_tuning[[paste0("Ta_", h_up, "m")]], 
      Ta_dgC_down = BREB_data_tuning[[paste0("Ta_", h_down, "m")]], 
      P_ground_hPa = BREB_data_tuning$P_ground_hPa, 
      R_net_Wm2 = BREB_data_tuning$R_Net_Wm2, 
      G_Wm2 = BREB_data_tuning$G_Wm2, 
      type = "latent"
    )
  
  # plot the results and compare to EC Eco 30m 
  model_LE = lm(
    as.formula(paste0("LE_", h_down, "_", h_up, " ~ LE_Wm2_truth")),
    data = result)
  
  # get results & calculate RMSE LE
  y <- result[[paste0("LE_", h_down, "_", h_up)]]
  x <- result$LE_Wm2_truth
  rmse_LE <- append(rmse_LE, sqrt(mean((y - x)^2, na.rm = TRUE)))
  
  ###### for H #######
  model_H = lm(
    as.formula(paste0("H_", h_down, "_", h_up, " ~ H_Wm2_truth")),
    data = result)
  
  # get results and calculate RMSE for H
  y <- result[[paste0("H_", h_down, "_", h_up)]]
  x <- result$H_Wm2_truth
  rmse_H <- append(rmse_H, sqrt(mean((y - x)^2, na.rm = TRUE)))
  
  # store mean
  rmse_mean = append(rmse_mean, mean(c(rmse_H[i], rmse_LE[i])))
  
  
  if(plot == TRUE){
    par(mfrow = c(1, 2))
    
    # plot H
    plot(
      x = result$H_Wm2_truth,
      y = result[[paste0("H_", h_down, "_", h_up)]],
      ylab = paste0("LE modeled ", h_down, "m & ", h_up, "m [Wm2]"),
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
      x = result$LE_Wm2_truth,
      y = result[[paste0("LE_", h_down, "_", h_up)]],
      ylab = paste0("LE modeled ", h_down, "m & ", h_up, "m [Wm2]"),
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

res_sorted = res%>%
  arrange(rmse_mean)
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
      as.formula(paste0("H_", h_down, "_", h_up, " ~ H_Wm2_truth")),
      data = result)
    
    # plot H
    plot(
      x = result$H_Wm2_truth,
      y = result[[paste0("H_", h_down, "_", h_up)]],
      ylab = paste0("H modeled BREB ", h_down, "m & ", h_up, "m [Wm2]"),
      xlab = "H EC eco 30m [Wm2]",
      main = paste("H", h_down, "-", h_up), 
      cex = 0.5,
      ylim = c(-350, 800)
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
      as.formula(paste0("LE_", h_down, "_", h_up, " ~ LE_Wm2_truth")),
      data = result)
    
    # plot LE
    plot(
      x = result$LE_Wm2_truth,
      y = result[[paste0("LE_", h_down, "_", h_up)]],
      ylab = paste0("LE modeled BREB ", h_down, "m & ", h_up, "m [Wm2]"),
      xlab = "LE EC eco 30m [Wm2]",
      main = paste("LE", h_down, "-", h_up),
      cex = 0.5,
      ylim = c(-350, 800)
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


### eyeball results for different months

plot = result %>%
  filter(between(datetime,
                 as.POSIXct("2021-12-07 01:00:00", tz = "UTC"),
                 as.POSIXct("2021-12-14 01:00:00", tz = "UTC"))) %>%
  ggplot() +
  geom_line(aes(x = datetime, y = LE_19_40), color = "darkred") +
  geom_line(aes(x = datetime, y = LE_Wm2_truth), color = "black")

plot

# save the result to use later
BREB = result%>%
  select(datetime, 
         LE_Wm2_truth, 
         H_Wm2_truth,
         H_19_40, 
         LE_19_40)%>%
  rename(LE_19_40_BREB = LE_19_40, 
         H_19_40_BREB = H_19_40)

# save
save(x = BREB, file = "data/processed/fluxes_BREB.RData")
