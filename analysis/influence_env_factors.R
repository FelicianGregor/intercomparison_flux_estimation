##### investigate stability/u_star effects with lm models #####

library(tidyverse)


## load data
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/fluxes_BREB.RData")
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/fluxes_MBR.RData")
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/sonic_profile_data.RData")
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/Eco_data_30m.RData")

# combine datasets
impact_df <- left_join(
  MBR_data,
  BREB %>% select(-H_Wm2_Eco, -LE_Wm2_Eco),
  by = "datetime"
)

# add R_net and G for energy balance closure 
impact_df <- left_join(
  impact_df, Eco_data_30m%>%select(datetime, R_Net_Wm2, G_Wm2), 
  by = "datetime")


# add Obhukhov length L and zeta (stability parameter based on L)
zeta <- sonic_profile_data %>%
  select(datetime, `(z-d)/L_[#]`, `L_[m]`)

impact_df <- left_join(impact_df, zeta, by = "datetime")

##### stability classification based on Biermann et al. 2014 #####
classify_stability <- function(zeta){
  ifelse(is.na(zeta), NA,
         ifelse(zeta < -0.0625, "unstable",
                ifelse(zeta < 0.0625, "neutral",
                       "stable")))}
# apply 
impact_df <- impact_df %>%
  mutate(stability_class = classify_stability(`(z-d)/L_[#]`))%>%
  # ensure correct order of stability
  mutate(
    stability_class = factor(
      stability_class,
      levels = c("stable", "neutral", "unstable"),
      ordered = TRUE))

##### u* classes, as in Billesbach et al. 2024 #####
classify_u_star <- function(u_star){
  ifelse(is.na(u_star), NA,
         ifelse(u_star < 0.2, "0-0.2",
                ifelse(u_star < 0.4, "0.2-0.4",
                       ifelse(u_star < 0.6, "0.4-0.6",
                              ">0.6"))))}
# apply again
impact_df <- impact_df %>%
  mutate(u_star_class = classify_u_star(u_star))
  

##### prepare pivot longer data ######
##### prepare pivot longer data ######
impact_df_long <- impact_df %>%
  mutate(
    `EC EBC` = H_Wm2_Eco + LE_Wm2_Eco,
    available_energy = R_Net_Wm2 - G_Wm2
  ) %>%
  rename(
    `MBR H` = H_EC_measured_sonic_30m,
    `BREB H` = H_19_40_BREB,
    `BREB LE` = LE_19_40_BREB,
    `MBR LE` = LE_Wm2_MBR
  ) %>%
  pivot_longer(
    cols = c(`MBR H`, `BREB H`, `BREB LE`, `MBR LE`, `EC EBC`),
    names_to = "flux_type",
    values_to = "flux_value"
  ) %>%
  # ensure order order 
  mutate(
    flux_type = factor(
      flux_type,
      levels = c("BREB H", "BREB LE", "MBR H", "MBR LE", "EC EBC")
    ))%>%
  mutate(
    Eco_data = case_when(
      flux_type %in% c("MBR H", "BREB H") ~ H_Wm2_Eco,
      flux_type %in% c("MBR LE", "BREB LE") ~ LE_Wm2_Eco,
      flux_type == "EC EBC" ~ available_energy
    )
  )

##### function to extract model statistics #####

get_stats <- function(df){
  
  # remove missing values
  df <- df %>%
    filter(
      !is.na(Eco_data),
      !is.na(flux_value))
  
  
  # make NA if too few observations
  if(nrow(df) < 3){
    return(
      tibble(
        intercept = NA_real_,
        slope = NA_real_,
        R2 = NA_real_,
        MAE = NA_real_,
        NSE = NA_real_
      ))}
  
  # linear model
  model <- lm(flux_value ~ Eco_data, data = df)
  
  # predictions for getting the residual
  pred <- predict(model)
  resid <- df$flux_value - pred
  
  #get coefficients
  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  # calculate R2
  r2   <- summary(model)$r.squared
  
  # paste the equation together
  equation <- paste0(
    "y = ",
    round(intercept, 2),
    " + ",
    round(slope, 2),
    "x"
  )
  
  # calculate other eval metric
  ### ATTENTION: these are not based on the lm! ###
  error = df$flux_value - df$Eco_data
  bias = mean(error, na.rm = T)
  mae  <- mean(abs(error), na.rm = T)
  mse  <- mean(error^2)
  rmse <- sqrt(mse)
  
  # standard error of regression
  std_error = summary(model)$sigma

  #Nash Sutcliff Efficiency for combined model performance/goodness of fit
  obs = df$Eco_data
  pred = df$flux_value
  NSE <- 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)
  
  # Kling-Gupta efficiency, better than NSE
  r <- cor(pred, obs)
  alpha <- sd(pred) / sd(obs)
  beta <- mean(pred) / mean(obs)
  KGE <- 1 - sqrt((r - 1)^2 +  (alpha - 1)^2 +  (beta - 1)^2)
  
  # summarise
  tibble(
    n = nrow(df),
    intercept = intercept,
    Slope = slope,
    R2 = r2,
    Bias = bias, 
    MAE = mae,
    NSE = NSE, 
    KGE = KGE, 
    std_error = std_error
  )
}

###############################################################
##### influence of stability parameter 
###############################################################

stability_results <- impact_df_long %>%
  group_by(flux_type, stability_class) %>%
  group_modify(~ get_stats(.x)) %>%
  ungroup()%>%
  drop_na()

# ensure the correct order 
stability_results <- stability_results %>%
  # ensure order
  arrange(flux_type, stability_class)%>%
  rename("Flux" = flux_type, 
         "Stability" = stability_class, 
         "Intercept" = intercept)%>%
  select(-KGE, -std_error)

print(stability_results, n = 40)

# write to latex table
library(kableExtra)
table_tex_stab <- stability_results %>%
  kbl(
    format = "latex",
    escape = F, 
    booktabs = TRUE,
    digits = 2)

# write
writeLines(
  table_tex_stab,
  "C:/Users/Lenovo/Downloads/stability_table.tex"
)

###############################################################
##### influence of u*
###############################################################

u_star_results <- impact_df_long %>%
  group_by(flux_type, u_star_class) %>%
  group_modify(~ get_stats(.x)) %>%
  ungroup()%>%
  select(-slope)%>%
  # drop NA rows 
  drop_na()

# rename
u_star_results = u_star_results%>%
  rename("Flux" = flux_type, 
         "u*" = u_star_class, 
         "R2" = R2, 
         "Intercept" = intercept)%>%
  select(-KGE, -std_error)

print(u_star_results, n = 50)

### write to latex (only stability results)
library(kableExtra)
table_tex_u <- u_star_results %>%
  kbl(
    format = "latex",
    escape = F, 
    booktabs = TRUE,
    digits = 2
  )

# write
writeLines(
  table_tex_u,
  "C:/Users/Lenovo/Downloads/u_star_table.tex"
)

################################################
###### u* compare with Billesbach values
################################################

df_billesbach <- data.frame(
  `ustar` = as.factor(c('0-0.2', "0.2-0.4", "0.4-0.6", ">0.6")),
  
  BREB_H_Slope  = c(1.413, 1.136, 1.030, 0.914),
  BREB_H_R2     = c(0.928, 0.866, 0.817, 0.753),
  
  BREB_LE_Slope = c(0.935, 1.063, 1.025, 0.982),
  BREB_LE_R2    = c(0.878, 0.921, 0.873, 0.857),
  
  MBR_LE_Slope  = c(0.735, 0.780, 0.937, 1.039),
  MBR_LE_R2     = c(0.811, 0.713, 0.610, 0.598)
) %>%
  pivot_longer(
    -ustar,
    names_to = c("Flux", ".value"),
    names_pattern = "(.*)_(Slope|R2)"
  )%>%
  mutate(
    Flux = str_replace(Flux, pattern = "_", " ")
  )%>%
  rename(`u*` = ustar)%>%
  mutate(`u*` = factor(`u*`, levels = c('0-0.2', '0.2-0.4', '0.4-0.6', '>0.6')))%>% # make order correct
  mutate(study = "Billesbach et al. 2024")
df_billesbach

### bring own u* results in dataframe form for plotting
u_star_combine = u_star_results%>%
  select(-c(Intercept, n, Bias, MAE, NSE))%>%
  mutate(`u*` = factor(`u*`, levels = c('0-0.2', '0.2-0.4', '0.4-0.6', '>0.6')))%>% # make order correct
  mutate(study = "This study")

# combine Mine and Billesbach
df_comparison = rbind(
  u_star_combine, df_billesbach
)%>%
  #remove EBC and MBR H
  filter(Flux != "EC EBC" & Flux != "MBR H")%>%
  pivot_longer(
    cols = c(Slope, R2),
    names_to = "Slope/R2",
    values_to = "value"
  )
  

comparison_slope_r2 = ggplot(df_comparison,
       aes(x = `u*`,
           y = `value`,
           colour = Flux,
           group = interaction(Flux, study))) +
  geom_line(aes(alpha = study)) +
  geom_point(aes(alpha = study), size = 3) +
  scale_alpha_manual(values = c(
    "This study" = 1,
    "Billesbach et al. 2024" = 0.3
  )) +
  labs(y = "", colour = "Method & flux", alpha = "", x = "u* [m/s]")+
  facet_wrap(~`Slope/R2`, scales = "free_y")+
  theme_bw()

comparison_slope_r2

ggsave(
  filename = "C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/plots/comparison_slope_R2.pdf",
  plot = comparison_slope_r2,
  width = 18, height = 8, units = "cm", dpi = 300
)

# r2
ggplot(df_billesbach,
       aes(x = `u*`,
           y = R2,
           colour = Flux, group = Flux)) +
  geom_point(size = 3) +
  geom_line() +
  theme_bw()



#################################################
###### overall evaluation metrics 
#################################################

# overall models
overall <- impact_df_long %>%
  group_by(flux_type) %>%
  group_modify(~ get_stats(.x)) %>%
  mutate(u_star_class = "overall")

# u star group is confusing, actually does not matter here
print(overall)


################################################
###### correlation of u* and stability
################################################

boxplot(u_star ~ stability_class, data = impact_df)



#################################################
##### plot for better diagnostic of patterns 
#################################################

##### STABILITY PLOTS #####

# colors
stab_cols <- c(
  "unstable" = "red",
  "neutral"  = "green",
  "stable"   = "blue"
)

# stability order
stab_order <- c("stable", "neutral", "unstable")

# loop through flux types
for(ft in unique(impact_df_long$flux_type)){
  
  # subset one flux type
  df_ft <- subset(
    impact_df_long,
    flux_type == ft &
      is.finite(Eco_data) &
      is.finite(flux_value)
  )
  
  # open plotting window
  #windows()
  
  # panel layout
  par(
    mfrow = c(1,3),
    mar = c(4,4,3,1)
  )
  
  # loop through stability classes
  for(cl in stab_order){
    
    df_sub <- subset(
      df_ft,
      stability_class == cl
    )
    
    # skip empty groups
    if(nrow(df_sub) < 3){
      
      plot.new()
      title(main = paste(ft, "-", cl, "\nNo data"))
      
      next
    }
    
    # linear model
    mod <- lm(flux_value ~ Eco_data, data = df_sub)
    
    # metrics
    r2 <- round(summary(mod)$r.squared, 2)
    slope <- round(coef(mod)[2], 2)
    
    # scatterplot
    plot(
      df_sub$Eco_data,
      df_sub$flux_value,
      
      pch = 16,
      cex = 0.4,
      
      col = adjustcolor(
        stab_cols[cl],
        alpha.f = 0.3
      ),
      
      xlab = "EC flux [W/m2]",
      ylab = paste(ft, "[W/m2]"),
      
      main = cl,
      
      xlim = c(-300, 900),
      ylim = c(-300, 900)
    )
    
    # 1:1 line
    abline(0, 1, lwd = 2)
    
    # regression line
    abline(
      mod,
      col = stab_cols[cl],
      lwd = 2
    )
    
    # annotations
    legend(
      "topleft",
      legend = c(
        paste("R2 =", r2),
        paste("Slope =", slope)
      ),
      bty = "n"
    )
  }
  
  # overall title
  mtext(
    ft,
    outer = TRUE,
    line = -2,
    cex = 1.5
  )
}

#### U* plots ####
##### U* PLOTS #####

u_cols <- c(
  "0-0.2"   = "darkblue",
  "0.2-0.4" = "green",
  "0.4-0.6" = "orange",
  ">0.6"    = "red"
)

u_order <- c(
  "0-0.2",
  "0.2-0.4",
  "0.4-0.6",
  ">0.6"
)

for(ft in unique(impact_df_long$flux_type)){
  
  df_ft <- subset(
    impact_df_long,
    flux_type == ft &
      is.finite(Eco_data) &
      is.finite(flux_value)
  )
  
  #windows()
  
  par(
    mfrow = c(2,2),
    mar = c(4,4,3,1)
  )
  
  for(cl in u_order){
    
    df_sub <- subset(
      df_ft,
      u_star_class == cl
    )
    
    if(nrow(df_sub) < 3){
      
      plot.new()
      title(main = paste(ft, "-", cl, "\nNo data"))
      
      next
    }
    
    mod <- lm(flux_value ~ Eco_data, data = df_sub)
    
    r2 <- round(summary(mod)$r.squared, 2)
    slope <- round(coef(mod)[2], 2)
    
    plot(
      df_sub$Eco_data,
      df_sub$flux_value,
      
      pch = 16,
      cex = 0.4,
      
      col = adjustcolor(
        u_cols[cl],
        alpha.f = 0.3
      ),
      
      xlab = "EC flux [W/m2]",
      ylab = paste(ft, "[W/m2]"),
      
      main = paste("u* =", cl),
      
      xlim = c(-300, 900),
      ylim = c(-300, 900)
    )
    
    abline(0, 1, lwd = 2)
    
    abline(
      mod,
      col = u_cols[cl],
      lwd = 2
    )
    
    legend(
      "topleft",
      legend = c(
        paste("R2 =", r2),
        paste("Slope =", slope)
      ),
      bty = "n"
    )
  }
  
  mtext(
    ft,
    outer = TRUE,
    line = -2,
    cex = 1.5
  )
}
