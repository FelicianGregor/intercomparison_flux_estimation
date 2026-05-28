##### investigate stability/u_star effects with lm models #####

library(tidyverse)


## load data
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/fluxes_BREB.RData")
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/fluxes_MBR.RData")
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/sonic_profile_data.RData")

# combine datasets
impact_df <- left_join(
  MBR_data,
  BREB %>% select(-H_Wm2_Eco, -LE_Wm2_Eco),
  by = "datetime"
)

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
      levels = c("unstable", "neutral", "stable"),
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
impact_df_long <- impact_df %>%
  rename(
    "MBR H"   = H_EC_measured_sonic_30m,
    "BREB H"  = H_19_40_BREB,
    "BREB LE" = LE_19_40_BREB,
    "MBR LE"  = LE_Wm2_MBR
  ) %>%
  pivot_longer(
    cols = c("MBR H", "BREB H", "BREB LE", "MBR LE"),
    names_to = "flux_type",
    values_to = "flux_value"
  ) %>%
  mutate(
    Eco_data = ifelse(
      flux_type %in% c("MBR H", "BREB H"),
      H_Wm2_Eco,
      LE_Wm2_Eco))

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
    KGE = KGE
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
  arrange(flux_type, stability_class)

stability_results = stability_results%>%
  rename("Flux" = flux_type, 
         "Stability" = stability_class, 
         "R^{2}" = R2, 
         "n" = n, 
         "MAE (Wm^{-2})" = MAE, 
         "Bias (Wm^{-2})" = Bias, 
         "NSE" = NSE, 
         "Slope" = Slope, 
         "Intercept (Wm^{-2})" = intercept)%>%
  select(-KGE)

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

#cbind
u_star_results = u_star_results%>%
  rename("Flux" = flux_type, 
         "U* (ms^{-1})" = u_star_class, 
         "R^{2}" = R2, 
         "n" = n, 
         "MAE (Wm^{-2})" = MAE, 
         "Bias (Wm^{-2})" = Bias, 
         "NSE" = NSE, 
         "Slope" = Slope, 
         "Intercept (Wm^{-2})" = intercept)%>%
  select(-KGE)

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


#################################################
###### overall evaluation metrics 
#################################################

# overall models
u_star_overall <- impact_df_long %>%
  group_by(flux_type) %>%
  group_modify(~ get_stats(.x)) %>%
  mutate(u_star_class = "overall")

# u star group is confusing, actually does not matter here
print(u_star_overall)


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
stab_order <- c("unstable", "neutral", "stable")

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
  windows()
  
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
  
  windows()
  
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
