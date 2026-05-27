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
  mutate(stability_class = classify_stability(`(z-d)/L_[#]`))

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

extract_lm_stats <- function(df){
  
  # remove missing values
  df <- df %>%
    filter(
      !is.na(Eco_data),
      !is.na(flux_value))
  
  
  # make NA if too few observations
  if(nrow(df) < 3){
    return(
      tibble(
        n = nrow(df),
        equation = NA_character_,
        intercept_bias = NA_real_,
        slope = NA_real_,
        R2 = NA_real_,
        MAE = NA_real_,
        MSE = NA_real_,
        RMSE = NA_real_
      ))}
  
  # linear model
  model <- lm(flux_value ~ Eco_data, data = df)
  
  # predictions for getting the residual
  pred <- predict(model)
  resid <- df$flux_value - pred
  
  #get coefficients
  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  
  # calculate evaluation metrics
  r2   <- summary(model)$r.squared
  mae  <- mean(abs(resid))
  mse  <- mean(resid^2)
  rmse <- sqrt(mse)
  
  # paste the equation together
  equation <- paste0(
    "y = ",
    round(intercept, 3),
    " + ",
    round(slope, 3),
    "x"
  )
  
  # summarise
  tibble(
    n = nrow(df),
    equation = equation,
    intercept_bias = intercept,
    slope = slope,
    R2 = r2,
    MAE = mae,
    MSE = mse,
    RMSE = rmse
  )
}

###############################################################
##### influence of stability parameter 
###############################################################

stability_results <- impact_df_long %>%
  group_by(flux_type, stability_class) %>%
  group_modify(~ extract_lm_stats(.x)) %>%
  ungroup()

# overall models (all stability classes combined)
stability_overall <- impact_df_long %>%
  group_by(flux_type) %>%
  group_modify(~ extract_lm_stats(.x)) %>%
  mutate(stability_class = "overall") %>%
  ungroup()

# cbind
stability_results_all <- bind_rows(
  stability_results,
  stability_overall
)

print(stability_results_all, n = 40)

###############################################################
##### influence of u*
###############################################################

u_star_results <- impact_df_long %>%
  group_by(flux_type, u_star_class) %>%
  group_modify(~ extract_lm_stats(.x)) %>%
  ungroup()

# overall models
u_star_overall <- impact_df_long %>%
  group_by(flux_type) %>%
  group_modify(~ extract_lm_stats(.x)) %>%
  mutate(u_star_class = "overall") %>%
  ungroup()

#cbind
u_star_results_all <- bind_rows(
  u_star_results,
  u_star_overall
)

print(u_star_results_all, n = 50)

### write to latex (only stability results)
library(kableExtra)
table_tex <- stability_results %>%
  select(-equation)%>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    digits = 2
  )

# write
writeLines(
  table_tex,
  "C:/Users/Lenovo/Downloads/stability_table.tex"
)

