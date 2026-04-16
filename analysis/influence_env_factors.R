##### investigate stability/u_star and footprint effects on prediction accuracy #####

library(tidyverse)
library(ggpmisc)

## load data set for K Theory
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/fluxes_BREB.RData") # fluxes from BREB
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/fluxes_K_theory.RData") # fluxes from K theory
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/sonic_profile_data.RData")

# combine both df 
impact_df = left_join(K_theory, BREB%>%select(-H_Wm2_Eco, -LE_Wm2_Eco), by = "datetime")

# get stability parameter z-d/L
zeta = sonic_profile_data%>%
  select(datetime, `(z-d)/L_[#]`, `L_[m]`)

impact_df = left_join(impact_df, zeta, by = "datetime")

# stability classification based on Sorbjan and Grachev 2010: “An Evaluation of the FluxGradient Relationship in the Stable Boundary Laye
classify_stability <- function(zeta) {
  ifelse(is.na(zeta), NA,
         ifelse(zeta < -1, "xu",
                ifelse(zeta < -0.6, "vu",
                       ifelse(zeta < -0.2, "u",
                              ifelse(zeta < -0.02, "wu",
                                     ifelse(zeta < 0.02, "n",
                                            ifelse(zeta < 0.2, "ws",
                                                   ifelse(zeta < 0.6, "s",
                                                          ifelse(zeta < 1, "vs",
                                                                 "xs")))))))))
}

# apply function and get col with stability classes using Sorbjan and Grachev 2010:
impact_df = impact_df%>%
  mutate(stability_classes = classify_stability(zeta = impact_df$`(z-d)/L_[#]`))

# check perc of the classes
impact_df %>%
  count(stability_classes) %>%
  mutate(perc = n / sum(n) * 100)

# use only Obhukov length with stability classes (classes are rather arbitrary, but guided by the graphics from Stull 1988 page 181)
impact_df = impact_df%>%
  mutate(
  stability_L = case_when(
    `L_[m]` > 50  ~ "stable (L > 50)",
    `L_[m]` < -50 ~ "unstable (L < -50)",
    TRUE          ~ "neutral (abs(L) < 50)"
  ))

# prepare plotting
impact_df = impact_df %>%
  rename(H_Wm2_K = H_19_40_K, 
         H_Wm2_BREB = H_19_40_BREB, 
         LE_Wm2_BREB  = LE_19_40_BREB, 
         LE_Wm2_K = LE_19_40_K)%>%
  pivot_longer(
    cols = c(H_Wm2_K, LE_Wm2_K, LE_Wm2_BREB, H_Wm2_BREB), 
    names_to = "flux_type", 
    values_to = "flux_value"
  ) %>%
  mutate(
    Eco_data = ifelse(flux_type == "H_Wm2_K" | flux_type == "H_Wm2_BREB", 
                      yes = H_Wm2_Eco, 
                      no = LE_Wm2_Eco))

# make the plot
impact_df%>%
  ggplot(aes(x = Eco_data, y = flux_value)) +
  geom_point(size = 0.4, alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 1.5) +
  geom_smooth(aes(color = stability_L), method = "lm", linewidth = 1) +
  stat_poly_eq(
    aes(color = stability_L,
        label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 3
  )+
  coord_cartesian(xlim = c(-300, 800), ylim = c(-300, 800)) +
  facet_wrap(~flux_type, ncol = 2, nrow = 2) +
  theme_bw()


### look at u_star effects
impact_df%>%
  filter(between(datetime, 
                 as.POSIXct("2021-07-01 01:00:00", tz = "UTC"),
                 as.POSIXct("2021-08-07 01:00:00", tz = "UTC")))%>%
  pivot_longer(cols = c(H_24_55_K, LE_24_55_K, LE_19_40_BREB, H_19_40_BREB), 
               names_to = "flux_type", 
               values_to = "flux_value")%>%
  ggplot(aes(x = u_star, y = flux_value))+
  geom_point(size = 0.4, alpha = 0.3) +
  geom_smooth( method = "loess", linewidth = 1, color = "red") +
  facet_wrap(~flux_type, ncol = 2, nrow = 2)+
  theme_bw()

### Attention ###
# doing the quality control and then investigating the effects of different parameters can trick me!
# I used already the u* filtering procedure for the K_theory fluxes (0.15), then this introduces errors in the statistical analysis later


