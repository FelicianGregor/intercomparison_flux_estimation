##### investigate stability/u_star and footprint effects on prediction accuracy #####

library(tidyverse)
library(ggpmisc)

## load data set for K Theory
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/fluxes_BREB.RData") # fluxes from BREB
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/fluxes_MBR.RData") # fluxes from K theory
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/sonic_profile_data.RData")

# combine both df 
impact_df = left_join(MBR_data, BREB%>%select(-H_Wm2_Eco, -LE_Wm2_Eco), by = "datetime")

# get stability parameter z-d/L
zeta = sonic_profile_data%>%
  select(datetime, `(z-d)/L_[#]`, `L_[m]`)

impact_df = left_join(impact_df, zeta, by = "datetime")

# stability classification based on Sorbjan and Grachev 2010: “An Evaluation of the FluxGradient Relationship in the Stable Boundary Layer
classify_stability_fine <- function(zeta) {
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
classify_stability = function(zeta){
  ifelse(is.na(zeta), NA,
         ifelse(zeta < -0.02, "unstable",
                ifelse(zeta < 0.02, "neutral",
                       "stable")))
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
  rename("MBR H" = H_EC_measured_sonic_30m, 
         "BREB H" = H_19_40_BREB, 
         "BREB LE"  = LE_19_40_BREB, 
         "MBR LE" = LE_Wm2_MBR)%>%
  pivot_longer(
    cols = c("MBR H", "BREB H", "BREB LE", "MBR LE"), 
    names_to = "flux_type", 
    values_to = "flux_value"
  ) %>%
  mutate(
    Eco_data = ifelse(flux_type == "MBR H" | flux_type == "BREB H", 
                      yes = H_Wm2_Eco, 
                      no = LE_Wm2_Eco))

# make the plot
impact_df%>%
  ggplot(aes(x = Eco_data, y = flux_value)) +
  geom_point(size = 0.4, alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 1.5) +
  geom_smooth(aes(color = stability_classes), method = "lm", linewidth = 1) +
  stat_poly_eq(
    aes(color = stability_classes,
        label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 3
  )+
  coord_cartesian(xlim = c(-300, 800), ylim = c(-300, 800)) +
  facet_wrap(~flux_type, ncol = 2, nrow = 2) +
  theme_bw()


### look at u_star effects
# create filer function first:
classify_u_star = function(u_star){
  ifelse(is.na(u_star), NA, 
         ifelse(u_star<0.2, "0-0.2", 
                ifelse(u_star<0.4, "0.2-0.4", 
                       ifelse(u_star<0.6, "0.4-0.6", 
                              ">0.6"))))
}

u_star_plot = impact_df%>%
  mutate(u_star_class = classify_u_star(u_star))%>%
  ggplot(aes(x = Eco_data, y = flux_value)) +
  geom_point(size = 0.4, alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 1.5) +
  geom_smooth(aes(color = u_star_class), method = "lm", linewidth = 1) +
  stat_poly_eq(
    aes(color = u_star_class,
        label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    size = 3
  )+
  coord_cartesian(xlim = c(-300, 900), ylim = c(-300, 900)) +
  facet_wrap(~flux_type, ncol = 2, nrow = 2) +
  labs(y = "Turbulent flux [W/m2]",
       x = "Reference flux ICOS Ecosystem station [W/m2]", 
       color = "u* class")+
  theme_bw()

ggsave(
  filename = "C:/Users/Lenovo/Downloads/u_star_plot.png",
  plot = u_star_plot,
  width = 21, height = 18, units = "cm",
  dpi = 300
)



