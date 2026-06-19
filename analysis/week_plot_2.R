library(tidyverse)
library(patchwork)
library(grid)

## -----------------------------
## LOAD + PREP
## -----------------------------

## load data
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/fluxes_BREB.RData")
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/fluxes_MBR.RData")
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/sonic_profile_data.RData")
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/Eco_data_30m.RData")


Eco_data_30m[Eco_data_30m == -9999] <- NA

week_df <- MBR_data %>%
  left_join(BREB %>% select(-H_Wm2_Eco, -LE_Wm2_Eco), by = "datetime") %>%
  left_join(Eco_data_30m %>% select(datetime, R_Net_Wm2, G_Wm2, WS, WD, SW_IN_1_1_1, P_mm, Ta_dgC),
            by = "datetime")

## -----------------------------
## COMMON THEME
## -----------------------------

theme_common <- theme_bw() +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.6), color = NA),
    legend.title = element_blank(),
    plot.tag = element_text(size = 9, face = "bold"),
    plot.tag.position = c(-0.013, 0.92),
    #axis.text.x = element_blank(),
    plot.margin = margin(5, 1, 5, 5)
  )

x_scale <- scale_x_datetime(date_breaks = "1 day", date_labels = "%d %b")

## -----------------------------
## SUMMER / WINTER SUBSETS
## -----------------------------

summer_week <- week_df %>%
  filter(datetime >= as.POSIXct("2021-07-01 UTC"),
         datetime <  as.POSIXct("2021-07-08 UTC"))

winter_week <- week_df %>%
  filter(datetime >= as.POSIXct("2021-12-01 UTC"),
         datetime <  as.POSIXct("2021-12-08 UTC"))

## =========================================================
## FLUX PLOTS (SUMMER EXAMPLE)
## =========================================================

p_H_summer <- summer_week %>%
  pivot_longer(
    c(H_Wm2_Eco, H_EC_measured_sonic_30m, H_19_40_BREB),
    names_to = "Method",
    values_to = "Flux"
  ) %>%
  
  mutate(
    Method = recode(Method,
                    "H_Wm2_Eco"              = "EC",
                    "H_EC_measured_sonic_30m" = "MBR",
                    "H_19_40_BREB"           = "BREB"),
    Method = factor(Method, levels = c("BREB", "EC", "MBR"))
  ) %>%
  
  ggplot(aes(datetime, Flux, colour = Method, shape = Method)) +
  
  geom_line() +
  geom_point(size = 1) +
  
  scale_colour_manual(values = c(
    "EC"   = "black",
    "MBR"  = "#0072B2",
    "BREB" = "#D55E00"
  )) +
  
  scale_shape_manual(values = c(
    "EC"   = 16,
    "MBR"  = 2,
    "BREB" = 15
  )) +
  
  x_scale +
  
  labs(
    colour = "Sensible heat flux",
    shape  = "Sensible heat flux",
    y = expression(H~"["*W~m^{-2}*"]"),
    x = NULL, title = "Summer"
  ) +
  
  theme_common +
  
  theme(
    plot.title = element_text(hjust = 0.5),
    
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    legend.box = "vertical",
    
    # tight spacing
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.spacing.y = unit(0.05, "cm"),
    
    legend.key.height = unit(0.32, "cm"),
    legend.key.width  = unit(0.5, "cm"),
    
    legend.title = element_text(
      size = 9,
      face = "plain",
      margin = margin(b = 2)
    ),
    
    axis.text.x = element_blank()
  )


p_LE_summer <- summer_week %>%
  pivot_longer(
    c(LE_Wm2_Eco, LE_Wm2_MBR, LE_19_40_BREB),
    names_to = "Method",
    values_to = "Flux"
  ) %>%
  
  mutate(
    Method = recode(Method,
                    "LE_Wm2_Eco"    = "EC",
                    "LE_Wm2_MBR"    = "MBR",
                    "LE_19_40_BREB" = "BREB"),
    
    Method = factor(Method, levels = c("BREB", "MBR", "EC"))
  ) %>%
  
  ggplot(aes(datetime, Flux, colour = Method, shape = Method)) +
  
  geom_line() +
  geom_point(size = 1) +
  
  scale_colour_manual(values = c(
    "EC"   = "black",
    "MBR"  = "#0072B2",
    "BREB" = "#D55E00"
  )) +
  
  scale_shape_manual(values = c(
    "EC"   = 16,
    "MBR"  = 2,
    "BREB" = 15
  )) +
  
  x_scale +
  
  labs(
    colour = "Latent heat flux",
    shape  = "Latent heat flux",
    y = expression(LE~"["*W~m^{-2}*"]"),
    x = NULL
  ) +
  
  theme_common +
  
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    legend.box = "vertical",
    
    #tight spacing
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.spacing.y = unit(0, "cm"),
    legend.spacing.x = unit(0, "cm"),
    
    legend.key.height = unit(0.30, "cm"),
    legend.key.width  = unit(0.45, "cm"),
    
    legend.title = element_text(
      size = 9,
      face = "plain",
      margin = margin(b = 1)
    ),
    
    axis.text.x = element_blank()
  )


p_H_error_summer <- summer_week %>%
  mutate(
    MBR  = H_EC_measured_sonic_30m - H_Wm2_Eco,
    BREB = H_19_40_BREB - H_Wm2_Eco
  ) %>%
  
  pivot_longer(
    c(MBR, BREB),
    names_to = "Method",
    values_to = "Error"
  ) %>%
  
  ggplot(aes(datetime, Error, colour = Method, shape = Method)) +
  
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(size = 1.3) +
  
  scale_colour_manual(values = c(
    "MBR"  = "#0072B2",
    "BREB" = "#D55E00"
  )) +
  
  scale_shape_manual(values = c(
    "MBR"  = 2,
    "BREB" = 15
  )) +
  
  x_scale +
  
  labs(
    colour = "H Error: BRE/MBR - EC",
    shape  = "H Error: BRE/MBR - EC",
    y = "[W m-2]",
    x = NULL
  ) +
  
  theme_common +
  
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    legend.box = "vertical",
    
    # tight spacing (same system as LE)
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.spacing.y = unit(0, "cm"),
    legend.spacing.x = unit(0, "cm"),
    
    legend.key.height = unit(0.30, "cm"),
    legend.key.width  = unit(0.45, "cm"),
    
    legend.title = element_text(
      size = 9,
      face = "plain",
      margin = margin(b = 1)
    ),
    
    axis.text.x = element_blank()
  )


          
p_LE_error_summer <- summer_week %>%
  mutate(
    MBR  = LE_Wm2_MBR - LE_Wm2_Eco,
    BREB = LE_19_40_BREB - LE_Wm2_Eco
  ) %>%
  
  pivot_longer(
    c(MBR, BREB),
    names_to = "Method",
    values_to = "Error"
  ) %>%
  
  ggplot(aes(datetime, Error, colour = Method, shape = Method)) +
  
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(size = 1.3) +
  
  scale_colour_manual(values = c(
    "MBR"  = "#0072B2",
    "BREB" = "#D55E00"
  )) +
  
  scale_shape_manual(values = c(
    "MBR"  = 2,
    "BREB" = 15
  )) +
  
  x_scale +
  
  labs(
    colour = "LE Error: BREB/MBR - EC",
    shape  = "LE Error: BREB/MBR - EC",
    y = "[W m-2]",
    x = NULL
  ) +
  
  theme_common +
  
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    legend.box = "vertical",
    
    # tight spacing (consistent with H error)
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.spacing.y = unit(0, "cm"),
    legend.spacing.x = unit(0, "cm"),
    
    legend.key.height = unit(0.30, "cm"),
    legend.key.width  = unit(0.45, "cm"),
    
    legend.title = element_text(
      size = 9,
      face = "plain",
      margin = margin(b = 1)
    ),
    
    axis.text.x = element_blank()
  )

## -----------------------------
## ENERGY (WITH Ta SECOND AXIS)
## -----------------------------

scale_factor <- max(summer_week$R_Net_Wm2 - summer_week$G_Wm2, na.rm = TRUE) /
  max(summer_week$Ta_dgC, na.rm = TRUE)

p_energy_summer <- summer_week %>%
  ggplot(aes(datetime)) +
  
  geom_line(aes(y = R_Net_Wm2 - G_Wm2, colour = "Rnet - G [W m-2]")) +
  geom_line(aes(y = Ta_dgC * scale_factor, colour = "Ta [°C]")) +
  
  scale_y_continuous(
    name = expression("["*W~m^{-2}*"]"),
    sec.axis = sec_axis(~ . / scale_factor, name = "Ta [°C]")
  ) +
  
  scale_colour_manual(values = c(
    "Rnet - G [W m-2]" = "orange",
    "Ta [°C]"          = "darkred"
  )) +
  
  x_scale +
  
  labs(x = NULL) +
  
  theme_common +
  
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.background = element_rect(fill = alpha("white", 0.6), color = NA),
    
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.spacing.y = unit(0, "cm"),
    legend.spacing.x = unit(0, "cm"),
    
    legend.key.height = unit(0.30, "cm"),
    legend.key.width  = unit(0.45, "cm"),
    
    axis.title.y.left  = element_text(color = "orange"),
    axis.text.y.left   = element_text(color = "orange"),
    axis.ticks.y.left  = element_line(color = "orange"),
    
    axis.text.y.right  = element_text(color = "darkred"),
    axis.ticks.y.right = element_line(color = "darkred"),
    
    axis.text.x = element_blank(), 
    axis.title.y.right = element_blank()
  )

## -----------------------------
## MET
## -----------------------------

scale_factor_P_summer <- max(summer_week$P_mm, na.rm = TRUE) /
  max(summer_week$WS, na.rm = TRUE)

p_met_summer <- summer_week %>%
  ggplot(aes(x = datetime)) +
  
  geom_line(aes(y = WS, color = "U [m/s]")) +
  
  geom_col(aes(y = P_mm / scale_factor_P_summer,
               fill = "Precipitation P [mm/30 min]"),
           alpha = 0.8) +
  
  scale_color_manual(values = c(
    "U [m/s]" = "#009E73"
  )) +
  
  scale_fill_manual(values = c(
    "Precipitation P [mm/30 min]" = "blue"
  )) +
  
  scale_y_continuous(
    name = "U [m/s]",
    sec.axis = sec_axis(~ . * scale_factor_P_summer,
                        name = "")
  ) +
  
  scale_x_datetime(date_breaks = "1 day", date_labels = "%d %b") +
  
  labs(x = NULL) +
  
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.background = element_rect(fill = alpha("white", 0.6), color = NA),
    #legend.background = element_blank(),
    legend.key.width = unit(0.2, "cm"),
    legend.key.height = unit(0.4, "cm"), 
    
    axis.title.y.left  = element_text(color = "#009E73"),
    axis.text.y.left   = element_text(color = "#009E73"),
    axis.ticks.y.left  = element_line(color = "#009E73"),
    
    axis.text.y.right  = element_text(color = "blue"),
    axis.ticks.y.right = element_line(color = "blue"), 
    axis.title.y.right = element_blank(), 
    
    plot.margin = margin(5, 0, 5, 5)
    
  )

## =========================================================
## WINTER FLUX PLOTS
## =========================================================

p_H_winter <- winter_week %>%
  pivot_longer(
    c(H_Wm2_Eco, H_EC_measured_sonic_30m, H_19_40_BREB),
    names_to = "Method",
    values_to = "Flux"
  ) %>%
  
  mutate(
    Method = recode(Method,
                    "H_Wm2_Eco"               = "EC",
                    "H_EC_measured_sonic_30m" = "MBR",
                    "H_19_40_BREB"            = "BREB"),
    Method = factor(Method, levels = c("BREB", "MBR", "EC"))
  ) %>%
  
  ggplot(aes(datetime, Flux, colour = Method, shape = Method)) +
  geom_line() +
  geom_point(size = 1) +
  
  scale_colour_manual(values = c(
    "EC"   = "black",
    "MBR"  = "#0072B2",
    "BREB" = "#D55E00"
  )) +
  
  scale_shape_manual(values = c(
    "EC"   = 16,
    "MBR"  = 2,
    "BREB" = 15
  )) +
  
  x_scale +
  
  labs(
    colour = "Sensible heat flux",
    shape  = "Sensible heat flux",
    y = expression(H~"["*W~m^{-2}*"]"),
    x = NULL, title = "Winter"
  ) +
  
  theme_common +
  
  theme(
    plot.title = element_text(hjust = 0.5),
    
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    legend.box = "vertical",
    
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.spacing.y = unit(0.05, "cm"),
    
    legend.key.height = unit(0.32, "cm"),
    legend.key.width  = unit(0.5, "cm"),
    
    axis.title.y.left  = element_blank(),
    
    legend.title = element_text(size = 9, face = "plain"),
    
    axis.text.x = element_blank()
  )



## =========================================================
## WINTER LE
## =========================================================

p_LE_winter <- winter_week %>%
  pivot_longer(
    c(LE_Wm2_Eco, LE_Wm2_MBR, LE_19_40_BREB),
    names_to = "Method",
    values_to = "Flux"
  ) %>%
  
  mutate(
    Method = recode(Method,
                    "LE_Wm2_Eco"    = "EC",
                    "LE_Wm2_MBR"    = "MBR",
                    "LE_19_40_BREB" = "BREB"),
    Method = factor(Method, levels = c("BREB", "MBR", "EC"))
  ) %>%
  
  ggplot(aes(datetime, Flux, colour = Method, shape = Method)) +
  geom_line() +
  geom_point(size = 1) +
  
  scale_colour_manual(values = c(
    "EC"   = "black",
    "MBR"  = "#0072B2",
    "BREB" = "#D55E00"
  )) +
  
  scale_shape_manual(values = c(
    "EC"   = 16,
    "MBR"  = 2,
    "BREB" = 15
  )) +
  
  x_scale +
  
  labs(
    colour = "Latent heat flux",
    shape  = "Latent heat flux",
    y = expression(LE~"["*W~m^{-2}*"]"),
    x = NULL
  ) +
  
  theme_common +
  
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    legend.box = "vertical",
    
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.spacing.y = unit(0.05, "cm"),
    
    legend.key.height = unit(0.30, "cm"),
    legend.key.width  = unit(0.45, "cm"),
    
    axis.title.y.left  = element_blank(),
    
    legend.title = element_text(size = 9, face = "plain"),
    
    axis.text.x = element_blank()
  )



## =========================================================
## WINTER ERRORS
## =========================================================

p_H_error_winter <- winter_week %>%
  mutate(
    MBR  = H_EC_measured_sonic_30m - H_Wm2_Eco,
    BREB = H_19_40_BREB - H_Wm2_Eco
  ) %>%
  
  pivot_longer(c(MBR, BREB), names_to = "Method", values_to = "Error") %>%
  
  ggplot(aes(datetime, Error, colour = Method, shape = Method)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(size = 1.3) +
  
  scale_colour_manual(values = c(
    "MBR"  = "#0072B2",
    "BREB" = "#D55E00"
  )) +
  
  scale_shape_manual(values = c(
    "MBR"  = 2,
    "BREB" = 15
  )) +
  
  x_scale +
  
  labs(
    colour = "H Error: BREB/MBR - EC",
    shape  = "H Error: BREB/MBR - EC",
    y = "[W m-2]",
    x = NULL
  ) +
  
  theme_common +
  
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    legend.box = "vertical",
    
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.spacing.y = unit(0, "cm"),
    
    legend.key.height = unit(0.30, "cm"),
    legend.key.width  = unit(0.45, "cm"),
    
    axis.title.y.left  = element_blank(),
    
    legend.title = element_text(size = 9, face = "plain"),
    
    axis.text.x = element_blank()
  )



p_LE_error_winter <- winter_week %>%
  mutate(
    MBR  = LE_Wm2_MBR - LE_Wm2_Eco,
    BREB = LE_19_40_BREB - LE_Wm2_Eco
  ) %>%
  
  pivot_longer(c(MBR, BREB), names_to = "Method", values_to = "Error") %>%
  
  ggplot(aes(datetime, Error, colour = Method, shape = Method)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(size = 1.3) +
  
  scale_colour_manual(values = c(
    "MBR"  = "#0072B2",
    "BREB" = "#D55E00"
  )) +
  
  scale_shape_manual(values = c(
    "MBR"  = 2,
    "BREB" = 15
  )) +
  
  x_scale + 
  
  labs(
    colour = "LE Error: BREB/MBR - EC",
    shape  = "LE Error: BREB/MBR - EC",
    y = "[W m-2]",
    x = NULL
  ) +
  
  theme_common +
  
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    legend.box = "vertical",
    
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.spacing.y = unit(0, "cm"),
    
    legend.key.height = unit(0.30, "cm"),
    legend.key.width  = unit(0.45, "cm"),
    
    axis.title.y.left  = element_blank(),
    
    legend.title = element_text(size = 9, face = "plain"),
    
    axis.text.x = element_blank()
  )



## =========================================================
## WINTER ENERGY
## =========================================================

scale_factor_w <- max(winter_week$R_Net_Wm2 - winter_week$G_Wm2, na.rm = TRUE) /
  max(winter_week$Ta_dgC, na.rm = TRUE)

p_energy_winter <- winter_week %>%
  ggplot(aes(datetime)) +
  
  geom_line(aes(y = R_Net_Wm2 - G_Wm2, colour = "Rnet - G [W m-2]")) +
  geom_line(aes(y = Ta_dgC * scale_factor_w, colour = "Ta [°C]")) +
  
  scale_y_continuous(
    name = expression("["*W~m^{-2}*"]"),
    sec.axis = sec_axis(~ . / scale_factor_w, name = "Ta [°C]")
  ) +
  
  scale_colour_manual(values = c(
    "Rnet - G [W m-2]" = "orange",
    "Ta [°C]"          = "darkred"
  )) +
  
  x_scale +
  
  labs(x = NULL) +
  
  theme_common +
  
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    legend.box = "vertical",
    
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.spacing.y = unit(0, "cm"),
    
    legend.key.height = unit(0.30, "cm"),
    legend.key.width  = unit(0.45, "cm"),
    
    axis.title.y.left  = element_blank(),
    
    axis.text.y.left   = element_text(color = "orange"),
    axis.ticks.y.left  = element_line(color = "orange"),
    
    axis.text.y.right  = element_text(color = "darkred"),
    axis.ticks.y.right = element_line(color = "darkred"), 
    axis.title.y.right  = element_text(color = "darkred"),
    
    axis.text.x = element_blank()
  )



## =========================================================
## WINTER MET
## =========================================================

# delete two extreme values 
winter_week[which(winter_week$WS == max(winter_week$WS, na.rm = T)), "WS"] = NA
winter_week[which(winter_week$WS == max(winter_week$WS, na.rm = T)), "WS"] = NA


scale_factor_P_w <- max(winter_week$P_mm, na.rm = TRUE) /
  max(winter_week$WS, na.rm = TRUE)

p_met_winter <- winter_week %>%
  ggplot(aes(x = datetime)) +
  
  geom_line(aes(y = WS, color = "U [m/s]")) +
  
  geom_col(aes(y = P_mm / scale_factor_P_w,
               fill = "Precipitation P [mm/30 min]"),
           alpha = 0.8) +
  
  scale_color_manual(values = c(
    "U [m/s]" = "#009E73"
  )) +
  
  scale_fill_manual(values = c(
    "Precipitation P [mm/30 min]" = "blue"
  )) +
  
  scale_y_continuous(
    name = "U [m/s]",
    sec.axis = sec_axis(~ . * scale_factor_P_w, name = "[mm/30 min]")
  ) +
  
  x_scale +
  
  labs(x = NULL, fill = NULL, color = NULL) +
  
  theme_bw() +
  
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    
    legend.key.width = unit(0.2, "cm"),
    legend.key.height = unit(0.4, "cm"), 
    axis.title.y.left  = element_blank(),
    axis.text.y.left   = element_text(color = "#009E73"),
    axis.ticks.y.left  = element_line(color = "#009E73"),
    
    axis.text.y.right  = element_text(color = "blue"),
    axis.ticks.y.right = element_line(color = "blue"), 
    axis.title.y.right  = element_text(color = "blue")
  )



## =========================================================
## FINAL
## =========================================================

# make the annotation:
p_H_summer <- p_H_summer +
  labs(tag = "a") &
  theme(
    plot.tag = element_text(size = 9, face = "bold"),
    plot.tag.position = c(-0.013, 0.92)
  )

p_H_error_summer <- p_H_error_summer +
  labs(tag = "b") &
  theme(
    plot.tag = element_text(size = 9, face = "bold"),
    plot.tag.position = c(-0.013, 0.92)
  )

p_LE_summer <- p_LE_summer +
  labs(tag = "c") &
  theme(
    plot.tag = element_text(size = 9, face = "bold"),
    plot.tag.position = c(-0.013, 0.92)
  )

p_LE_error_summer <- p_LE_error_summer +
  labs(tag = "d") &
  theme(
    plot.tag = element_text(size = 9, face = "bold"),
    plot.tag.position = c(-0.013, 0.92)
  )

p_energy_summer <- p_energy_summer +
  labs(tag = "e") &
  theme(
    plot.tag = element_text(size = 9, face = "bold"),
    plot.tag.position = c(-0.013, 0.92)
  )

p_met_summer <- p_met_summer +
  labs(tag = "f") &
  theme(
    plot.tag = element_text(size = 9, face = "bold"),
    plot.tag.position = c(-0.013, 0.92)
  )

week_summer <- (p_H_summer / p_H_error_summer /
                  p_LE_summer / p_LE_error_summer /
                  p_energy_summer / p_met_summer)
  

week_winter <- (p_H_winter / p_H_error_winter /
                  p_LE_winter / p_LE_error_winter /
                  p_energy_winter / p_met_winter)

plot_final <- (week_summer | week_winter)

ggsave("plots/weeks2.pdf",
       plot_final,
       width = 21.5, height = 15.8, units = "cm", dpi = 300)
