#### preprocessing ecosystem station radiation & eddy covariance data ####

##### read data from 30m EC station (radiation and EC for comparison) ####
library(readxl)
library(lubridate)
library(tidyverse)

# read data
radiation_data = read_excel("data/SE-Htm_ETC_L2_data_2020-2023.xlsx", sheet = 3)

#delete one last value, its creating a mess...

# clean and set date 
# note: Time zone is UTC here! 
radiation_data = radiation_data%>%
  mutate(datetime = ymd_hm(TIMESTAMP_END))%>%
  mutate(G_1 = ifelse(G_1 == -9999, yes = NA, no = G_1))%>%
  mutate(NETRAD_1_1_1 = ifelse(NETRAD_1_1_1 == -9999, yes = NA, no = NETRAD_1_1_1))%>%
  mutate(PA_1_1_1 = ifelse(PA_1_1_1 == -9999, yes = NA, no =PA_1_1_1))%>%
  mutate(PA_1_1_1 = PA_1_1_1*10, # convert to hPa
         NEE_VUT_REF = ifelse(NEE_VUT_REF == -999, yes = NA, no = NEE_VUT_REF)) 

radiation_data = radiation_data[-which(is.na(radiation_data$datetime)),]

# what do i need for BREB?
# delta T and Delta e from profile measurements
# net radiation
# ground heat flux 

#1 get ground heat flux

plot(radiation_data$datetime, radiation_data$G_1, col = "brown", type = "l", las = 1, xlab = "", ylab = "G [W/m2]")
plot(radiation_data$datetime, radiation_data$NETRAD_1_1_1, col = "darkgreen", type = "l", las = 1, xlab = "", ylab = "G [W/m2]")

# check out H data
plot(radiation_data$datetime, radiation_data$H_F_MDS, col = "purple", type = "l", las = 1, xlab = "", ylab = "H [W/m2]")

# calculate fraction of data gap filled:
radiation_data %>%
  summarise(LE = mean(LE_F_MDS_QC != "0"), 
            H = mean(H_F_MDS_QC != "0"), 
            NEE = mean(NEE_VUT_REF_QC != "0"))
  

# filter gap filled data from the EC data: all flags > 0 are gap filled (more info here: https://doi.org/10.1038/s41597-020-0534-3)
radiation_data = radiation_data%>%
  mutate(LE_F_MDS = ifelse(LE_F_MDS_QC==0, yes = LE_F_MDS, no = NA), 
         H_F_MDS = ifelse(H_F_MDS_QC == 0, yes = H_F_MDS,no = NA), 
         NEE_VUT_REF = ifelse(NEE_VUT_REF_QC == 0, yes = NEE_VUT_REF, no = NA))

# inspect H remaining fluxes - this is not a lot...
plot(radiation_data$datetime[4800:5200], radiation_data$H_F_MDS[4800:5200 ], col = "purple", type = "l", las = 1, xlab = "", ylab = "H [W/m2]")


# rename 
Eco_data_30m = radiation_data # whole data frame


# also single columns
Eco_data_30m = Eco_data_30m%>%
  rename(G_Wm2 = G_1, 
         H_Wm2 = H_F_MDS, 
         LE_Wm2 = LE_F_MDS, 
         R_Net_Wm2 = NETRAD_1_1_1, 
         P_ground_hPa = PA_1_1_1, 
         CO2_μmol_m2 = NEE_VUT_REF)

# save
save(x = Eco_data_30m, file = "data/processed/Eco_data_30m.RData")

#remove for saving space
rm(radiation_data, Eco_data_30m)


