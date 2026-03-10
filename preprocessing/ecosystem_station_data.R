#### preprocessing ecosystem station radiation & eddy covariance data ####

##### read data from 30m EC station (radiation and EC for comparison) ####
library(readxl)
library(lubridate)
library(tidyverse)

# read data
radiation_data = read_excel("data/SE-Htm_ETC_L2_data_2020-2023.xlsx", sheet = 2)


# clean and set date 
# note: Time zone is UTC here! 
radiation_data = radiation_data%>%
  mutate(datetime = ymd_hm(TIMESTAMP_END))%>%
  mutate(G_1 = ifelse(G_1 == -9999, yes = NA, no = G_1))%>%
  mutate(NETRAD_1_1_1 = ifelse(NETRAD_1_1_1 == -9999, yes = NA, no = NETRAD_1_1_1))%>%
  mutate(PA_1_1_1 = ifelse(PA_1_1_1 == -9999, yes = NA, no =PA_1_1_1))%>%
  mutate(PA_1_1_1 = PA_1_1_1*10) # convert to hPa

# what do i need for BREB?
# delta T and Delta e from profile measurements
# net radiation
# ground heat flux 

#1 get ground heat flux

plot(radiation_data$datetime, radiation_data$G_1, col = "brown", type = "l", las = 1, xlab = "", ylab = "G [W/m2]")
plot(radiation_data$datetime, radiation_data$NETRAD_1_1_1, col = "darkgreen", type = "l", las = 1, xlab = "", ylab = "G [W/m2]")

# check out H data
plot(radiation_data$datetime, radiation_data$H_F_MDS, col = "purple", type = "l", las = 1, xlab = "", ylab = "H [W/m2]")


# rename 
Eco_data_30m = radiation_data # whole data frame

# also single columns
Eco_data_30m = Eco_data_30m%>%
  rename(G_Wm2 = G_1, 
         H_Wm2 = H_F_MDS, 
         LE_Wm2 = LE_F_MDS, 
         R_Net_Wm2 = NETRAD_1_1_1, 
         P_ground_hPa = PA_1_1_1)



# save
save(x = Eco_data_30m, file = "data/processed/Eco_data_30m.RData")

#remove for saving space
rm(radiation_data, Eco_data_30m)


