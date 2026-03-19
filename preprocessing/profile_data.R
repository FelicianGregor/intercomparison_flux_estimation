#### preprocessing of temperature and gas profile data #####


# load packages
library(tidyverse)
library(lubridate)
library(readxl)


# read in
H2O_data = read_excel("data/gas_profile.xlsx", sheet = 2)

# make numeric
H2O_data[, 4:ncol(H2O_data)] = lapply(H2O_data[, 4:ncol(H2O_data)], as.numeric)

# set dates
H2O_data = H2O_data %>%
  mutate(datetime = dmy(date) + seconds(as.numeric(time) * 86400))%>%
  mutate(datetime = force_tz(datetime, tzone = "UTC"))%>%
  # rename h20
  rename(H2O_1m = H2O_1_14_1, 
         H2O_4m = H2O_1_13_1, 
         H2O_9m = H2O_1_12_1, 
         H2O_14m = H2O_1_11_1, 
         H2O_19m = H2O_1_10_1, 
         H2O_24m = H2O_1_9_1, 
         H2O_30m = H2O_1_8_1, 
         H2O_40m = H2O_1_7_1, 
         H2O_55m = H2O_1_6_1, 
         H2O_70m = H2O_1_5_1, 
         H2O_85m = H2O_1_4_1, 
         H2O_100m = H2O_1_3_1, 
         H2O_125m = H2O_1_2_1, 
         H2O_1148m = H2O_1_1_1)%>%
  # the same for co2
  rename(CO2_1m = CO2_1_14_1, 
         CO2_4m = CO2_1_13_1, 
         CO2_9m = CO2_1_12_1, 
         CO2_14m = CO2_1_11_1, 
         CO2_19m = CO2_1_10_1, 
         CO2_24m = CO2_1_9_1, 
         CO2_30m = CO2_1_8_1, 
         CO2_40m = CO2_1_7_1, 
         CO2_55m = CO2_1_6_1, 
         CO2_70m = CO2_1_5_1, 
         CO2_85m = CO2_1_4_1, 
         CO2_100m = CO2_1_3_1, 
         CO2_125m = CO2_1_2_1, 
         CO2_1148m = CO2_1_1_1)

# interested in 30m - 150 m contrast
# 30m = H2O_1_8_1
# 148m  = H2O_1_1_1
plot(x = H2O_data$datetime, H2O_data$H2O_1_8_1, type = "l", col = "blue", las = 1, xlab = "", ylab = "mixing ratio H20")
lines(x = H2O_data$datetime, H2O_data$H2O_1_1_1, type = "l", col = "red", las = 1, xlab = "", ylab = "mixing ratio H20")

##### prepare delta T from profile data ####

#read in T data from profile csv file 
T_data = read_excel("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/HTM_data/Profile_14Levels/temperature_profile.xlsx", sheet = 2)
# make numeric
T_data[, 4:ncol(T_data)] = lapply(T_data[, 4:ncol(T_data)], as.numeric)

# set dates
T_data = T_data %>%
  mutate(datetime = dmy(date) + seconds(as.numeric(time) * 86400))

# interested in 30m - 148 m contrast
# 30m = T_1_8_1
# 148m  = T_1_1_1
plot(x = T_data$datetime, T_data$Ta_1_8_1, type = "l", col = "blue", las = 1, xlab = "", ylab = "Air temperature [ºC]")
lines(x = T_data$datetime, T_data$Ta_1_1_1, type = "l", col = "red", las = 1, xlab = "", ylab = "Air temperature [ºC]")

# calculate difference
T_data$delta_T_C = T_data$Ta_1_8_1 - T_data$Ta_1_1_1

###### make the join with Humidity data #####

slow_profile_data = left_join(T_data, H2O_data, by = "datetime")

# rename columns:
slow_profile_data = slow_profile_data%>%
  rename(Ta_dgC_148m = Ta_1_1_1, 
         Ta_dgC_30m  = Ta_1_8_1, 
         H2O_mmol_mol_148m = H2O_1_1_1, 
         H2O_mmol_mol_30m = H2O_1_8_1)

# write to Rdata file
save(x = slow_profile_data, file = "data/processed/slow_profile_data.RData")

# remove
rm(T_data, H2O_data, slow_profile_data)


