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
  mutate(datetime = force_tz(datetime, tzone = "UTC"))

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


