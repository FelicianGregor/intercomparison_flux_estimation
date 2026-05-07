#### preprocessing of temperature and gas profile data #####


# load packages
library(tidyverse)
library(lubridate)
library(readxl)


# read in
H2O_data = read_excel("data/gas_profile.xlsx", sheet = 3)

# make numeric
H2O_data[, 4:ncol(H2O_data)] = lapply(H2O_data[, 4:ncol(H2O_data)], as.numeric)
H2O_data = H2O_data[-1, ]

# set dates
H2O_data <- H2O_data %>%
  mutate(datetime = dmy(date) + seconds(as.numeric(time) * 86400)) %>% # make datetime object
  mutate(datetime = force_tz(datetime, tzone = "CET")) %>% # set the tz: is CET, as given in excel table in 2nd row of date column
  mutate(datetime = with_tz(datetime, tzone = "UTC"))%>% # convert to UTC time.
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
         H2O_148m = H2O_1_1_1)%>%
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
         CO2_148m = CO2_1_1_1)%>%
  #for the quality control
  rename(
    qc_H2O_1m   = qc_H2O_1_14_1,
    qc_H2O_4m   = qc_H2O_1_13_1,
    qc_H2O_9m   = qc_H2O_1_12_1,
    qc_H2O_14m  = qc_H2O_1_11_1,
    qc_H2O_19m  = qc_H2O_1_10_1,
    qc_H2O_24m  = qc_H2O_1_9_1,
    qc_H2O_30m  = qc_H2O_1_8_1,
    qc_H2O_40m  = qc_H2O_1_7_1,
    qc_H2O_55m  = qc_H2O_1_6_1,
    qc_H2O_70m  = qc_H2O_1_5_1,
    qc_H2O_85m  = qc_H2O_1_4_1,
    qc_H2O_100m = qc_H2O_1_3_1,
    qc_H2O_125m = qc_H2O_1_2_1,
    qc_H2O_148m = qc_H2O_1_1_1, 
    
    # for co2
    qc_CO2_1m   = qc_CO2_1_14_1,
    qc_CO2_4m   = qc_CO2_1_13_1,
    qc_CO2_9m   = qc_CO2_1_12_1,
    qc_CO2_14m  = qc_CO2_1_11_1,
    qc_CO2_19m  = qc_CO2_1_10_1,
    qc_CO2_24m  = qc_CO2_1_9_1,
    qc_CO2_30m  = qc_CO2_1_8_1,
    qc_CO2_40m  = qc_CO2_1_7_1,
    qc_CO2_55m  = qc_CO2_1_6_1,
    qc_CO2_70m  = qc_CO2_1_5_1,
    qc_CO2_85m  = qc_CO2_1_4_1,
    qc_CO2_100m = qc_CO2_1_3_1,
    qc_CO2_125m = qc_CO2_1_2_1,
    qc_CO2_148m = qc_CO2_1_1_1
  )


##### prepare delta T from profile data ####

#read in T data from profile csv file 
T_data = read_excel("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/HTM_data/Profile_14Levels/temperature_profile.xlsx", sheet = 3)
# make numeric
T_data[, 4:ncol(T_data)] = lapply(T_data[, 4:ncol(T_data)], as.numeric)
T_data = T_data[-1, ]

# set dates
T_data = T_data %>%
  mutate(datetime = dmy(date) + seconds(as.numeric(time) * 86400))%>%
  # rename temp data
  rename(
    Ta_1m   = Ta_1_14_1,
    Ta_4m   = Ta_1_13_1,
    Ta_9m   = Ta_1_12_1,
    Ta_14m  = Ta_1_11_1,
    Ta_19m  = Ta_1_10_1,
    Ta_24m  = Ta_1_9_1,
    Ta_30m  = Ta_1_8_1,
    Ta_40m  = Ta_1_7_1,
    Ta_55m  = Ta_1_6_1,
    Ta_70m  = Ta_1_5_1,
    Ta_85m  = Ta_1_4_1,
    Ta_100m = Ta_1_3_1,
    Ta_125m = Ta_1_2_1,
    Ta_148m = Ta_1_1_1
  )%>%
# do the same for quality flags
rename(
  qc_Ta_1m   = qc_Ta_1_14_1,
  qc_Ta_4m   = qc_Ta_1_13_1,
  qc_Ta_9m   = qc_Ta_1_12_1,
  qc_Ta_14m  = qc_Ta_1_11_1,
  qc_Ta_19m  = qc_Ta_1_10_1,
  qc_Ta_24m  = qc_Ta_1_9_1,
  qc_Ta_30m  = qc_Ta_1_8_1,
  qc_Ta_40m  = qc_Ta_1_7_1,
  qc_Ta_55m  = qc_Ta_1_6_1,
  qc_Ta_70m  = qc_Ta_1_5_1,
  qc_Ta_85m  = qc_Ta_1_4_1,
  qc_Ta_100m = qc_Ta_1_3_1,
  qc_Ta_125m = qc_Ta_1_2_1,
  qc_Ta_148m = qc_Ta_1_1_1
)


####
#plot the profiles 
H2O_data_plotting <- H2O_data %>%
  pivot_longer(
    cols = matches("^(CO2|H2O)_"),
    names_to = c("gas", "height_m"),
    names_pattern = "(CO2|H2O)_(.*)",
    values_to = "value"
  ) %>%
  mutate(height_m = as.numeric(str_remove(height_m, "m")))

# do the profile plot using geom_path
H2O_data_plotting %>%
  filter(between(date(datetime),
                 ymd("2021-04-21"),
                 ymd("2021-04-26"))) %>%
  filter(gas == "H2O") %>%
  mutate(
    date = date(datetime),
    hour = hour(datetime)
  ) %>%
  arrange(datetime, height_m) %>%
  ggplot(aes(
    x = value,
    y = height_m,
    group = datetime,
    color = hour
  )) +
  geom_path() +
  geom_abline(intercept = 19, slope = 0, color = "grey")+
  ylab("Height [m]") +
  xlab("H2O mixing ratio [mmol/mol]") +
  facet_wrap(~date, ncol = 3, nrow = 3)+
  theme_bw()

### for Temp data
T_data_plotting <- T_data %>%
  pivot_longer(
    cols = matches("^Ta_"),
    names_to = "height_m",
    values_to = "value"
  ) %>%
  mutate(
    # make numeric from character
    height_m = str_remove(height_m, "Ta_"), 
    height_m = str_remove(height_m, "m"), 
    height_m = as.numeric(height_m))

T_data_plotting %>%
  filter(between(date(datetime),
                 ymd("2021-07-01"),
                 ymd("2021-07-07"))) %>%
  mutate(
    date = date(datetime),
    hour = hour(datetime)
  ) %>%
  arrange(datetime, height_m) %>%
  ggplot(aes(
    x = value,
    y = height_m,
    group = datetime,
    color = hour
  )) +
  geom_path() +
  geom_abline(intercept = 19, slope = 0, color = "grey")+
  ylab("Height [m]") +
  xlab("Air temperature [°C]") +
  facet_wrap(~date, ncol = 3, nrow = 3)+
  theme_bw()
  

###### make the join with Humidity data #####

slow_profile_data = left_join(T_data, H2O_data, by = "datetime")

# rename columns:
slow_profile_data = slow_profile_data%>%
  mutate(Ta_dgC_148m = Ta_148m, 
         Ta_dgC_30m  = Ta_30m, 
         H2O_mmol_mol_148m = H2O_148m, 
         H2O_mmol_mol_30m = H2O_30m)

# write to Rdata file
save(x = slow_profile_data, file = "data/processed/slow_profile_data.RData")

# remove
rm(T_data, H2O_data, slow_profile_data, T_data_plotting, H2O_data_plotting)



