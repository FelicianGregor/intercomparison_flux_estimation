##### preprocessing of Eddy Covariance multilevel sonic data ####


# read in and preprocess data as processed by EddyPro
# for the test time frame and only the 30m height system 

# load libraries
library(tidyverse)
library(lubridate)

# read in excel file 
data_proc = read.csv("data/eddypro_12345_full_output_2026-03-06T104434_adv.csv", skip = 1)
names(data_proc) = paste0(names(data_proc), "_", data_proc[1, ])
data_proc = data_proc[-c(1), ] # delete other first rows with units

# make timestamp
data_proc$datetime =  as.POSIXct(paste(data_proc$`date_[yyyy-mm-dd]`, data_proc$`time_[HH:MM]`),format = "%Y-%m-%d %H:%M")

# plot for inspection
plot(x = data_proc$datetime, y = data_proc$`un_H_[W+1m-2]`, type = "l", col = "purple", las = 1, xlab = "", ylab = "H [W/m2]")
abline(h = 0, col = "grey")

# rename
sonic_profile_data = data_proc

rm(data_proc) # remove

save(x = sonic_profile_data, file = "data/processed/sonic_profile_data.Rdata")

# remove data
rm(sonic_profile_data)

################################################################
###### for two data files due to data gap on 21/05/25 ##########
################################################################

first_part = read.csv("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/HTM_data/sonic profile/HFdata/2020/output_2020/eddypro_2345_full_output_2026-03-09T203305_exp.csv", 
                      skip = 1)
names(first_part) = paste0(names(first_part), "_", first_part[1, ])
first_part = first_part[-c(1), ] # delete other first rows with units
# make timestamp
first_part$datetime =  as.POSIXct(paste(first_part$`date_[yyyy-mm-dd]`, first_part$`time_[HH:MM]`),format = "%Y-%m-%d %H:%M", tz = "UCT")

# second part 
second_part = read.csv("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/HTM_data/sonic profile/HFdata/2020/output_2020_05_23/eddypro_9876_full_output_2026-03-09T233852_exp.csv", 
                      skip = 1)
names(second_part) = paste0(names(second_part), "_", second_part[1, ])
second_part = second_part[-c(1), ] # delete other first rows with units
# make timestamp
second_part$datetime =  as.POSIXct(paste(second_part$`date_[yyyy-mm-dd]`, second_part$`time_[HH:MM]`),format = "%Y-%m-%d %H:%M", tz = "UCT")
# replace -9999 by NA
second_part = second_part%>%
  mutate(`H_[W+1m-2]` = ifelse(`H_[W+1m-2]` == -9999, yes = NA, no = `H_[W+1m-2]`))

plot(second_part$datetime, second_part$`H_[W+1m-2]`, las = 1, type = "l", col = "darkred", ylab = "H in Wm2", xlab = "")


###### put together, fill gap in between #####

# create skeleton for dates and times for the whole year!
# dateime refers to the end time of the 30min interval
start <- as.POSIXct("2020-01-01 01:00", tz = "UTC")
end   <- as.POSIXct("2021-01-01 00:00", tz = "UTC")
time_steps_30min_2020 <- seq(from = start, to = end, by = "30 mins") # create sequence

time_skeleton = data.frame("datetime" = time_steps_30min_2020)

# join the other two data frames
full_data = rbind(first_part, second_part)

# join into the date skeleton
sonic_profile_data = merge(time_skeleton, full_data, by = "datetime", all.x = TRUE)

# make -9999 to NA
sonic_profile_data = sonic_profile_data%>%
  mutate(`H_[W+1m-2]` = ifelse(`H_[W+1m-2]` == -9999, yes = NA, no = `H_[W+1m-2]`))%>%
  rename(H_Wm2_sonic_30m = `H_[W+1m-2]`)%>%
  mutate(H_Wm2_sonic_30m = as.numeric(H_Wm2_sonic_30m))



# plot
plot(sonic_profile_data$datetime, sonic_profile_data$H_Wm2_sonic_30m, las = 1, type = "l", col = "darkred", ylab = "H in Wm2", xlab = "")


# save the full year to .RData
save(sonic_profile_data, file = "data/processed/sonic_profile_data.Rdata")

rm(first_part, second_part, sonic_profile_data)
