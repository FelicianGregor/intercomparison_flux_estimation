##### preprocessing of Eddy Covariance multilevel sonic data ####


##### read in ensemble covariance data #####

library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(plotly)

# set main directory (where all folders are)
main_dir <- "C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/HTM_data/sonic profile/HFdata/2021/output/"

# get subfolders
folders <- list.dirs(main_dir, recursive = FALSE)

# function 
read_full_output <- function(folder_path) {
  
  folder_name <- basename(folder_path)
  
  # find csv with "full_output"
  file <- list.files(folder_path,
                     pattern = "full_output.*\\.csv$",
                     full.names = TRUE)
  
  # skip if none found
  if (length(file) == 0) return(NULL)

  # read in
  df <- read_csv(file[1], skip = 1)
  
  units = df[1, ] # get the units in first row and make them names
  
  colname_vec = paste(names(df), units, sep = "_")
  names(df) = colname_vec
  
  df = df[-c(1), ] # delete units row
  
  # make numeric
  df = df%>%
    mutate(across(-c(`date_[yyyy-mm-dd]`, `time_[HH:MM]`, filename_NA), 
                  as.numeric))%>%
    mutate(datetime = as.POSIXct(
      paste(`date_[yyyy-mm-dd]`, `time_[HH:MM]`),
      format = "%Y-%m-%d %H:%M",
      tz = "UTC"))
  
  df[df == -9999] = NA # replace -9999 by NA
  
  # split folder name into components
  meta <- str_split(folder_name, "_", simplify = TRUE)
  
  # add metadata columns
  df <- df %>%
    mutate(
      folder = folder_name,
      year = meta[1],
      height = meta[2],
      rotation = meta[3],
      detrending = meta[4]
    )
  
  return(df)
}

# apply to all folders and rowbind
sonic_profile_data <- map_dfr(folders, read_full_output)

# there is an issue with the time: the sonic data is processed in a way that apparently the datetime refers to the start of the half an hour, and not to the end as it is for the ICOS Ecosystem data
# therefore: subtract 30min from the datetime object
sonic_profile_data$datetime = sonic_profile_data$datetime - 30*60

# calculate ensemble mean
sonic_profile_data = sonic_profile_data%>%
  group_by(height, datetime)%>%
  mutate(
    H_ensemble_mean = mean(`H_[W+1m-2]`)
  )%>%
  ungroup()

# plot the different ensemble members and the mean, dynamically 
# select specific time frame
library(plotly)
library(ggplot2)

plot_time_data = sonic_profile_data %>%
  filter(folder == "2021_148m_double_block")

#  filter(between(date(datetime),
#                 ymd("2021-05-21"),
#                 ymd("2021-05-26")))

plot_time <- ggplot(plot_time_data) +
  geom_line(aes(x = datetime, y = `H_[W+1m-2]`, color = folder)) +
  theme_bw()

plot_time

#ggplotly(plot_time)

# make comparison of the detrending procedure: block and linear
sonic_profile_data%>%
  filter(between(date(datetime),
                 ymd("2021-01-21"),
                 ymd("2021-05-26")))%>%
  filter(height == "30m")%>%
  select(datetime, detrending, rotation, `H_[W+1m-2]`) %>%
  pivot_wider(
    names_from = detrending,
    values_from = `H_[W+1m-2]`
  )%>%
  ggplot()+
  geom_point(aes(x = block, y = linear))+
  ylab("H linear [Wm2]")+
  xlab("H block [Wm2]")+
  geom_abline(intercept = 0, slope = 1, color = "black", size = 1.5)+
  geom_smooth(aes(x = block, y = linear), method = "lm", color = "red", size = 1)+
  facet_grid(~rotation)+
  theme_bw()

# comparison for measurement height of sonic
# ATTENTION!!! I HAD TO FILTER OUT VALUES FOR 148m > abs(1000)!!! 
sonic_profile_data%>%
  filter(between(date(datetime),
                 ymd("2021-01-21"),
                 ymd("2021-05-26")))%>%
  select(datetime, detrending, rotation, height, `H_[W+1m-2]`) %>%
  pivot_wider(
    names_from = height,
    values_from = `H_[W+1m-2]`
  )%>%
  # filter (without any reason, 1000 is arbitrarily chosen by looking at the data)
  filter(`148m`<1000& `148m`>-1000)%>%
  ggplot()+
  geom_point(aes(x = `30m`, y = `70m`), col = "darkgrey", alpha = .3, size = 0.5)+
  geom_point(aes(x = `30m`, y = `148m`), col = "darkblue", alpha = .3, size = 0.5)+
  ylab("H 70m / 148m [Wm2]")+
  xlab("H 30m [Wm2]")+
  geom_abline(intercept = 0, slope = 1, color = "black", size = 1.5)+
  geom_smooth(aes(x = `30m`, y = `70m`), method = "lm", color = "red", size = 1)+
  geom_smooth(aes(x = `30m`, y = `148m`), method = "lm", col = "darkblue", size = 1)+
  theme_bw()


# time series plot with all the data processing options
height_comp <- sonic_profile_data %>%
  filter(
    between(date(datetime),
            ymd("2021-05-21"),
            ymd("2021-05-26"))
  ) %>%
  mutate(`H_[W+1m-2]` = 
           ifelse(`qc_H_[#]`==  2, yes = NA, no = `H_[W+1m-2]`))%>%
  ggplot() +
  geom_line(aes(y = `H_[W+1m-2]`, x = datetime, color = folder)) +
  theme_bw()

height_comp

# compare them using a scatter plot
# load EC data from ecosystem station for comparison
load("C:/Users/Lenovo/Documents/Physical_Geography/master_thesis/scripts_master_thesis/data/processed/Eco_data_30m.RData") # Ecosystem data 30m

sonic_profile_data %>%
  left_join(
    Eco_data_30m %>%
      rename(H_Wm2_Eco = H_Wm2) %>%
      select(datetime, H_Wm2_Eco),
    by = "datetime"
  ) %>%
  #filter(
   # between(date(datetime),
    #        ymd("2021-01-21"),
    #        ymd("2021-05-26"))
  #) %>%
  filter(`qc_H_[#]` != 2 & `qc_H_[#]` != 1)%>%
  # filter out spikes very roughly
  filter(abs(H_ensemble_mean) < 750 )%>%
  ggplot(aes(x = H_Wm2_Eco, y = H_ensemble_mean)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "black", linewidth = 2) +
  geom_smooth(method = "lm", color = "red")+
  coord_equal(xlim = c(-170, 750),
             ylim = c(-170, 750))+
  facet_grid(~height)+
  labs(y = "H [Wm2]", 
       x = "H [Wm2] 30m Ecosystem station")+
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )
  


# save the full year to .RData
save(sonic_profile_data, file = "data/processed/sonic_profile_data.Rdata")

rm(first_part, second_part, sonic_profile_data)


### check for time interval issue:
time_data = sonic_profile_data%>%
  filter(folder == "2021_30m_triple_block")

time_data = time_data %>%
  left_join(Eco_data_30m%>%select(datetime, H_Wm2)%>%rename(H_Wm2_Eco = H_Wm2), by = "datetime")

time_data%>%
  filter(datetime > as.POSIXct("2021-06-21 00:00:00 UTC") &
           datetime < as.POSIXct("2021-06-28 00:00:00 UTC")) %>%
  ggplot()+
  geom_line(aes(x = datetime, y = `H_[W+1m-2]`), color = "red") +
  geom_line(aes(x = datetime, y = H_Wm2_Eco), color = "darkgreen") +
  geom_vline(
    xintercept = as.POSIXct("2021-06-21 15:00:00 UTC"),
    color = "black"
  ) +
  theme_classic()

# range of sonic data:
range(sonic_profile_data$datetime)
# range of ecosystem data
range(Eco_data_30m$datetime)
