##### preprocessing of Eddy Covariance multilevel sonic data ####


##### read in ensemble covariance data #####

library(dplyr)
library(purrr)
library(stringr)
library(readr)

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

# calculate ensemble mean
sonic_profile_data = sonic_profile_data%>%
  group_by(height, datetime)%>%
  mutate(
    H_ensemble_mean = mean(`H_[W+1m-2]`)
  )%>%
  ungroup()

# plot the different ensemble members and the mean
# select specific time frame
sonic_profile_data%>%
  filter(between(date(datetime),
                 ymd("2021-06-21"),
                 ymd("2021-06-26")))%>%
  filter(height == "30m")%>%
  ggplot()+
  geom_line(aes(x = datetime, y = `H_[W+1m-2]`, color = folder))+
  geom_line(aes(x = datetime, y = H_ensemble_mean), color = "black", label = "mean")+
  theme_bw()


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

 
# save the full year to .RData
save(sonic_profile_data, file = "data/processed/sonic_profile_data.Rdata")

rm(first_part, second_part, sonic_profile_data)



  


