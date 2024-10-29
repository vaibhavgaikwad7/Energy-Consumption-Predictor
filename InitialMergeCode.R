#####################################
library(arrow)
library(tidyverse)
library(dplyr)
library(lubridate)
library(data.table)

#read in house info data
static_house_info <- read_parquet("https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/static_house_info.parquet")

#####energy#####
#initialize storage for combined energy df
combined_energy = NULL
#loop for energy data
for (i in static_house_info$bldg_id) {
  #create energy url for specific house id
  energy_url = paste0(
    "https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/2023-houseData/",i,".parquet") 
  energy = read_parquet(energy_url)
  energy <- energy[energy$time >= as.POSIXct("2018-07-01 00:00:00") & 
                     energy$time <= as.POSIXct("2018-07-31 23:00:00"), ] #only keep july dates
  energy$total_energy_usage = rowSums(energy[1:42])
  energy$bldg_id = i
  energy = na.omit(energy)
  if (is.null(combined_energy)) {
    combined_energy = energy
  } else {
    combined_energy = rbind(combined_energy, energy)
  }
}

house_energy <- combined_energy %>% left_join(static_house_info, by='bldg_id')
house_energy_less_columns = house_energy[,c(1:45,71)] 


#for some reason the first time of each day (00:00:00) only had the date and not the time,
  #this code fixes that
house_energy_less_columns = house_energy_less_columns %>%
  mutate(time = if_else(str_detect(time, "^\\d{4}-\\d{2}-\\d{2}$"), 
                        paste(time, "00:00:00"), 
                        time))

#convert time to datetime format
house_energy_less_columns$time = as.POSIXct(house_energy_less_columns$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

#####weather#####
#grab unique county codes
county_codes <- unique(house_energy_less_columns$in.county)
#initialize storage for combined_weather df
combined_weather = NULL
#loop for weather data
for (i in county_codes) {
  #create weather url for specific county
  weather_url <- paste0(
    "https://intro-datascience.s3.us-east-2.amazonaws.com/SC-data/weather/2023-weather-data/",i,".csv") 
  weather = read_csv(weather_url)
  #only keep july dates
  weather = weather[weather$date_time >= as.POSIXct("2018-07-01 00:00:00", tz = "UTC") & 
                      weather$date_time <= as.POSIXct("2018-07-31 23:00:00", tz = "UTC"), ] 
  weather$in.county = i
  if (is.null(combined_weather)) {
    combined_weather = weather
  } else {
    combined_weather = rbind(combined_weather, weather)
  }
}

#convert date_time to datetime format
combined_weather$date_time = as.POSIXct(combined_weather$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

#set both as data tables (supposed to use less storage and work faster)
setDT(house_energy_less_columns)
setDT(combined_weather)

#merge energy/house and weather
final_dataset = merge(house_energy_less_columns, combined_weather, 
                      by.x = c("in.county", "time"), 
                      by.y = c("in.county", "date_time"), all.x = TRUE)

#rename time to datetime and add specifically a time column
names(final_dataset)[names(final_dataset) == 'time'] <- 'datetime'
final_dataset$date = as.Date(final_dataset$datetime)
final_dataset$time = format(as.POSIXct(final_dataset$datetime), format = "%H")

#data check 
sum(is.na(final_dataset))

#the 00:00:00 is not appearing at the midnight times so this code just fixes that
final_dataset = final_dataset %>%
  mutate(datetime = if_else(str_detect(datetime, "^\\d{4}-\\d{2}-\\d{2}$"), 
                            paste(datetime, "00:00:00"), 
                            datetime))

#write.csv(final_dataset, "/Users/megankratzer/Desktop/grad school/IST 687 - Intro to Data Science/FinalProject/final_dataset_less_columns.csv", row.names=FALSE)

