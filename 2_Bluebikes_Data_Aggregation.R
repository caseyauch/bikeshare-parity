#----------------------SET UP
#load packages
library(terra)
library(sf)
library(tidyverse)

#set working directory
setwd("C:/Users/casey/OneDrive - Tufts/Documents/2022 Thesis/Data/Data_Clean") 

#import data
df = read_csv('./Tables/trips_cleaned_276.csv')
nrow(df)

locations <- read_sf('stations_ids_276.shp')
locations_sp <- as_Spatial(locations)
locations_df <- locations_sp@data

#----------------------DATA SEGMENTATION
#time period 
am_trips <- df %>% filter(period== "AM") 
midday_trips <- df %>% filter(period== "Midday") 
pm_trips <- df %>% filter(period== "PM") 
evening_trips <- df %>% filter(period== "Evening") 
night_trips <- df %>% filter(period== "Night") 

#day of week
weekday_trips <- df %>% filter(day== "Weekday") 
weekend_trips <- df %>% filter(day== "Weekend")

#age
genz_trips <- df %>% filter(age== "Gen Z") 
mill_trips <- df %>% filter(age== "Millenial") 
genx_trips <- df %>% filter(age== "Generation X") 
boomer_trips <- df %>% filter(age== "Baby Boomers") 
silent_trips <- df %>% filter(age== "Silent Generation") 

#user type 
subscriber_trips <- df %>% filter(usertype== "Subscriber") 
customer_trips <- df %>% filter(usertype== "Customer")
subscriber_trips %>% write_csv("subscriber_trips.csv")
customer_trips %>% write_csv("customer_trips.csv")

#----------------------DATA AGGREGATION
trips_by_station <- function(trips, station_locations){ 
  df1 <- trips
  locations <- station_locations
  arrivals_df <- df1 %>% group_by(start_id) %>% summarise(arrivals=n())
  departures_df <- df1 %>% group_by(end_id) %>% summarise(departures=n())
  female_arrivals <- df1 %>% filter(gender =='female') %>% group_by(start_id) %>% summarise(female_arrivals=n())
  male_arrivals <- df1 %>% filter(gender =='male') %>% group_by(start_id) %>% summarise(male_arrivals=n())
  female_departures <- df1 %>% filter(gender =='female') %>% group_by(end_id) %>% summarise(female_departures=n())
  male_departures <- df1 %>% filter(gender =='male') %>% group_by(end_id) %>% summarise(male_departures=n())
  arrivals_df <- arrivals_df %>% left_join(female_arrivals)
  arrivals_df <- arrivals_df %>% left_join(male_arrivals)
  arrivals_df['arrival_trip_parity'] <- arrivals_df$female_arrivals/arrivals_df$male_arrivals
  departures_df <- departures_df %>% left_join(female_departures)
  departures_df <- departures_df %>% left_join(male_departures)
  departures_df['departure_trip_parity'] <- departures_df$female_departures/departures_df$male_departures
  df2 <- locations %>% left_join(arrivals_df, by=join_by(station_id == start_id))
  df3 <- df2 %>% left_join(departures_df, by=join_by(station_id == end_id))
  return(df3)
}

trips_sorted <- trips_by_station(df, locations_df)
trips_sorted %>% write_csv("./Tables/trips_sorted_276.csv")

#time period
am_sorted <- trips_by_station(am_trips, locations_df)
am_sorted %>% write_csv("./Tables/am_sorted.csv")
midday_sorted <- trips_by_station(midday_trips, locations_df)
midday_sorted %>% write_csv("./Tables/midday_sorted.csv")
pm_sorted <- trips_by_station(pm_trips, locations_df)
pm_sorted %>% write_csv("./Tables/pm_sorted.csv")
evening_sorted <- trips_by_station(evening_trips, locations_df)
evening_sorted %>% write_csv("./Tables/evening_sorted.csv")
night_sorted <- trips_by_station(night_trips, locations_df)
night_sorted %>% write_csv("./Tables/night_sorted.csv")

#day of week
weekday_sorted <- trips_by_station(weekday_trips, locations_df)
weekday_sorted %>% write_csv("./Tables/weekday_sorted.csv")

weekend_sorted <- trips_by_station(weekend_trips, locations_df)
weekend_sorted %>% write_csv("./Tables/weekend_sorted.csv")

#age
genz_sorted <- trips_by_station(genz_trips, locations_df) 
mill_sorted <- trips_by_station(mill_trips, locations_df) 
genx_sorted <- trips_by_station(genx_trips, locations_df) 
boomer_sorted <- trips_by_station(boomer_trips, locations_df) 
silent_sorted <- trips_by_station(silent_trips, locations_df)  

#user type
subscriber_sorted <- trips_by_station(subscriber_trips, locations_df)
subscriber_sorted %>% write_csv("./Tables/subscriber_sorted.csv")
customer_sorted <- trips_by_station(customer_trips, locations_df)
customer_sorted %>% write_csv("./Tables/customer_sorted.csv")
