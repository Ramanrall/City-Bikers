install.packages("janitor")  #data cleaning
install.packages("tidyverse")

library(tidyverse) #to wrangle data
library(lubridate) #to wrangle data attributes
library(skimr) #stats
library(janitor)    #data cleaning
library(scales) #visuals
getwd() #displays working directory


#Collect latest Data of last 12 months, 03/2022 - 02/2022
#reading the data from CVS file and store it into new dataframes c1, c2, c3.....

c1 <- read_csv("/Users/ramandeepsingh/CityBikers/202103-divvy-tripdata.csv", show_col_types = FALSE)
c2 <- read_csv("/Users/ramandeepsingh/CityBikers/202104-divvy-tripdata.csv", show_col_types = FALSE)
c3 <- read_csv("/Users/ramandeepsingh/CityBikers/202105-divvy-tripdata.csv", show_col_types = FALSE)
c4 <- read_csv("/Users/ramandeepsingh/CityBikers/202106-divvy-tripdata.csv", show_col_types = FALSE)
c5 <- read_csv("/Users/ramandeepsingh/CityBikers/202107-divvy-tripdata.csv", show_col_types = FALSE)
c6 <- read_csv("/Users/ramandeepsingh/CityBikers/202108-divvy-tripdata.csv", show_col_types = FALSE)
c7 <- read_csv("/Users/ramandeepsingh/CityBikers/202109-divvy-tripdata.csv", show_col_types = FALSE)
c8 <- read_csv("/Users/ramandeepsingh/CityBikers/202110-divvy-tripdata.csv", show_col_types = FALSE)
c9 <- read_csv("/Users/ramandeepsingh/CityBikers/202111-divvy-tripdata.csv", show_col_types = FALSE)
c10 <- read_csv("/Users/ramandeepsingh/CityBikers/202112-divvy-tripdata.csv", show_col_types = FALSE)
c11 <- read_csv("/Users/ramandeepsingh/CityBikers/202201-divvy-tripdata.csv", show_col_types = FALSE)
c12 <- read_csv("/Users/ramandeepsingh/CityBikers/202202-divvy-tripdata.csv", show_col_types = FALSE)

compare_df_cols(c1, c12) 
#check all 12 months of data for consistency with the columns
compare_df_cols_same(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)


#Trying to install skimr library
install.packages("devtools")
devtools::install_github("ropensci/skimr")
library("skimr")

#Combine 12 months of data, into 1 big dataset:
all_trips <- bind_rows(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
#inspect the new dataset
skim_without_charts(all_trips)

#Total unique ride_id = total number of rows, means no duplicates
#3 unique rideable-types, 2 member-types, good data
#station name & id total is different, look LATER1
#station end name & id total is different, look LATER2

#Add a “ride_length” calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)


#ride_length is in character. convert to "numeric" for calculations
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)  #check again

#ride_length has negative values. lets see whats wrong
temp1 <- filter(all_trips, ride_length < 0)
View(temp1)
count(temp1, start_station_name, sort = TRUE)
count(temp1, end_station_name, sort = TRUE)

#DATA CLEANING 
# 1. Negative ride_lenghts rides are invalid (rapairs/lost/testing)
# 2. Rides with less than 10 secs are invalid (swapping or testing)
# 3. NA's station_names but with enging logitude and latitude are valid rides

#Take a look at NA stations
filter(all_trips, is.na(end_station_id)) #~762k results
filter(all_trips, is.na(end_station_name)) #~762k results
filter(all_trips, is.na(start_station_name)) #~713k results
filter(all_trips, is.na(start_station_id)) #~713k results
filter(all_trips, is.na(start_lat)) #zero
filter(all_trips, is.na(start_lng)) #zero
filter(all_trips, is.na(end_lat)) # ~4600 results
filter(all_trips, is.na(end_lng))# ~4600 results

#Analyzing the ride_length where end_lat and end_station_name is missing
temp2 <- filter(all_trips, is.na(end_station_id) & is.na(end_lat)) #4617 rows
skim_without_charts(temp2)

#removing negative ride_length, rides with below 10 sec, with NA end_station_id, with NA end_lat
all_trips_v2 <- all_trips[!(all_trips$ride_length<10),]
all_trips_v3 <- all_trips_v2[!(is.na(all_trips_v2$end_station_id) | is.na(all_trips_v2$start_station_id) | is.na(all_trips_v2$end_lat) | is.na(all_trips_v2$end_lng)),]

#romoving the ride_length with more than 24 Hours  = 86400 sec regarded as stolen/lost. regardless of real rides because of analysis
all_trips_v3 <- all_trips_v3[!(all_trips_v3$ride_length >= 86400),]


#number of rows removed with bad data
nrow(all_trips) - nrow(all_trips_v3)  #1053202 row

# percentage of bad data removed
(nrow(all_trips) - nrow(all_trips_v3)) / nrow(all_trips_v3) * 100  #22.82235 percent


all_trips_v3$date <- as.Date(all_trips_v3$started_at)
View(all_trips_v3)
