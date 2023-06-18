## READ ME FILE

## THIS IS CODE FOR CITYBIKERS DATA ANALYSIS
## OBSERVATIONS + CONCLUSIONS + RECOMENDATIONS


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


all_trips_v3$date <- as.Date(all_trips_v3$started_at) #add date coloum to all_trips_v3
all_trips_v3$month <- format(as.Date(all_trips_v3$date), "%m") #add month coloum to all_trips_v3
all_trips_v3$day <- format(as.Date(all_trips_v3$date), "%d")   #add day coloum to all_trips_v3
all_trips_v3$year <- format(as.Date(all_trips_v3$date), "%Y")  #add year coloum to all_trips_v3
all_trips_v3$day_of_week <- format(as.Date(all_trips_v3$date), "%A")  #add day of week column in all_trips_v3

#compare members and casual users by MEAN, MEDIAN, MAX and MIN
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = mean)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = median)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = max)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = min)

#Checking average ride time (mean) by each day for members and casual users
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)

#arrange the week of days in order
all_trips_v3$day_of_week <- ordered(all_trips_v3$day_of_week, levels=
c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))


all_trips_v3 %>%
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)

View(all_trips_v2)

#VISUALIZE THE NUMBER OF RIDES (number_of_rides) BY RIDE TYPE (member_casual)
all_trips_v3 %>%
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x= weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge")


#VISUALIZE THE AVERAGE DURATION (average_duration) BY RIDE TYPE (member_casual)
all_trips_v3 %>%
  mutate(weekday = wday(started_at, label=TRUE)) %>%
  group_by(member_casual,weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x= weekday, y=average_duration, fill=member_casual)) +
  geom_col(position = "dodge")


#OBSERVATIONS:
#1. CASUAL riders have longer rides ON AVERAGE than MEMBERS (Casual:1691.1944, Member:785.3618)
#2. The ride length of CASUAL riders (in seconds) is more than MEMBERS. (C:86362, M:85594)
#3. Minimun ride length of both riders casual and members is same. 10 each
#4  The number of rides for MEMBER riders is more as compare to CASUAL. BUT totally opposite on saturday and sunday.

#calculate TOTAL number of rides by each member group
all_trips_v3 %>%
  group_by(member_casual) %>%
  summarise(total_number_of_rides=n(), .groups = 'drop')
#5  The Total number of riders for MEMBER riders is 2567534 and for CASUAL is 2047250.


#CONCLUSION
#There is no significant difference in how casual and member riders use the bikes.
#However members tend to use the bikes often as compared to casual riders. 
#If we are able to convert the existing casual riders to members it will add 
#to companies success.

#RECOMENDATIONS
#Recommendations on how to convert existing casual riders to membership 
#1. Offer discount on annual memberships. 
#2. Offer exclusive member perks like access to all types of bikes, can including some premium bikes in the list. 
#3. Besides annual membership offer other membership plans with flexible time period, may be something like a quarterly plan.
