## install and load some stuff
install.packages("tidyverse")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("readr")

library("tidyverse")
library("dplyr")
library("lubridate")
library("ggplot2")
library("readr")

## figure out where you are
getwd()
#set your directory if needed
setwd()

## import data
## change to reflect directory
q2_2019 <- read_csv("bike_data_csv/2019/Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("bike_data_csv/2019/Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("bike_data_csv/2019/Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("bike_data_csv/2019/Divvy_Trips_2020_Q1.csv")

## lets compare column names
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)

## rename things to fit with 2020. No need to rename 2020. research more later, 
#i'm sure there's a way to import the column names from 2020 and apply it to all the rest, vs renaming each one.
## these column headers have weird characters and need quotes around them. 
(q2_2019 <- rename(q2_2019,
                   ride_id = "01 - Rental Details Rental ID",
                   rideable_type = "01 - Rental Details Bike ID",
                   started_at = "01 - Rental Details Local Start Time",  
                   ended_at = "01 - Rental Details Local End Time",  
                   start_station_name = "03 - Rental Start Station Name", 
                   start_station_id = "03 - Rental Start Station ID",
                   end_station_name = "02 - Rental End Station Name", 
                   end_station_id = "02 - Rental End Station ID",
                   member_casual = "User Type"))

(q3_2019 <- rename(q3_2019,
                   ride_id = trip_id,
                   rideable_type = bikeid,
                   started_at = start_time,  
                   ended_at = end_time,  
                   start_station_name = from_station_name, 
                   start_station_id = from_station_id, 
                   end_station_name = to_station_name, 
                   end_station_id = to_station_id, 
                   member_casual = usertype))

(q4_2019 <- rename(q4_2019,
                   ride_id = trip_id,
                   rideable_type = bikeid,
                   started_at = start_time,
                   ended_at = end_time,  
                   start_station_name = from_station_name, 
                   start_station_id = from_station_id, 
                   end_station_name = to_station_name, 
                   end_station_id = to_station_id, 
                   member_casual = usertype))

#view again, they feel nicer.
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)

## inspect dataframs for inconsistencies.
str(q2_2019)
str(q3_2019)
str(q4_2019)
str(q1_2020)

#ride_id & ridable_type need to convert
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id),
                   rideable_type = as.character(rideable_type))
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id),
                   rideable_type = as.character(rideable_type)) 
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id),
                   rideable_type = as.character(rideable_type)) 

# Combine into one big data frame
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

#this is how to remove uneeded data: 
# Remove lat, long, birthyear, and gender fields.
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender,
            "01 - Rental Details Duration In Seconds Uncapped",
            "05 - Member Details Member Birthday Year",
            "Member Gender",
            "tripduration"))


#look around some: 
head(all_trips)
colnames(all_trips)
#look a lil deeper on a column
table(all_trips$member_casual)

#In the "member_casual" column, there are two names for members 
#("member" and "Subscriber") and two names for casual riders 
#("Customer" and "casual"). We will need to consolidate that from four to 
# two labels. Go with 2020 style
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member",
                                "Customer" = "casual"))

#double check we did the thing.
table(all_trips$member_casual)


# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year, 
# instead of only at the ride level. 
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)


#create new table - remove bad data. Some trips are to HQ or for quality checks. 
#remove those, and remove negative time.
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
View(all_trips_v2)

# get some basic info like min, max, median, mean. 
summary(all_trips_v2$ride_length)

#comparing members to casual users. FUN = function. 
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)


# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#reorder days of the week. 
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week,
                                    levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#average ride time by each day for members vs casual
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)


### Visualize things: 
# number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "How Many Rides?", 
       subtitle = "casual = PAYGO model, and member = SUBSCRIBER model")

# average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Average Duration of Rides, in Seconds.", 
       subtitle = "casual = PAYGO model, and member = SUBSCRIBER model")


## wow brain hurt. i just wanted the  number ot not be in scientific 
# notation on the y axis. much google. big result. 
# they say run this at the beginning, but i found it now so it's here.
# i'll put it at the beginngin in the notebook. 
options(scipen=999)



## 
# member riders take MORE rides during the week by ~3-4x.
# casual riders take WAY longer rides by ~3x
