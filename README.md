Introduction Project Overview Bike-sharing systems have become an increasingly popular mode of sustainable transportation in cities around the world. These systems allow users to rent bikes for short trips, promoting eco-friendly travel and reducing urban congestion. In this project, I explore and analyze bike-sharing data to uncover patterns in bike usage and gain insights into how different factors, such as time of day, weather, and seasonality, affect ridership. This analysis aims to provide data-driven insights that can help improve bike-sharing operations and potentially guide decisions in urban planning and resource allocation.

Objectives The main goals of this project are to:

Analyze Patterns in Bike Usage: Identify trends in daily and hourly bike rentals, and understand how usage varies by day of the week, time of day, and across different seasons. Understand External Factors Affecting Demand: Examine the impact of factors like weather, temperature, and humidity on bike usage. Forecast Future Demand: Build and evaluate time series models to predict future demand for bike rentals, enabling better planning and resource management. Dataset This analysis uses a publicly available dataset that contains information on bike rentals, including timestamped records of the number of bikes rented, as well as weather and temperature conditions. Each data point includes fields such as:

Datetime: The date and time of the rental record. Count: The number of bikes rented in a specific time interval. Weather Conditions: Indicators for temperature, humidity, and general weather conditions (e.g., clear, cloudy, rainy). Holiday & Working Day Flags: Whether the date is a holiday or a regular working day. Analysis Approach This project employs a combination of data visualization, time series analysis, and forecasting techniques. The analysis is structured as follows:

Exploratory Data Analysis (EDA): We begin with EDA to visualize and understand the distribution and trends in bike rentals. This includes examining hourly and daily trends, as well as visualizing the effects of weather and other factors on bike demand. Feature Engineering: We create additional features, such as day of the week, month, and seasonal indicators, to capture patterns in the data. Time Series Forecasting: Using time series modeling techniques, we develop and evaluate models to forecast bike rental demand. These models help predict usage patterns, which is valuable for bike availability management. Key Insights and Potential Applications This analysis provides actionable insights that can help bike-sharing companies and city planners optimize bike availability, identify peak usage times, and allocate resources more effectively. Moreover, the forecasting model can be used as a planning tool to meet future demand, especially during peak hours and seasons.

Data Preparation I start by installing and loading necessary libraries like tidyverse, DataExplorer, janitor, and lubridate. The working directory is set to load data for each month from October 2023 to September 2024. I load each month’s CSV data into separate data frames (bs1 to bs12) and check their structure with glimpse().
Data Merging and Cleaning I merge the data for the 12 months into one large dataset (divvydata) and remove empty rows and columns using the janitor::remove_empty() function. A new date column is created from the started_at column, and additional columns like month, day, year, weekday, start_hour, end_hour, and season are added. The ride_length column is calculated by finding the difference between ended_at and started_at. You convert this into a numeric format.
Data Filtering I clean the dataset by removing rows with missing values or negative ride lengths and exclude rows where start_station_name contains invalid values (e.g., "HQ QR", "CHECK", etc.).
Descriptive Statistics Summary statistics are calculated for ride_length by user type (member_casual) using aggregate(). I summarize the data to explore user type distribution, ride lengths, and ride counts for various time periods and categories.
Trends Analysis Weekday Trends:
I calculate the average ride duration and count of rides by weekday and user type (casual vs. member). A bar plot visualizes the daily ridership by user type, with colors representing the user type. Ride Type Trends:

The dataset is grouped by rideable_type (e.g., bike type) and member_casual to compare casual vs. member usage of different ride types (like "classic bike", "electric bike", etc.). A bar plot visualizes the distribution of ride types across user types. Seasonal Trends:

Seasonal ride distribution is explored, and the average ride duration is calculated for each season (Spring, Summer, Fall, and Winter) by user type. A bar plot visualizes seasonal ridership trends for each user type. Monthly Trends:

Monthly ridership trends are displayed, broken down by user type, with separate visualizations for total rides and average duration. Hourly Trends:

The code then calculates and visualizes the popular start and end hours for each user type (casual vs. member). Bar plots are created for the top start and end hours, visualizing how users of different types use bikes at different times of the day. Station Trends:

The most popular start and end stations are identified, and visualized separately for casual vs. member users. The top 10 start and end stations are visualized using bar plots to show how ridership is distributed across the city. Summary of Key Visualizations: Weekday Trends: Show the distribution of rides across the days of the week, highlighting which days are most popular for casual riders vs. members. Ride Type Trends: Compare usage of different bike types between casual and member riders. Seasonal Trends: Analyze how seasonality impacts ridership, including average duration. Monthly Trends: Show changes in ride counts and ride durations month-to-month. Hourly Trends: Examine patterns in ride start and end times, providing insights into peak usage hours. Station Trends: Identify the most popular bike stations, both for starting and ending rides.


# data preparation
#. instal.packages

install.packages("tidyverse")
library(tidyverse)
options(dplyr.summarise.inform = FALSE)
install.packages("DataExplorer")
library(DataExplorer)
install.packages("janitor")
library(janitor)
install.packages("lubridate")
library(lubridate)

#.Set working directory
setwd("C:/Users/ZiiN 008/OneDrive - ZiiN Kft/Asztal/Mira/Coursera/1. Capestone/Bike Sharing/All .csv")

#.Load 12 months of data 2023.10-2024.09
bs1<-read.csv("divvytripdata202310.csv")
bs2<-read.csv("divvytripdata202311.csv")
bs3<-read.csv("divvytripdata202312.csv")
bs4<-read.csv("divvytripdata202401.csv")
bs5<-read.csv("divvytripdata202402.csv")
bs6<-read.csv("divvytripdata202403.csv")
bs7<-read.csv("divvytripdata202404.csv")
bs8<-read.csv("divvytripdata202405.csv")
bs9<-read.csv("divvytripdata202406.csv")
bs10<-read.csv("divvytripdata202407.csv")
bs11<-read.csv("divvytripdata202408.csv")
bs12<-read.csv("divvytripdata202409.csv")

#. View data
glimpse(bs1)
glimpse(bs2)
glimpse(bs3)
glimpse(bs4)
glimpse(bs5)
glimpse(bs6)
glimpse(bs7)
glimpse(bs8)
glimpse(bs9)
glimpse(bs10)
glimpse(bs11)
glimpse(bs12)

#Ensure Column Consistency
str(bs1)
str(bs2)
str(bs4)
str(bs5)
str(bs6)
str(bs7)
str(bs8)
str(bs9)
str(bs10)
str(bs11)
str(bs12)

#. Merge the Dataframes
divvydata <- rbind(bs1,bs2,bs3,bs4,bs5,bs6,bs7,bs8,bs9,bs10,bs11,bs12)
create_report(divvydata)

# Remove empty columns and rows
divvydata <- janitor::remove_empty(divvydata, which = c("cols"))
divvydata <- janitor::remove_empty(divvydata, which = c("rows"))

# Create columns as follows: date, month, day, year, weekday, start_hour, end_hour, and season
divvydata$date <- as.Date(divvydata$started_at) 
divvydata$month <- format(as.Date(divvydata$date), "%m")
divvydata$day <- format(as.Date(divvydata$date), "%d")
divvydata$year <- format(as.Date(divvydata$date), "%Y")
divvydata$weekday <- format(as.Date(divvydata$date), "%A")
divvydata$start_hour = format(as.POSIXct(divvydata$started_at), "%H")
divvydata$end_hour = format(as.POSIXct(divvydata$ended_at), "%H")
divvydata$season <- ifelse (divvydata$month %in% c('06','07','08'), "Summer",
                            ifelse (divvydata$month %in% c('09','10','11'), "Fall",
                                    ifelse (divvydata$month %in% c('12','01','02'), "Winter",
                                            ifelse (divvydata$month %in% c('03','04','05'), "Spring", NA))))

#Create ride_length Column
divvydata$started_at <- as.POSIXct(divvydata$started_at, format="%Y-%m-%d %H:%M:%S")
divvydata$ended_at <- as.POSIXct(divvydata$ended_at, format="%Y-%m-%d %H:%M:%S")
divvydata$ride_length <- difftime(divvydata$ended_at, divvydata$started_at, units = "mins")

# Check the data type for ride_length and change it to numeric
is.factor(divvydata$ride_length) 
divvydata$ride_length <- as.numeric(as.character(divvydata$ride_length)) 
is.numeric(divvydata$ride_length)

# Data frame dimensions
dim(divvydata)

# Data frame summary
glimpse(divvydata)
summary(divvydata)

# count() returns unique values of the variable passed
divvydata %>% 
  count(start_station_name)

# omitting NA values in the entire dataframe
new_divvydata <- na.omit(divvydata)

# Omit NAs, negative trip lengths, and maintenance checks
divvydata <- na.omit(divvydata)
divvydata_v2 <- divvydata[!(divvydata$start_station_name == "HQ QR"| divvydata$start_station_name == "CHECK" | divvydata$start_station_name == "TEST" | divvydata$start_station_name == "DIVVY" |
                              divvydata$start_station_name == "" |
                              divvydata$ride_length < 0),]

# Summary statistics for ride_length
summary(divvydata_v2$ride_length)

# Summary statistics for member and casual usage
aggregate(divvydata_v2$ride_length ~ divvydata_v2$member_casual, FUN = mean)
aggregate(divvydata_v2$ride_length ~ divvydata_v2$member_casual, FUN = median)
aggregate(divvydata_v2$ride_length ~ divvydata_v2$member_casual, FUN = max)
aggregate(divvydata_v2$ride_length ~ divvydata_v2$member_casual, FUN = min)

# Set levels for weekdays

divvydata_v2$weekday <- ordered(divvydata_v2$weekday, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Weekday Trends
# Average duration by user type and weekday
divvydata_v2 %>%
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n() 
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)

# Ridership by weekday and user type
divvydata_v2$weekday<- ordered(divvydata_v2$weekday, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

ggplot(divvydata_v2, aes(x = weekday, fill = member_casual)) +
  geom_bar(position = "dodge") +
  ggtitle('Daily Ridership by User Type', subtitle = "October 2023 - September 2024") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('Weekday') + ylab('Ride Count') + 
  labs(fill='User Type') +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"),
                    labels = c("casual","member"))

# Average ride duration by user type and weekday
divvydata_v2 %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  ggtitle('Average Ride Duration by User Type and Weekday', subtitle = "October 2023 - September 2024") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) +	
  xlab('Weekday') + ylab('Ride Duration (sec)') + 
  labs(fill='User Type') +
  labs(caption = "NOTE: 1000 sec = 16.6667 min") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"),
                    labels = c("casual","member"))

# Ride Type Trends
# Count of ride type by user type

divvydata_v2 %>% count(rideable_type, member_casual)

ggplot(divvydata_v2, aes(x = rideable_type, fill = member_casual)) + 
  geom_bar(position = "dodge") +
  ggtitle('Ride Type by User Type', subtitle = "October 2023 - September 2024") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('Ride Type') + 	ylab('Ride Count') + 
  labs(fill='User Type') +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"),
                    labels = c("casual","member"))

# Seasonal Trends
# Seasonal trends by user type
divvydata_v2 %>% count(season, member_casual)
ggplot(divvydata_v2, aes(x = season, fill = member_casual)) +
  geom_bar(position = "dodge") +
  ggtitle('Seasonal Trends by User Type', subtitle = "October 2023 - September 2024") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('Season') + 	ylab('Ride Count') + 
  labs(fill='User Type') +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"),
                    labels = c("casual","member"))

# Average ride duration by user type and season
seasonal_avg_duration <- divvydata_v2 %>%
  group_by(member_casual, season) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(member_casual, season) 

print(seasonal_avg_duration)

ggplot(seasonal_avg_duration, aes(x = season, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  ggtitle('Average Ride Duration by User Type and Season', subtitle = "October 2023 - September 2024") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) + 
  xlab('Season') + ylab('Ride Duration (sec)') + 
  labs(fill='User Type') +
  labs(caption = "NOTE: 1000 sec = 16.6667 min") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"),
                    labels = c("casual","member"))

# Monthly trends by user type
monthly_usercount <- divvydata_v2 %>% count(month, member_casual)

 print(monthly_usercount, n=24)

ggplot(divvydata_v2, aes(x = month, fill = member_casual)) +
  geom_bar(position = "dodge") +
  ggtitle('Monthly Trends by User Type', subtitle = "October 2023 - September 2024") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) +	
  xlab('Month') + 	ylab('Ride Count') + 
  labs(fill='User Type') +
  labs(caption = "NOTE: Months represented in MM format") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"),
                    labels = c("casual","member"))

# Average ride duration by user type and month
avg_duration <- divvydata_v2 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(member_casual, month) 

print(avg_duration, n=24)

ggplot(avg_duration, aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + ggtitle('Average Ride Duration by User Type and Month', subtitle = "October 2023 - September 2024") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) + xlab('Month') + ylab('Ride Duration (sec)') + labs(fill='User Type') + labs(caption = "NOTE: 1000 sec = 16.6667 min | Months represented in MM format") + scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"),
                    labels = c("casual","member"))

# Hourly Trends
# Popular start hours by user type
pop_start_hour <- divvydata_v2 %>% count(start_hour, member_casual, sort = TRUE)

casual_start_hour <- filter(pop_start_hour, member_casual == 'casual')

casual_start_hour <- casual_start_hour %>% 
  arrange(desc(n)) %>% 
  slice_head(n=10)

print(casual_start_hour)

member_start_hour <- filter(pop_start_hour, member_casual == 'member')

member_start_hour <- member_start_hour %>%
  arrange(desc(n)) %>% 
  slice_head(n=10)

print(member_start_hour)

# Start hour trends by user type
ggplot(divvydata_v2, aes(x = start_hour, fill = member_casual)) +
  geom_bar(position = "dodge") + 
  ggtitle('Start Hour Trends by User Type', subtitle = "October 2023 - September 2024") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) +	
  xlab('Start Hour (Military Time)') + 	ylab('Ride Count') + 
  labs(fill='User Type') +
  labs(caption = 'NOTE: 0000 / 2400 = 12 a.m.') +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"),
                    labels = c("casual","member"))

# Popular start hours - Casuals
ggplot(casual_start_hour, aes(x = start_hour, y = n)) + 
  geom_bar(stat = "identity", fill="#99cad5", colour="black") +
  ggtitle('Top 10 Start Hours - Casuals', subtitle = "October 2023 - September 2024") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('Start Hour (Military Time)') + ylab('Ride Count') +
  scale_y_continuous(labels = scales::comma)

# Popular start hours - Members
ggplot(data = member_start_hour, aes(x = start_hour, y = n)) + 
  geom_bar(stat = "identity", fill="#3f93a2", colour="black") +
  ggtitle('Top 10 Start Hours - Members', subtitle = "October 2023 - March 2024") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('Start Hour (Military Time)') + ylab('Ride Count') +
  scale_y_continuous(labels = scales::comma) 

# Popular end hours by user type
pop_end_hour <- divvydata_v2 %>% count(end_hour, member_casual, sort = TRUE) 

print(pop_end_hour, n=48)

member_end_hour <- filter(pop_end_hour, member_casual == 'member', sort = TRUE) 
member_end_hour <- member_end_hour %>%
  arrange(desc(n)) %>% 
  slice_head(n=10)

print(member_end_hour)

casual_end_hour <- filter(pop_end_hour, member_casual == 'casual', sort = TRUE) 
casual_end_hour <- casual_end_hour %>%
  arrange(desc(n)) %>% 
  slice_head(n=10)

print(casual_end_hour)

# End hour trends by user type
ggplot(divvydata_v2, aes(x = end_hour, fill = member_casual)) +
  geom_bar(position = "dodge") + 
  ggtitle('End Hour Trends by User Type', subtitle = "October 2023 - September 2024") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5)) +	
  xlab('End Hour (Military Time)') + 	ylab('Ride Count') + 
  labs(fill='User Type') +
  labs(caption = 'NOTE: 0000 / 2400 = 12 a.m.') +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"),
                    labels = c("casual","member"))

# Popular end hours - Casuals
ggplot(casual_end_hour, aes(x = end_hour, y = n)) + 
  geom_bar(stat = "identity", fill="#99cad5", colour="black") +
  ggtitle('Top 10 End Hours - Casuals', subtitle = "October 2023 - September 2024") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('End Hour (Military Time)') + ylab('Ride Count') +
  scale_y_continuous(labels = scales::comma) 

# Popular end hours - Members
ggplot(data = member_end_hour, aes(x = end_hour, y = n)) + 
  geom_bar(stat = "identity", fill="#3f93a2", colour="black") +
  ggtitle('Top 10 End Hours - Members', subtitle = "October 2023 - September 2024") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('End Hour (Military Time)') + ylab('Ride Count') +
  scale_y_continuous(labels = scales::comma) 

# Station Trends
# Popular start stations by user type
popular_stations <- divvydata_v2 %>% count(start_station_name, member_casual)

print(popular_stations)

# Popular end stations by user type
end_stations <- divvydata_v2 %>% count(end_station_name, member_casual)

print(end_stations)

# Top 10 start stations - Casuals
pop_stations_casual<- filter(popular_stations, member_casual == 'casual')

pop_stations_casual <- pop_stations_casual %>% 
  arrange(desc(n)) %>% 
  slice_head(n=10)

print(pop_stations_casual)

ggplot(data = pop_stations_casual, aes(x = start_station_name, y = n)) + 
  geom_bar(stat = "identity", fill="#99cad5", colour="black") +
  ggtitle('Top 10 Start Stations - Casuals', subtitle = "October 2023 - September 2024") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('Station Name') + ylab('Ride Count') + 
  coord_flip( )

# Top 10 start stations - Members
pop_stations_member<- filter(popular_stations, member_casual == 'member')

pop_stations_member <- pop_stations_member %>% 
  arrange(desc(n)) %>% 
  slice_head(n=10)

print(pop_stations_member)

ggplot(data = pop_stations_member, aes(x = start_station_name, y = n)) + 
  geom_bar(stat = "identity", fill="#3f93a2", colour="black") +
  ggtitle('Top 10 Start Stations - Members', subtitle = "October 2023 - September 2024") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('Station Name') + ylab('Ride Count') + 
  coord_flip( )

# Top 10 end stations - Casuals 
end_stations_casual<- filter(end_stations, member_casual == 'casual') 

end_stations_casual <- end_stations_casual %>% 
  arrange(desc(n)) %>% 
  slice_head(n=10)

print(end_stations_casual)

ggplot(data = end_stations_casual, aes(x = end_station_name, y = n)) + 
  geom_bar(stat = "identity", fill="#99cad5", colour="black") +
  ggtitle('Top 10 End Stations - Casuals', subtitle = "October 2023 - September 2024") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('Station Name') + ylab('Ride Count') + 
  coord_flip( )

# Top 10 end stations - Members 
end_stations_member <- filter(end_stations, member_casual == 'member')

end_stations_member <- end_stations_member %>%
  arrange(desc(n)) %>% 
  slice_head(n=10)

print(end_stations_member)

ggplot(data = end_stations_member, aes(x = end_station_name, y = n)) + 
  geom_bar(stat = "identity", fill="#3f93a2", colour="black") +
  ggtitle('Top 10 End Stations - Members', subtitle = "October 2023 - September 2024") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +	
  xlab('Station Name') + ylab('Ride Count') + 
  coord_flip( )

#Heatmap of Daily Usage Patterns:
library(ggplot2)

divvydata_v2 %>%
  mutate(start_hour = as.numeric(start_hour)) %>%
  group_by(weekday, start_hour) %>%
  summarise(ride_count = n()) %>%
  ggplot(aes(x = start_hour, y = reorder(weekday, -start_hour), fill = ride_count)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Heatmap of Bike Usage by Hour and Weekday", x = "Start Hour", y = "Weekday", fill = "Ride Count") +
  theme_minimal()

#Total Rides per Season

seasonal_rides <- divvydata_v2 %>%
  group_by(season, member_casual) %>%
  summarise(total_rides = n(), .groups = "drop")
ggplot(seasonal_rides, aes(x = season, y = total_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  ggtitle('Total Rides per Season by User Type', subtitle = "October 2023 - September 2024") +
  xlab('Season') + ylab('Total Rides') +
  labs(fill = 'User Type') +
  scale_fill_manual(values = c("#99cad5", "#3f93a2"), labels = c("Casual", "Member")) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

#Monthly Ride Growth or Decline Rate
monthly_growth <- divvydata_v2 %>%
  count(month) %>%
  mutate(growth_rate = (n - lag(n)) / lag(n) * 100) %>%
  ggplot(aes(x = month, y = growth_rate)) +
  geom_line(color = "blue") +
  geom_point(size = 2) +
  labs(title = "Monthly Ride Growth Rate", x = "Month", y = "Growth Rate (%)") +
  theme_minimal()

#Map of Popular Routes (if location data available)
# Assuming start_lat, start_lng, end_lat, end_lng columns exist
install.packages("leaflet")
library(leaflet)

# Assuming you have a `stations` dataframe with columns:
# station_name, latitude, and longitude

# Join latitude and longitude for start stations
popular_routes <- popular_routes %>%
  left_join(stations %>% select(station_name, latitude, longitude), 
            by = c("start_station_name" = "station_name")) %>%
  rename(start_lat = latitude, start_lng = longitude)

# Join latitude and longitude for end stations
popular_routes <- popular_routes %>%
  left_join(stations %>% select(station_name, latitude, longitude), 
            by = c("end_station_name" = "station_name")) %>%
  rename(end_lat = latitude, end_lng = longitude)
