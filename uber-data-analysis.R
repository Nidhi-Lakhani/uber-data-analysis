#setting color vector 
colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

#importing data
apr_data <- read.csv("uber-raw-data-apr14.csv")
may_data <- read.csv("uber-raw-data-may14.csv")
jun_data <- read.csv("uber-raw-data-jun14.csv")
jul_data <- read.csv("uber-raw-data-jul14.csv")
aug_data <- read.csv("uber-raw-data-aug14.csv")
sep_data <- read.csv("uber-raw-data-sep14.csv")

#combining dataframe
data_2014 <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)

#preprocessing data to get correct date and time
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)

data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))

data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

#plotting trips by hour
hour_data <- data_2014 %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n())
datatable(hour_data)

ggplot(hour_data, aes(hour, Total)) + 
  geom_bar(stat = "identity", fill = "steelblue", color = "red") + 
  ggtitle("Trips every hour") + 
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)

#plotting trips by month and hour
month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarise(Total = n())

ggplot(month_hour, aes(hour, Total, fill = month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Hour and Month") + 
  scale_y_continuous(labels = comma)

#plotting trips by everyday of the month
day_group <- data_2014 %>%
  group_by(day) %>%
  dplyr::summarise(Total = n())
datatable(day_group)

ggplot(day_group, aes(day, Total)) +
  geom_bar(stat = "identity", fill = "orange") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)
  
#plotting trips by Days and Month
day_month_group <- data_2014 %>%
  group_by(month, day) %>%
  dplyr::summarise(Total = n())

ggplot(day_month_group, aes(day, Total, fill = month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

#Number of trips taking place during months in a Year
month_group <- data_2014 %>%
  group_by(month) %>%
  dplyr::summarise(Total = n())
datatable(month_group)
  
ggplot(month_group, aes(month, Total, fill = month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

#plotting by weekday  
month_weekday <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarise(Total = n())

ggplot(month_weekday, aes(month, Total, fill = dayofweek)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

#Number of Trips by bases
ggplot(data_2014, aes(Base)) +
  geom_bar(fill = "darkred") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")

#Trips by bases per month
ggplot(data_2014, aes(Base, fill = month)) +
  geom_bar(position = "dodge") +
  scale_y_continous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colors)

#Trips by weekdays
ggplot(data_2014, aes(Base, fill = dayofweek)) +
  geom_bar(position = 'dodge') +
  scale_y_continous(labels = comma) +
  ggtitle("Trips by Bases and Days of the week") +
  scale_fill_manual(values = colors)

#Creating a Heatmap visualization of day, hour and month
day_and_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarise(Total = n())
datatable(day_and_hour)

ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")

ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day")

ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")

month_base <- data_2014 %>%
  group_by(Base, month) %>%
  dplyr::summarise(Total = n())

dayofweek_bases <- data_2014 %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarise(Total = n())

ggplot(month_base, aes(Base, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Bases")

ggplot(dayofweek_bases, aes(Base, dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")  

#Creating a map visualization of rides in New York
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggplot(data_2014, aes(x=lon, y=lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continous(limits = c(min_long, max_long)) +
  scale_y_continuous(limits = c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR - SEP)")

ggplot(data_2014, aes(x=lon, y=lat, color = Base)) +
  geom_point(size = 1) +
  scale_x_continous(limits = c(min_long, max_long)) +
  scale_y_continuous(limits = c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR - SEP) BY BASE")

#SUMMARY
# made use of ggplot2 that allowed to plot various types of visualizations that pertained to several time-frames of the year
# we conclude how time affected customer trips
# finally, we made a geo plot of New York that provided us with the details of how various users made trips from different bases







