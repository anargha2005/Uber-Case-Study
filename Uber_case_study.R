#--------------------------------------------------------------------------------------------------------*
# Author : Anargha Biswas
# Brief on Topic:                                                                                        *
# The dataset contains UBER cab booking details to and from the city and the airport            ---------*
# It contain details of the bookings, like if the trip was completed, or was cancelled, or      ---------*
# cabs were not available.                                                                       --------*
# The aim of analysis is to identify the root cause of the problem                              ---------*
# (i.e. cancellation and non-availability of cars) and recommend ways to improve the situation. ---------*
#                                                                                               ---------*
# Steps Followed :                                                                              ---------*
# 1. Data Cleaning: It includes proper formatting of the data, checking for missing parameters  ---------*
# etc.
# 2.Getting a proper understanding of the status of the bookings and trying to find the         ---------*
# reason behind it.
# 3. Trying to find the supply demand gap and how to fix the problem of this gap. 
#--------------------------------------------------------------------------------------------------------*
#--------------------------------------------------------------------------------------------------------*
# Libraries Used : 
#--------------------------------------------------------------------------------------------------------*
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

#--------------------------------------------------------------------------------------------------------*
# Importing input files to R                                                                             *
#--------------------------------------------------------------------------------------------------------*
uber <- read.csv("Uber Request Data.csv")
str(uber)

#--------------------------------------------------------------------------------------------------------*
# Data Cleaning and formatting.
# Request.timestamp and Drop.Timestamp are factors by default, lets change them to proper format.
#--------------------------------------------------------------------------------------------------------*

uber$Request.timestamp <- parse_date_time(uber$Request.timestamp, c("dmy_HMS", "dmy_HM"))
uber$Drop.timestamp <- parse_date_time(uber$Drop.timestamp, c("dmy_HMS", "dmy_HM"))

#3914 NA values in drop.timestamp
length(which(is.na(uber$Drop.timestamp)))

#2650 NA values for driver ID 
length(which(is.na(uber$Driver.id)))

#--------------------------------------------------------------------------------------------------------*
#NA values in drop.timestamp indicate the cancelled and no cars available bookings.
#NA values in driver ID indicate the Cancelled bookings. 
#--------------------------------------------------------------------------------------------------------*
#There is an almost equal number of bookings from city and airport present. 
#--------------------------------------------------------------------------------------------------------*

summary(uber$Pickup.point)

#--------------------------------------------------------------------------------------------------------*
#Fetching the day of week. Data has only weekdays and no weekends. 
#--------------------------------------------------------------------------------------------------------*
uber$day <- weekdays.POSIXt(uber$Request.timestamp)

summary(uber$Status)

#--------------------------------------------------------------------------------------------------------*
#        Cancelled No Cars Available    Trip Completed 
#         1264              2650              2831
#--------------------------------------------------------------------------------------------------------*
# Adding new columns in the data, which will extract more information. The hour of the day, 
# If the route is from airport to the city or the opposite,  
# And Dividing the data into timeslots.
# 5am-9am -- Early Mornings
# 10am - 4pm -- Day Time
# 5pm-9pm -- Late Evenings
# 10pm - 4am -- Night Time
#--------------------------------------------------------------------------------------------------------*
uber$hour <- hour(uber$Request.timestamp)

uber <- mutate(uber, 
               time.of.day = if_else(hour>=5 & hour <=9, "Early Mornings", 
                                     if_else(hour>=17 & hour<=21, "Late Evenings", 
                                             if_else(hour>=10 & hour <=16, "Day Time", "Night Time"))))

uber <- mutate(uber, 
               airport.2.city = if_else(Pickup.point == "Airport", 1, 0))

uber$time.of.day = as.factor(uber$time.of.day)
uber$airport.2.city = as.factor(uber$airport.2.city)

summary(uber$time.of.day)
summary(uber$airport.2.city)
#--------------------------------------------------------------------------------------------------------*
# There is a comparatively more number of bookings during late Evening. 
#--------------------------------------------------------------------------------------------------------*

#--------------------------------------------------------------------------------------------------------*
# Lets look into the overall bookings. I.e How many trips get cancelled, or how many trips get completed
# and at what time of the day. 
# For this analysis we have used a bar plot with position dodge since we need a comparison of bookings
# for different slots of the day, i.e Status vs Timeslots of the day. 

# Aesthetics is chosed as time of the day to get an idea of the bookings on an hourly basis. 
#--------------------------------------------------------------------------------------------------------*

ggplot(uber, aes(x = time.of.day)) + 
  geom_bar(aes(fill = Status), stat = "count",
           position = position_dodge()) + 
  ggtitle("OverAll Bookings throughout the day") +
  labs(y="Demands",x="")
#--------------------------------------------------------------------------------------------------------*
# It is observed that in the wee hours of the day, there is an almost equal number of trips completed
# and cancelled booking, whereas the major problem in the evening is No cab Avaialbility. 
# Now Lets look into the route, I.e How many trips get cancelled, or how many trips get completed
# and at what time of the day and is it from the airport or to the airport. 
#--------------------------------------------------------------------------------------------------------*

ggplot(uber[uber$airport.2.city == 1, ]
       , aes(x = time.of.day)) + 
  geom_bar(aes(fill = Status), stat = "count",
           position = position_dodge()) + 
  ggtitle("Airport to City Bookings") +
  labs(y="Demands",x="")

#--------------------------------------------------------------------------------------------------------*
# From the plot, it is evident that there is a huge demand of cars in the evening from the airport
# to the city. CAncellations are also there, but the No Cars availability reaches its peak during evening.
#--------------------------------------------------------------------------------------------------------*

ggplot(uber[uber$airport.2.city == 0, ]
       , aes(x = time.of.day)) + 
  geom_bar(aes(fill = Status), stat = "count",
           position = position_dodge()) + 
  ggtitle("City to Airport Bookings") +
  labs(y="Demands",x="")

#--------------------------------------------------------------------------------------------------------*
# There is a huge number of cancellations in the early morning, from the city to the airport route.  
#--------------------------------------------------------------------------------------------------------*

#--------------------------------------------------------------------------------------------------------*
# Supply - Demand Analysis

# Demand is defined as sum of Cancelled bookings, No Cabs Available Bookings and Trip Completed Bookings
# Supply is simply the Trips Completed Bookings. 
# Gap is the difference of Demand and Supply. 
#--------------------------------------------------------------------------------------------------------*
df <- aggregate(uber$Status, by = list(Type = uber$time.of.day, availaibility = uber$Status), FUN = length)
df_wide <- spread(df, availaibility, x)
df_wide <- mutate(df_wide, demand = df_wide$Cancelled + df_wide$`No Cars Available` + df_wide$`Trip Completed`, 
                  supply = df_wide$`Trip Completed`)
df_wide <- mutate(df_wide, gap = df_wide$demand - df_wide$supply)

#--------------------------------------------------------------------------------------------------------*
# This gives us an understanding of the availability of cabs grouped by time slots of the day.
# These are plotted in Tableau.  
#--------------------------------------------------------------------------------------------------------*

#--------------------------------------------------------------------------------------------------------*
# Demand supply based on the route, i.e to and from the Airport. 
# We are looking into the demand and supply of cabs when the pickup place is the airport. 
#--------------------------------------------------------------------------------------------------------*

airport_pickup <- uber[uber$Pickup.point == "Airport",]
airport_pickup_df <- aggregate(airport_pickup$Status, 
                               by = list(availability = airport_pickup$Status, 
                                         timeslot = airport_pickup$time.of.day) 
                               ,FUN = length)
airport_pickup_df <- spread(airport_pickup_df, availability, x)
airport_pickup_df <- mutate(airport_pickup_df, demand = airport_pickup_df$Cancelled + 
                              airport_pickup_df$`No Cars Available` + airport_pickup_df$`Trip Completed`, 
                            supply = airport_pickup_df$`Trip Completed`)
airport_pickup_df <- mutate(airport_pickup_df, gap = airport_pickup_df$demand - airport_pickup_df$supply)

#--------------------------------------------------------------------------------------------------------*
# Demand supply based on the route, i.e to and from the Airport. 
# Next, lets have a look into the demand and supply of cabs when the pickup place is City. 
#--------------------------------------------------------------------------------------------------------*

city_pickup <- uber[uber$Pickup.point == "City",]
city_pickup_df <- aggregate(city_pickup$Status, 
                            by = list(availability = city_pickup$Status,
                                      timeslot = city_pickup$time.of.day), FUN = length)
city_pickup_df <- spread(city_pickup_df, availability, x)
city_pickup_df <- mutate(city_pickup_df, demand = city_pickup_df$Cancelled + 
                           city_pickup_df$`No Cars Available` + city_pickup_df$`Trip Completed`, 
                         supply = city_pickup_df$`Trip Completed`)
city_pickup_df <- mutate(city_pickup_df, gap = city_pickup_df$demand - city_pickup_df$supply)

#--------------------------------------------------------------------------------------------------------*
# We even did a plot of hourly basis to find exactly which hours of the day, there are problems of 
# no cab availability and Cancelled bookings. And for which route the problem occurs. 
# i.e if it is from the airport to the city, or from the city to the airport. 
#--------------------------------------------------------------------------------------------------------*

No_cars <- uber[uber$Status == "No Cars Available", ]
View(No_cars)

summary(as.factor(No_cars$hour))
ggplot(No_cars, aes(x = hour)) + geom_bar(colour = "green") + 
  ggtitle("No Cars Availability") +
  labs(y="Count of No Cars Availability Bookings",x="Time of day")


#--------------------------------------------------------------------------------------------------------*
#Observation : The most number of no cars available situation happens 
#from evening 5pm to 10pm (Bar plot)
#--------------------------------------------------------------------------------------------------------*

No_cars$day <- as.factor(No_cars$day)
summary(No_cars$day)
#--------------------------------------------------------------------------------------------------------*
# Friday    Monday  Thursday   Tuesday Wednesday 
# 580       504       571       505       490 

# Observation : This simply means that irrespective of the day, the number of no cars availability 
# booking is more or less the same. 
#--------------------------------------------------------------------------------------------------------*
summary(No_cars$time.of.day)

#--------------------------------------------------------------------------------------------------------*
# Day Time Early Mornings  Late Evenings     Night Time 
# 334            406           1392            518
#--------------------------------------------------------------------------------------------------------*

summary(No_cars$airport.2.city)

ggplot(No_cars[No_cars$airport.2.city == 1, ]
       , aes(x = as.factor(hour))) + geom_histogram(stat = "count", colour = "green") + 
  ggtitle("Airport to City No Cars Availability") +
  labs(y="Count of No Cars Availability",x="Time of day")

#--------------------------------------------------------------------------------------------------------*
# This implies that people travelling from airport to city face the max problem of no cars available
#between 5pm to 9pm.

#--------------------------------------------------------------------------------------------------------*

#Thats all for No Car Availability. Lets find some insights about the problem of Cancelled Cars.
#--------------------------------------------------------------------------------------------------------*
Cancelled_cars <- uber[uber$Status == "Cancelled", ]
View(Cancelled_cars)

#--------------------------------------------------------------------------------------------------------*
# Observation : Most cancellation of cars happen between 5 am to 9am.
# Lets see now the most problematic type of requests.
#--------------------------------------------------------------------------------------------------------*

summary(Cancelled_cars$airport.2.city)
summary(Cancelled_cars$time.of.day)

#--------------------------------------------------------------------------------------------------------*
#Day Time Early Mornings  Late Evenings     Night Time 
#168            843            166             87
#City to airport Cancellation is at its max between 5am to 9am.
#--------------------------------------------------------------------------------------------------------*

ggplot(Cancelled_cars[Cancelled_cars$airport.2.city == 0, ]
       , aes(x = as.factor(hour))) + geom_histogram(stat = "count", colour = "green") + 
  ggtitle("City  to Airport Cancellations") +
  labs(y="Count of Cancellations",x="Time of day")
