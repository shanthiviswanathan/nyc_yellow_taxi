library(tidyverse)
library(lubridate)
trip = read_csv("../data/nyc-taxi/yellow_tripdata_2018-01.csv")
loc = read_csv("../data/nyc-taxi/taxi_zone_lookup.csv")
daylvl = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')
trip["Weekday"] = weekdays(trip$tpep_pickup_datetime)

dim(trip)
head(trip)
cleantrips = trip %>% 
  filter (between(fare_amount,0,150) & trip_distance > 0) %>% 
  inner_join(loc, c("PULocationID" = "LocationID"))  %>% 
  inner_join(loc, c("DOLocationID" = "LocationID")) %>% 
  select(1:18, "StartBoro" = Borough.x, "EndBoro" = Borough.y, "StartZone" = Zone.x, "EndZone" = Zone.y)

summary(cleantrips$fare_amount)
summary(cleantrips$trip_distance)

cleantrips %>% 
  ggplot() + 
  geom_histogram(aes(fare_amount))

cleantrips %>% 
  group_by(PULocationID, Weekday) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

#jfk vs lga #of trips
cleantrips %>% 
  filter(PULocationID %in% c(132,138)) %>% 
  group_by(PULocationID, Weekday) %>% 
  summarize(count=n())  %>% 
  ggplot(aes(x=factor(Weekday,levels=daylvl), y = count, fill=factor(PULocationID))) +
  geom_bar(position="dodge", stat = "identity") +
  labs(x='Weekday', fill = 'StartPoint')

#Boro comparison for pickup
cleantrips %>% 
  filter(!(StartBoro %in% c('EWR', 'Staten Island'))) %>% 
  group_by(StartBoro, Weekday) %>% 
  summarize(count=n())  %>% 
  ggplot(aes(x=factor(Weekday,levels=daylvl), y = count, fill=factor(StartBoro))) +
  geom_bar(position="dodge", stat = "identity") +
  labs(x='Weekday')

#Boro comparison for Drop off
cleantrips %>% 
  filter(!(EndBoro %in% c('EWR', 'Staten Island'))) %>% 
  group_by(EndBoro, Weekday) %>% 
  summarize(count=n())  %>% 
  ggplot(aes(x=factor(Weekday,levels=daylvl), y = count, fill=factor(EndBoro))) +
  geom_bar(position="dodge", stat = "identity") +
  labs(x='Weekday')  

#Within Manhattan which zones have more pickups
cleantrips %>% 
  filter(StartBoro %in% c('Manhattan')) %>% 
  group_by(StartZone) %>% 
  summarize(count=n())  %>% 
  arrange(desc(count)) %>% 
  top_n(20)

#From airports where are the drop off
cleantrips %>% 
  filter(PULocationID %in% c(132,138)) %>% 
  group_by(EndZone) %>% 
  summarize(count=n())  %>% 
  arrange(desc(count)) %>% 
  top_n(20)

#What is the distribution of taxi rides across boroughs
table(cleantrips$StartBoro, cleantrips$EndBoro)

#Pickups by the hour 
cleantrips %>% 
   group_by('hour' = hour(tpep_pickup_datetime)) %>% 
   summarize(count=n())  %>%
   ggplot(aes( x=hour, y = count)) +
   geom_line()

#Pickups by the hour per weekday
cleantrips %>% 
  group_by('hour' = hour(tpep_pickup_datetime), Weekday) %>% 
  summarize(count=n())  %>%
  ggplot(aes( x=hour, y = count, color=factor(Weekday,levels=daylvl))) +
  geom_line()  
