# Libraries
library(tidyverse)
library(lubridate)

# Get data (exported from the City of Toronto cycling app)
folder <- "~/Documents/GIS_data/Bike_Trips/"
file_list <- list.files(path=folder, pattern = "*.csv")
biketrips <-
  do.call("rbind",
          lapply(file_list,
                 function(x)
                   read.csv(paste0(folder, x), stringsAsFactors = FALSE ))) 
                          
# Pasre date-times
biketrips <- biketrips %>% 
  mutate(date_time = ymd_hms(paste(biketrips$Date, biketrips$Time, sep = " ")))

# Get subset of today's trips only
today_trip <- biketrips %>% filter(Date == Sys.Date())

# Plot all trips in record and export as PNG
all_trips <- ggplot(biketrips, aes(Longitude, Latitude, colour = Date)) + geom_point()
all_trips
ggsave("all.png", dpi = 300, width = 10, height  = 6)

# Plot trips by year
by_year <- ggplot(biketrips, 
            aes(Longitude, Latitude, colour = as.factor(year(Date)))) +
  geom_point() + guides(colour = guide_legend(title = "Year"))
by_year

# Plot showing which parts of trips were in the morning vs. the afternoon
am_pm_trips <- ggplot(biketrips, aes(Longitude, Latitude, colour = am(date_time))) + 
  geom_point() + 
  guides(colour = guide_legend(title = "Morning Ride?"))
am_pm_trips

# Plot just today's trip
todays_ride <- ggplot(today_trip, aes(Longitude, Latitude)) + geom_point()
todays_ride

# Plot with all trips in grey in the background and today's trip in colour
focus_today <- ggplot(biketrips, aes(Longitude, Latitude)) + geom_point() + 
  geom_point(data = today_trip, aes(Longitude, Latitude, colour = Date)) + 
  theme(legend.position="none")
focus_today
ggsave("today_overlay.png", dpi = 300, width = 8, height  = 6)
