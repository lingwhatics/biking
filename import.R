# Libraries
library(tidyverse)
library(lubridate)

# Get data (exported from the City of Toronto cycling app)
folder <- "Bike_Trips/"
file_list <- list.files(path=folder, pattern = "*.csv")
biketrips <-
  do.call("rbind",
          lapply(file_list,
                 function(x)
                   read.csv(paste0(folder, x), stringsAsFactors = FALSE ))) 
                          
# Parse date-times
biketrips <- biketrips %>% 
  mutate(date_time = ymd_hms(paste(biketrips$Date, biketrips$Time, sep = " ")),
         tz = "America/Toronto")

# Get subset of today's trips only
today_trip <- biketrips %>% filter(Date == Sys.Date())

# Plot all trips in record
# all_trips <- ggplot(biketrips, aes(Longitude, Latitude, colour = Date)) + 
#   geom_point() + coord_map() +
#   scale_colour_viridis(discrete = TRUE)
# all_trips

# Plot trips by year
by_year <- ggplot(biketrips, 
            aes(Longitude, Latitude, colour = year(Date))) +
  geom_point() + 
  guides(colour = guide_legend(title = "Year"))  + 
  coord_map() +
  scale_colour_viridis_c()
by_year
ggsave("all.png", dpi = 300, width = 10, height  = 6)

# Plot showing which parts of trips were in the morning vs. the afternoon
am_pm_trips <- ggplot(biketrips, aes(Longitude, Latitude, colour = am(date_time))) + 
  geom_point() + coord_map() +
  guides(colour = guide_legend(title = "Morning Ride?")) +
  scale_colour_viridis_d()
am_pm_trips

# Plot just today's trip
todays_ride <- ggplot(today_trip, aes(Longitude, Latitude)) + 
  geom_point() + coord_map()
todays_ride

# Plot with all trips in grey in the background and today's trip in colour
focus_today <- ggplot(biketrips, aes(Longitude, Latitude)) + 
  geom_point() + 
  geom_point(data = today_trip, aes(Longitude, Latitude, colour = Date)) + 
  theme(legend.position="none") + 
  coord_map() + 
  scale_colour_viridis_d(direction = -1)
focus_today
ggsave(paste0(format(today(), "%Y-%m-%d"),"_overlay.png"), dpi = 300, width = 8, height  = 6)

# Create summary counts for heatmap
trips_summary <- biketrips %>% mutate(lat_grp = round(Latitude, 3), 
                                      lon_grp = round(Longitude, 3),
                                      yr = as.factor(year(Date))) %>%
  group_by(lat_grp, lon_grp) %>%
  summarize(points = n())
  
# Plot density heatmap of all trips in record and export as PNG
all_trips_heat <- ggplot(trips_summary, aes(lon_grp, lat_grp)) +
  borders() +
  xlim(c(-79.6, -79.25)) + ylim(c(43.5, 43.85)) +
  stat_density_2d(aes(fill = ..level.., alpha = ..level..), geom = "polygon")
all_trips_heat
# ggsave("all_heat.png", dpi = 300, width = 10, height  = 6)

trips_summary <- trips_summary %>% mutate(freq = case_when(
  points < 10 ~ "<10",
  points <20 ~ "11 to 20",
  points <30 ~ "21 to 30",
  points <40 ~ "31 to 40",
  TRUE ~ ">40"
  ),
  freq = factor(freq, levels = c("<10", "11 to 20",
                                   "21 to 30", "31 to 40",
                                   ">40")))

all_trips_dots <- ggplot(trips_summary %>% filter(points < 100), 
                         aes(lon_grp, lat_grp, 
                             colour = points)) +
  scale_colour_viridis() +
  geom_point() + coord_map() #+ theme(legend.position="none")
all_trips_dots

