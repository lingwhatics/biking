# Libraries
library(XML)
library(tidyverse)
library(lubridate)
library(janitor)

# Get data (exported from STRAVA)
folder <- "Bike_Trips/"
file_list <- list.files(path=folder, pattern = "*.gpx")

# https://stackoverflow.com/questions/36377252/import-gpx-track-using-the-xml-library
gpx.raw <- xmlTreeParse(paste0(folder, "Lachine_1.gpx") , useInternalNodes = TRUE)
rootNode <- xmlRoot(gpx.raw)
gpx.rawlist <- xmlToList(rootNode)$trk
gpx.list <- unlist(gpx.rawlist[names(gpx.rawlist) == "trkseg"], recursive = FALSE)
gpx <- do.call(bind_rows, 
               lapply(gpx.list, 
                      function(x) as.data.frame(t(unlist(x)), 
                                                stringsAsFactors=F)))

gpx <- paste0(folder, "Lachine_1.gpx") %>%
  xmlTreeParse(useInternalNodes = TRUE) %>%
  xmlRoot %>%
  xmlToList %>%
  (function(x) x$trk) %>%
  (function(x) unlist(x[names(x) == "trkseg"], recursive = FALSE)) %>%
  map_df(function(x) as.data.frame(t(unlist(x)), stringsAsFactors=FALSE))

names(gpx) <- c("X", "date_time", "latitude", "longitude")
gpx$date_time <- format(ymd_hms(gpx$date_time), tz = "America/Montreal", usetz = TRUE)
gpx$time <- format(as.POSIXct(strptime(gpx$date_time,
                                       "%Y-%m-%d %H:%M",
                                       tz="America/Montreal")),
                   format = "%H:%M:%S")
gpx$date <- format(as.POSIXct(strptime(gpx$date_time,"%Y-%m-%d %H:%M",tz="America/Montreal")) ,format = "%Y-%m-%d")

gpx$tz <- "America/Montreal"
gpx <- gpx %>% select(date, time, latitude, longitude, X, date_time, tz) %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         date_time = as.POSIXct(date_time))

# biketrips <-
#   do.call("rbind",
#           lapply(file_list,
#                  function(x)
#                    read.csv(paste0(folder, x), stringsAsFactors = FALSE ))) 
               
biketrips <- readRDS("bike_trips.rds") %>% clean_names()        

biketrips <- bind_rows(biketrips, gpx)

# Get subset of today's trips only
today_trip <- biketrips %>% filter(date == "2019-06-29") #Sys.Date())

# Plot all trips in record
# all_trips <- ggplot(biketrips, aes(Longitude, Latitude, colour = Date)) + 
#   geom_point() + coord_map() +
#   scale_colour_viridis(discrete = TRUE)
# all_trips

# Plot trips by year
by_year <- ggplot(biketrips, 
            aes(longitude, latitude, colour = year(date))) +
  geom_point() + 
  guides(colour = guide_legend(title = "Year"))  + 
  coord_map() +
  scale_colour_viridis_c()
by_year
ggsave("all.png", dpi = 300, width = 10, height  = 6)

# Plot showing which parts of trips were in the morning vs. the afternoon
am_pm_trips <- ggplot(biketrips, aes(longitude, latitude, colour = am(date_time))) + 
  geom_point() + coord_map() +
  guides(colour = guide_legend(title = "Morning Ride?")) +
  scale_colour_viridis_d()
am_pm_trips

# Plot just today's trip
todays_ride <- ggplot(today_trip, aes(longitude, latitude)) + 
  geom_point() + 
  coord_map()
todays_ride

# Plot with all trips in grey in the background and today's trip in colour
# limit to Montreal
focus_today <- biketrips %>% filter(tz == "America/Montreal") %>%
  ggplot(aes(longitude, latitude)) + 
  geom_point() + 
  geom_point(data = today_trip, aes(longitude, latitude, colour = date)) + 
  theme(legend.position="none") + 
  coord_map() + 
  scale_colour_viridis_d(direction = -1)
focus_today
ggsave(paste0(format(today(), "%Y-%m-%d"),"_overlay.png"), dpi = 300, width = 8, height  = 6)

# Create summary counts for heatmap
trips_summary <- biketrips %>% 
  mutate(lat_grp = round(latitude, 3),
         lon_grp = round(longitude, 3),
         yr = as.factor(year(date))) %>%
  group_by(lat_grp, lon_grp) %>%
  summarize(points = n())
  
# Plot density heatmap of all trips in record and export as PNG
all_trips_heat <- ggplot(trips_summary, aes(lon_grp, lat_grp)) +
  borders() +
  #xlim() + 
  scale_x_continuous(trans = "reverse", limits = c(-73.0, -79.25)) +
  ylim(c(43.5, 45.5)) +
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

