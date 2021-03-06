# Libraries
library(XML)
library(tidyverse)
library(lubridate)
library(janitor)
library(ggthemes)

# Get data (exported from STRAVA)
folder <- "Bike_Trips/"
file_list <- list.files(path=folder, pattern = "*.gpx")

# define function for parsing GPX inputs
parse_GPX <- function(filename) {
  filename %>%
  xmlTreeParse(useInternalNodes = TRUE) %>%
  xmlRoot %>%
  xmlToList %>%
  (function(x) x$trk) %>%
  (function(x) unlist(x[names(x) == "trkseg"], recursive = FALSE)) %>%
  map_df(function(x) as.data.frame(t(unlist(x)), stringsAsFactors=FALSE))
}

gpx <- map_df(paste0(folder, file_list), parse_GPX)

names(gpx) <- c("X", "date_time", "latitude", "longitude")
gpx$date_time <- format(ymd_hms(gpx$date_time), 
                        tz = "America/Montreal", usetz = TRUE)
gpx$time <- format(as.POSIXct(strptime(gpx$date_time,
                                       "%Y-%m-%d %H:%M",
                                       tz="America/Montreal")),
                   format = "%H:%M:%S")
gpx$date <- format(as.POSIXct(strptime(gpx$date_time,"%Y-%m-%d %H:%M",
                                       tz="America/Montreal")),
                   format = "%Y-%m-%d")

gpx$tz <- "America/Montreal"
gpx <- gpx %>% 
  select(date, time, latitude, longitude, X, date_time, tz) %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         date_time = as.POSIXct(date_time))

biketrips <- gpx

# write out updated data
write_rds(biketrips, "trips.rds")

# reduce number of data points where they only add clutter
biketrips2 <- biketrips %>%
  mutate(reduce = case_when(
    date == lag(date) & round(latitude, 4) == lag(round(latitude, 4)) & round(longitude, 4) == lag(round(longitude, 4)) ~ NA_real_,
    TRUE ~ 1
  )) %>%
  drop_na(reduce)

# Get subset of today's trips only
biketrips2 <- biketrips2 %>% 
  mutate(today = as.factor(if_else(date == Sys.Date(), 1, 0)))
  #mutate(today = as.factor(if_else(date == "2020-06-17", 1, 0)))

# Plot just today's trip
todays_ride <- biketrips2 %>%
  filter(today == 1) %>%
  ggplot(aes(longitude, latitude)) + 
  geom_point() + 
  coord_map()
todays_ride

# Plot with all trips in grey in the background and today's trip in colour
# limit to Montreal
focus_today <- biketrips2 %>% 
  filter(tz == "America/Montreal") %>%
  ggplot(aes(longitude, latitude, colour = today)) + 
  geom_point(size = 0.7) + 
  #geom_point(data = today_trip, 
  #           aes(longitude, latitude, colour = date), 
  #           size = 0.7) + 
  theme_void() +
  theme(legend.position="none") + 
  coord_map() + 
  ggthemes::scale_color_colorblind() +
  NULL

focus_today
ggsave(paste0(format(today(), "%Y-%m-%d"),"_overlay.png"), 
       dpi = 300, width = 8, height  = 6)

write_rds(biketrips, "trips.rds")
