# Libraries
library(XML)
library(tidyverse)
library(lubridate)
library(janitor)
library(ggthemes)

# define function for parsing GPX inputs
parse_GPX <- function(filename) {
  filename %>%
    xmlTreeParse(useInternalNodes = TRUE) %>%
    xmlRoot %>%
    xmlToList %>%
    (function(x) x$trk) %>%
    (function(x) unlist(x[names(x) == "trkseg"], recursive = FALSE)) %>%
    map_df(function(x) as.data.frame(t(unlist(x)), stringsAsFactors=FALSE)) %>%
    mutate(file = filename)
}

# Load existing data
biketrips <- read_rds("trips.rds")
# List files already in data
existing_files <- unique(biketrips$file)

# Get data (exported from STRAVA)
folder <- "Bike_Trips/"
file_list <- list.files(path=folder, pattern = "*.gpx")

# keep only new files in folder for import
new_list <- setdiff(file_list, existing_files)

# import files and include filename as id column
gpx <- map_df(paste0(folder, new_list), parse_GPX) %>%
  mutate(file = str_remove(file, folder))

gpx <- gpx %>% select(1:2,4:6,3)

names(gpx) <- c("X", "date_time", "latitude", "longitude", "file", "heart_rate")

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
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         date_time = as.POSIXct(date_time))

biketrips <- bind_rows(biketrips, gpx)

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
  #mutate(today = as.factor(if_else(date == "2020-08-11", 1, 0)))

# Plot just today's trip
todays_ride_path <- biketrips2 %>%
  filter(today == 1) %>%
  ggplot(aes(longitude, latitude)) + 
  geom_path() + 
  coord_map()
todays_ride_path

limits <- biketrips2 %>%
  filter(today == 1) %>%
  summarise(min_lon = min(longitude),
            max_lon = max(longitude),
            min_lat = min(latitude),
            max_lat = max(latitude)) %>%
  mutate_all(~ signif(.,digits = 5))

# Plot with all trips in grey in the background and today's trip in colour
# limit to Montreal
focus_today_path <- biketrips2 %>% 
  filter(tz == "America/Montreal") %>%
  ggplot(aes(longitude, latitude, colour = today, group=file)) + 
  geom_path() + 
  theme_void() +
  theme(legend.position="none") + 
  coord_map() + 
  ggthemes::scale_color_colorblind() +
  NULL

focus_today_path
ggsave(paste0(format(today(), "%Y-%m-%d"),"_overlay.png"), 
       dpi = 300, width = 8, height  = 6)

focus_today_path + xlim(limits$min_lon[1]-0.02, limits$max_lon[1]+0.02) + 
  ylim(limits$min_lat[1]-0.005, limits$max_lat[1]+0.005)

ggsave(paste0(format(today(), "%Y-%m-%d"),"_zoom.png"), 
       dpi = 300, width = 8, height  = 6)

# write out updated data
write_rds(biketrips, "trips.rds")
