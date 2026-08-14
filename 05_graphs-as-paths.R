# Libraries
library(XML)
library(tidyverse)
library(janitor)
library(ggthemes) # colour-blind friendly options
library(sf)
library(ggspatial) # scale bars and north arrows

# define function for parsing GPX inputs
parse_GPX <- function(filename) {
  filename %>%
    xmlTreeParse(useInternalNodes = TRUE) %>%
    xmlRoot() %>%
    xmlToList() %>%
    (function(x) x$trk) %>%
    (function(x) unlist(x[names(x) == "trkseg"], recursive = FALSE)) %>%
    map_df(function(x) as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)) %>%
    mutate(file = filename)
}

# Load existing data
biketrips <- read_rds("trips.rds")
# List files already in data
existing_files <- unique(biketrips$file)

# Get data (exported from STRAVA)
folder <- "Bike_Trips/"
file_list <- list.files(path = folder, pattern = "*.gpx")

# keep only new files in folder for import
new_list <- setdiff(file_list, existing_files)

# import files and include filename as id column
gpx <- map_df(paste0(folder, new_list), parse_GPX) |>
  mutate(file = str_remove(file, folder)) |> 
  select(1:2, 4:6, 3)

last_date = max(biketrips$date_time)
names(gpx) <- c("X", "date_time", "latitude", "longitude", "file", "heart_rate")
#tz_use = "America/Montreal"

gpx <- gpx %>%
  mutate(tz = "America/Montreal") %>%
  mutate(
         date_time = format(ymd_hms(date_time), tz = "America/Montreal", usetz = TRUE),
         time = format(as.POSIXct(strptime(date_time,
                                           "%Y-%m-%d %H:%M",
                                           tz = "America/Montreal")),
                       format = "%H:%M:%S"
         ),
         latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         date_time = as.POSIXct(date_time)
  ) 

gpx <- gpx %>%
  mutate(date = format(as.POSIXct(strptime(gpx$date_time, 
                                           "%Y-%m-%d %H:%M", 
                                           tz = "America/Montreal")), 
                       format = "%Y-%m-%d"))

biketrips <- bind_rows(biketrips, gpx)

# reduce number of data points where they only add clutter
biketrips2 <- biketrips |>
  mutate(reduce = case_when(
    date == lag(date) & round(latitude, 4) == lag(round(latitude, 4)) & round(longitude, 4) == lag(round(longitude, 4)) ~ NA_real_,
    TRUE ~ 1
  )) |>
  drop_na(reduce)

# Get subset of today's trips only
biketrips2 <- biketrips2 |>
  #mutate(today = as.factor(if_else(date == Sys.Date(), 1, 0)))
  #mutate(today = as.factor(if_else(file %in% new_list, 1, 0)))
  mutate(today = as.factor(if_else(file %in% new_list, date, "0")))

# Plot just today's trip
todays_ride_path <- biketrips2 |>
  filter(today != "0") |>
  ggplot(aes(longitude, latitude, group=time)) +
  geom_path() +
  coord_map()
todays_ride_path

limits <- biketrips2 |>
  filter(today != "0") |>
  summarise(
    min_lon = min(longitude),
    max_lon = max(longitude),
    min_lat = min(latitude),
    max_lat = max(latitude)
  ) |>
  mutate_all(~ signif(., digits = 5))

# Plot with all trips in grey in the background and today's trip in colour
# limit to Montreal
focus_today_path <- biketrips2 |>
  filter(tz == "America/Montreal", today == "0") |>
  ggplot(aes(longitude, latitude, colour = today, group = file)) +
  geom_path() +
  geom_path(data=filter(biketrips2, today != "0"), aes(longitude, latitude, colour = today, group = file)) +
  theme_void() +
  #theme(legend.position = "none") +
  coord_sf(crs = 4269) +
  ggthemes::scale_color_colorblind(name = "Date") +
  annotation_north_arrow(location = "tl", which_north = "true",
                         height = unit(0.7, "cm"),
                         width = unit(0.7, "cm"),
                         pad_x = unit(0.4, "in"), pad_y = unit(1, "in"),
                         style = north_arrow_orienteering) +
  NULL

focus_today_path
ggsave(paste0(format(today(), "%Y-%m-%d"), "_overlay.png"),
       dpi = 300, width = 8, height = 6
)

biketrips2 |>
  filter(tz == "America/Montreal", today == 0) |>
  ggplot(aes(longitude, latitude, colour = today, group = file)) +
  geom_path() +
  geom_path(data=filter(biketrips2, today != "0"), aes(longitude, latitude, colour = today, group = file)) +
  theme_void() +
  theme(legend.position = "bottom") +
  coord_sf(crs = 4269) +
  ggthemes::scale_color_colorblind(name = "Date") +
  scale_linetype_discrete(name = "Date") +
  xlim(limits$min_lon[1] - 0.0075, limits$max_lon[1] + 0.0075) +
  ylim(limits$min_lat[1] - 0.005, limits$max_lat[1] + 0.005) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(0.7, "cm"),
                         width = unit(0.7, "cm"),
                         pad_x = unit(0.75, "in"), pad_y = unit(.35, "in"),
                         style = north_arrow_orienteering) +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  NULL

ggsave(paste0(format(today(), "%Y-%m-%d"), "_zoom.png"),
  dpi = 300, width = 8, height = 6
)

# write out updated data
write_rds(biketrips, "trips.rds")

