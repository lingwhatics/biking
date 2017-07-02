# Get data (exported from the City of Toronto cycling app)
folder <- "~/Documents/GIS_data/Bike_Trips/"
file_list <- list.files(path=folder, pattern = "*.csv")
biketrips <-
  do.call("rbind",
          lapply(file_list,
                 function(x)
                   read.csv(paste0(folder, x), stringsAsFactors = FALSE)))

# Pasre date-times
library(lubridate)
biketrips$date_time <- ymd_hms(paste(biketrips$Date, biketrips$Time, sep = " "))

# Get subset of today's trips only
today_trip <- subset(biketrips, Date == Sys.Date())

# Plot all trips in record and export as PNG
library(ggplot2)
p <- ggplot(biketrips, aes(Longitude, Latitude, colour = Date))
p <- p + geom_point()
p
ggsave("all.png", dpi = 300, width = 10, height  = 6)

# Plot trips by year
q <- ggplot(biketrips, aes(Longitude, Latitude, colour = as.factor(year(Date))))
q + geom_point() + guides(colour = guide_legend(title = "Year"))

# Plot showing which parts of trips were in the morning vs. the afternoon
r <- ggplot(biketrips, aes(Longitude, Latitude, colour = am(date_time)))
r + geom_point() + guides(colour = guide_legend(title = "Morning Ride?"))

# Plot just today's trip
p2 <- ggplot(today_trip, aes(Longitude, Latitude))
p2 <- p2 + geom_point()
p2

# Plot with all trips in grey in the background and today's trip in colour
p <- ggplot(biketrips, aes(Longitude, Latitude)) + geom_point()
p + geom_point(data = today_trip, aes(Longitude, Latitude, colour = Date)) + 
  theme(legend.position="none")
ggsave("today_overlay.png", dpi = 300, width = 6, height  = 6)
