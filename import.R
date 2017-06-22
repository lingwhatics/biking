# Get data
folder <- "~/Documents/GIS_data/Bike_Trips/"
file_list <- list.files(path=folder, pattern = "*.csv")
biketrips <-
  do.call("rbind",
          lapply(file_list,
                 function(x)
                   read.csv(paste0(folder, x), stringsAsFactors = FALSE)))

library(lubridate)
biketrips$X <- ymd_hms(paste(biketrips$Date, biketrips$Time, sep = " "))

today_trip <- subset(biketrips, Date == Sys.Date())

#plots
library(ggplot2)
p <- ggplot(biketrips, aes(Longitude, Latitude, colour = Date))
p <- p + geom_point()
p
ggsave("all.png", dpi = 300, width = 10, height  = 6)

q <- ggplot(biketrips, aes(Longitude, Latitude, colour = year(Date)))
q + geom_point()

r <- ggplot(biketrips, aes(Longitude, Latitude, colour = am(X)))
r + geom_point()

p2 <- ggplot(today_trip, aes(Longitude, Latitude))
p2 <- p2 + geom_point()
p2

p + geom_point(data = today_trip, aes(Longitude, Latitude, colour = Date)) + 
  theme(legend.position="none")
ggsave("today_overlay.png", dpi = 300, width = 6, height  = 6)
