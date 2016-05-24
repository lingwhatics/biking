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


#plots
library(ggplot2)
p <- ggplot(biketrips, aes(Longitude, Latitude, colour = Date))
p + geom_point()

q <- ggplot(biketrips, aes(Longitude, Latitude, colour = year(Date)))
q + geom_point()

r <- ggplot(biketrips, aes(Longitude, Latitude, colour = am(X)))
r + geom_point()