# Tag this year
biketrips2 <- biketrips2 |>
  mutate(this_year = as.factor(if_else(year(date) == 2024, 1, 0)))

# Plot just today's trip
this_year_rides_path <- biketrips2 |>
  filter(this_year == 1) |>
  ggplot(aes(longitude, latitude)) +
  geom_path() +
  coord_map()
this_year_rides_path

# Plot with all trips in grey in the background and today's trip in colour
# limit to Montreal
focus_this_year_path <- biketrips2 |>
  filter(tz == "America/Montreal", this_year == 0) |>
  ggplot(aes(longitude, latitude, colour = today, group = file)) +
  geom_path() +
  geom_path(data=filter(biketrips2, this_year == 1), 
            aes(longitude, latitude, colour = this_year, group = file), linetype = "dotdash") +
  theme_void() +
  theme(legend.position = "none") +
  coord_map() +
  ggthemes::scale_color_colorblind() +
  NULL

focus_this_year_path
ggsave(paste0(format(today(), "%Y-%m-%d"), "_2024.png"),
       dpi = 300, width = 8, height = 6
)
