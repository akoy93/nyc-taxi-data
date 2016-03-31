library(dplyr)
library(extrafont)
library(ggplot2)
library(ggmap)
library(grid)
library(maptools)
library(rgdal)
library(RPostgreSQL)
library(scales)

con = dbConnect(dbDriver("PostgreSQL"), dbname = "nyc-taxi-data", host = "localhost")
query = function(sql) { fetch(dbSendQuery(con, sql), n = 1e8) }
title_with_subtitle = function(title, subtitle = "") {
  ggtitle(bquote(atop(bold(.(title)), atop(.(subtitle)))))
}
theme_dark_map = function(base_size = 12) {
  theme_bw(base_size) +
    theme(text = element_text(family = font_family, color = "#ffffff"),
          rect = element_rect(fill = "#000000", color = "#000000"),
          plot.background = element_rect(fill = "#000000", color = "#000000"),
          panel.background = element_rect(fill = "#000000", color = "#000000"),
          plot.title = element_text(family = title_font_family),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
}
if (all(c("Open Sans", "PT Sans") %in% fonts())) {
  font_family = "Open Sans"
  title_font_family = "PT Sans"
} else {
  font_family = "Arial"
  title_font_family = "Arial"
}
boroughs = c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island")
yellow_hex = "#f7b731"
green_hex = "#3f9e4d"

gpclibPermit()

tracts = spTransform(readOGR("../nyct2010_15b", layer = "nyct2010"), CRS("+proj=longlat +datum=WGS84"))
tracts@data$id = as.character(as.numeric(rownames(tracts@data)) + 1)
tracts.points = fortify(tracts, region = "id")
tracts.map = inner_join(tracts.points, tracts@data, by = "id")

nyc_map = tracts.map
ex_staten_island_map = filter(tracts.map, BoroName != "Staten Island")
manhattan_map = filter(tracts.map, BoroName == "Manhattan")

# NYC dot maps
pickups = query("SELECT * FROM trips_by_lat_long_cab_type ORDER BY count")
pickups = mutate(pickups, cab_type_id = factor(cab_type_id))

sampled_trips = query("SELECT * FROM trips TABLESAMPLE BERNOULLI(0.1)")
times <- sampled_trips$pickup_datetime %>% strptime("%Y-%m-%d %H:%M:%S")
times <- as.numeric(format(times, "%H")) + as.numeric(format(times, "%M"))/60 + as.numeric(format(times, "%S"))/3600
sampled_trips <- mutate(sampled_trips, pickup_time = times)

rush_hour_trips <- sampled_trips %>% filter((pickup_time >= 6.5 & pickup_time <= 9.5) | (pickup_time >= 15.5 & pickup_time <= 20))
morning_rush_hour_trips <- sampled_trips %>% filter(pickup_time >= 6.5 & pickup_time <= 9.5)
evening_rush_hour_trips <- sampled_trips %>% filter(pickup_time >= 15.5 & pickup_time <= 20)

filtered_sampled_trips <- sampled_trips %>% filter(pickup_time < 24 & pickup_time > 6.5)

manhattan <- get_map(location="Manhattan", zoom=12)
ggmap(manhattan, extent="device") +
  stat_density2d(data=pickups, 
    aes(x=pickup_long, y=pickup_lat, alpha=count, fill=count),
    size=0.01, bins=30, geom="polygon") +
  scale_fill_gradient(low="green", high="red", limits=range(pickups$count)) + 
  scale_alpha_continuous(range=c(0.01, 0.3), trans="log", limits=range(pickups$count)) +
  theme(legend.position="none")

sampled_locations 

ggmap(manhattan, extent="device") +
  geom_point(data=sampled_trips, aes(x=pickup_longitude, y=pickup_latitude, alpha=0.01, size=0.01, color='red')) +
  scale_alpha(range=c(0.01, 1.0)) +
  scale_size(range=c(0.01, 1.0)) +
  theme(legend.position="none")


  stat_density2d(data=sampled_trips, 
    aes(x=pickup_longitude, y=pickup_latitude, alpha=..level.., fill=..level..),
    size=0.01, bins=30, geom="polygon") +
  scale_fill_gradient(low="green", high="red") + 
  scale_alpha(range=c(0.05, 0.3)) +
  theme(legend.position="none")
  # coord_map(xlim = range(ex_staten_island_map$long), ylim = range(ex_staten_island_map$lat))

# ggmap(manhattan, extent="device") +
#   stat_density2d(data = sampled_trips, 
#     aes(x = dropoff_longitude, y = dropoff_latitude, alpha = ..level.., fill = ..level..),
#     size = 0.01, bins = 30, geom="polygon") +
#   scale_fill_gradient(low="green", high="red") + 
#   scale_alpha(range = c(0.01, 0.3)) +
#   theme(legend.position="none")

alpha_range = c(0.45, 0.75)
size_range = c(0.134, 0.173)

ggmap(manhattan, extent="device") +
  geom_point(data = pickups, aes(x=pickup_long, y=pickup_lat, alpha=count, size=count)) +
  scale_alpha_continuous(range=alpha_range, trans="log", limits=range(pickups$count)) +
  scale_size_continuous(range=size_range, trans="log", limits=range(pickups$count)) +
  theme(legend.position="none")



# Download the base map
tartu_map_g_str <- get_map(location = "tartu", zoom = 13)
# Draw the heat map
ggmap(tartu_map_g_str, extent = "device") +
  stat_density2d(data = tartu_housing, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 30, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.2), guide = FALSE) +
  theme(legend.position="none")

# alpha_range = c(0.14, 0.75)
# size_range = c(0.134, 0.173)

# p = ggplot() +
#   geom_polygon(data = ex_staten_island_map,
#                aes(x = long, y = lat, group = group),
#                fill = "#080808", color = "#080808") +
#   geom_point(data = pickups,
#              aes(x = pickup_long, y = pickup_lat, alpha = count, size = count, color = cab_type_id)) +
#   scale_alpha_continuous(range = alpha_range, trans = "log", limits = range(pickups$count)) +
#   scale_size_continuous(range = size_range, trans = "log", limits = range(pickups$count)) +
#   scale_color_manual(values = c("#ffffff", green_hex)) +
#   coord_map(xlim = range(ex_staten_island_map$long), ylim = range(ex_staten_island_map$lat)) +
#   title_with_subtitle("New York City Taxi Pickups", "2015") +
#   theme_dark_map(base_size = 24) +
#   theme(legend.position = "none")

# fname = "graphs/taxi_pickups_map.png"
# png(filename = fname, width = 490, height = 759, bg = "black")
# print(p)
# dev.off()