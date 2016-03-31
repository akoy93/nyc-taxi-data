library(Rsomoclu)
library(kohonen)
library(RPostgreSQL)
library(ggmap)
library(dplyr)

con = dbConnect(dbDriver("PostgreSQL"), dbname = "nyc-taxi-data", host = "localhost")
query = function(sql) { fetch(dbSendQuery(con, sql), n = 1e8) }

# Roughly 150,000 rows
sampled_trips <- query("SELECT * FROM trips TABLESAMPLE BERNOULLI(0.1)")
times <- sampled_trips$pickup_datetime %>% strptime("%Y-%m-%d %H:%M:%S")
times <- as.numeric(format(times, "%H")) + as.numeric(format(times, "%M"))/60 + as.numeric(format(times, "%S"))/3600
sampled_trips <- mutate(sampled_trips, pickup_time = times)

late_night_trips <- sampled_trips %>% filter(pickup_time < 4 & trip_distance > 1)

sampled_trips <- late_night_trips

pickup_locations <- na.omit(sampled_trips[c('pickup_longitude', 'pickup_latitude')])
dropoff_locations <- na.omit(sampled_trips[c('dropoff_longitude', 'dropoff_latitude')])

# Bounding Box for New York
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

# Filtered Pickup and Dropoff Locations
pickup_locations <- pickup_locations %>%
  filter(pickup_longitude >= min_long && pickup_longitude <= max_long && pickup_latitude <= max_lat && pickup_latitude >= min_lat)
dropoff_locations <- dropoff_locations %>%
  filter(dropoff_longitude >= min_long && dropoff_longitude <= max_long && dropoff_latitude <= max_lat && dropoff_latitude >= min_lat)

# Normalize Pickup and Dropoff Locations
pickup_locations[,1] <- (pickup_locations[,1] - min_long) / (max_long - min_long)
pickup_locations[,2] <- (pickup_locations[,2] - min_lat) / (max_lat - min_lat)
dropoff_locations[,1] <- (dropoff_locations[,1] - min_long) / (max_long - min_long)
dropoff_locations[,2] <- (dropoff_locations[,2] - min_lat) / (max_lat - min_lat)

pickup_input_data <- matrix(c(pickup_locations$pickup_longitude, pickup_locations$pickup_latitude), ncol=2)
dropoff_input_data <- matrix(c(dropoff_locations$dropoff_longitude, dropoff_locations$dropoff_latitude), ncol=2)

nSomX <- 5
nSomY <- 10
nEpoch <- 200
radius0 <- 3
radiusN <- 1
radiusCooling <- "linear"
scale0 <- 1.0
scaleN <- 0.01
scaleCooling <- "linear"
kernelType <- 0
mapType <- "planar"
gridType <- "rectangular"
compactSupport <- FALSE
#codebook <- input_data[sample(nrow(input_data), nSomX * nSomY), ]
codebook <- NULL
neighborhood <- "gaussian"

pickup_res <- Rsomoclu.train(pickup_input_data, nEpoch, nSomX, nSomY,
                      radius0, radiusN,
                      radiusCooling, scale0, scaleN,
                      scaleCooling,
                      kernelType, mapType, gridType, compactSupport, neighborhood, codebook)

dropoff_res <- Rsomoclu.train(dropoff_input_data, nEpoch, nSomX, nSomY,
                      radius0, radiusN,
                      radiusCooling, scale0, scaleN,
                      scaleCooling,
                      kernelType, mapType, gridType, compactSupport, neighborhood, codebook)

# Convert To Kohonen Object for Plotting
pickup_sommap <- Rsomoclu.kohonen(pickup_input_data, pickup_res)
dropoff_sommap <- Rsomoclu.kohonen(dropoff_input_data, dropoff_res)

# Cluster Centers
pickup_centers <- as.data.frame(pickup_sommap$codes)
pickup_centers[,1] <- pickup_centers[,1] * (max_long - min_long) + min_long
pickup_centers[,2] <- pickup_centers[,2] * (max_lat - min_lat) + min_lat

dropoff_centers <- as.data.frame(dropoff_sommap$codes)
dropoff_centers[,1] <- dropoff_centers[,1] * (max_long - min_long) + min_long
dropoff_centers[,2] <- dropoff_centers[,2] * (max_lat - min_lat) + min_lat

manhattan <- get_map(location="Manhattan", zoom=12)
ggmap(manhattan, extent="device") +
  geom_point(data=pickup_centers, aes(x=V1, y=V2, color='red')) +
  geom_point(data=dropoff_centers, aes(x=V1, y=V2)) +
  theme(legend.position="none")