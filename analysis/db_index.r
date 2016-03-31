library(clusterSim)
library(ggmap)
library(kohonen)
library(rgdal)
library(rgeos)
library(RPostgreSQL)
library(Rsomoclu)

con <- dbConnect(dbDriver("PostgreSQL"), dbname = "nyc-taxi-data", host = "localhost")
query <- function(sql) { fetch(dbSendQuery(con, sql), n = 1e8) }

# Roughly 15,000 rows
sampled_trips <- query("SELECT * FROM trips TABLESAMPLE BERNOULLI(0.01)")

# Get all taxi trip endpoints
all_locations <- na.omit(data.frame(longitude=append(sampled_trips$pickup_longitude, sampled_trips$dropoff_longitude),
                            latitude=append(sampled_trips$pickup_latitude, sampled_trips$dropoff_latitude)))

# Read NYC shapefile
nyc <- readOGR('Desktop/CMSC828E/project/nyc-taxi-data/analysis', 'nybb')
nyc <- spTransform(nyc, CRS("+proj=longlat +datum=WGS84"))
manhattan <- nyc[nyc$BoroName == 'Manhattan',]

# Filter out non-Manhattan points
is_in_manhattan <- apply(all_locations, 1, function(point) {
  point_df <- data.frame(longitude=c(point[1]), latitude=c(point[2]))
  sp1 <- SpatialPoints(point_df, proj4string=CRS(proj4string(manhattan)))
  gContains(manhattan, sp1[1,])
})
all_manhattan_locations <- all_locations[is_in_manhattan,]


# Normalize Pickup and Dropoff Locations
min_long <- min(all_manhattan_locations$longitude)
max_long <- max(all_manhattan_locations$longitude)
min_lat <- min(all_manhattan_locations$latitude)
max_lat <- max(all_manhattan_locations$latitude)

input_data <- matrix(c((all_manhattan_locations$longitude - min_long) / (max_long - min_long),
                       (all_manhattan_locations$latitude - min_lat) / (max_lat - min_lat)), ncol=2)

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
codebook <- NULL
neighborhood <- "gaussian"

res <- Rsomoclu.train(input_data, nEpoch, nSomX, nSomY,
                             radius0, radiusN,
                             radiusCooling, scale0, scaleN,
                             scaleCooling,
                             kernelType, mapType, gridType, compactSupport, neighborhood, codebook)

# Convert To Kohonen Object for Plotting
sommap <- Rsomoclu.kohonen(input_data, res)

# Cluster Centers
centers <- as.data.frame(sommap$codes)
centers[,1] <- centers[,1] * (max_long - min_long) + min_long
centers[,2] <- centers[,2] * (max_lat - min_lat) + min_lat

# Plot cluster centers
ggmap(get_map(location="Manhattan", zoom=12), extent="device") +
  geom_point(data=centers, aes(x=V1, y=V2, color='red')) +
  theme(legend.position="none")

# Calculate Davies-Bouldin Index
# Get cluster ID for each point
cluster_ids <- apply(all_manhattan_locations, 1, function(point) {
  which.min(apply(centers, 1, function(center) dist(rbind(center, point))))
})

DB_index <- index.DB(all_manhattan_locations, cluster_ids)$DB