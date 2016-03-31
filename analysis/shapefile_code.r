library(Rsomoclu)
library(kohonen)
library(RPostgreSQL)
library(clusterSim)
library(rgdal)
library(rgeos)

con = dbConnect(dbDriver("PostgreSQL"), dbname = "nyc-taxi-data", host = "localhost")
query = function(sql) { fetch(dbSendQuery(con, sql), n = 1e8) }

# Roughly 15,000 rows
sampled_trips <- query("SELECT * FROM trips TABLESAMPLE BERNOULLI(0.01)")

pickup_locations <- na.omit(sampled_trips[c('pickup_longitude', 'pickup_latitude')])
dropoff_locations <- na.omit(sampled_trips[c('dropoff_longitude', 'dropoff_latitude')])

# Read NYC shapefile
nyc <- readOGR('nybb_16a', 'nybb')
nyc <- spTransform(nyc, CRS("+proj=longlat +datum=WGS84"))
plot(nyc[nyc$BoroName == 'Manhattan',], axes=TRUE, border="gray")

point <- data.frame(lon=c(-73.96,1), lat=c(40.77,3))
sp2   <- SpatialPoints(point,proj4string=CRS(proj4string(nyc)))
gContains(nyc[nyc$BoroName == 'Manhattan',], sp2[1,])

sp3 <- SpatialPoints(pickup_locations,proj4string=CRS(proj4string(nyc)))