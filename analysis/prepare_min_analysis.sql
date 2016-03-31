CREATE INDEX index_trips_on_pickup_gid ON trips (pickup_nyct2010_gid);
CREATE INDEX index_trips_on_dropoff_gid ON trips (dropoff_nyct2010_gid);

-- Right now, only yellow taxis are in the database, so the cab_type_id will be the same for all.
CREATE TABLE trips_by_lat_long_cab_type AS
SELECT
  cab_type_id,
  ROUND(pickup_longitude, 4) AS pickup_long,
  ROUND(pickup_latitude, 4) AS pickup_lat,
  COUNT(*) AS count
FROM trips
WHERE pickup_nyct2010_gid IS NOT NULL
  AND cab_type_id IN (1, 2)
GROUP BY cab_type_id, pickup_long, pickup_lat
ORDER BY cab_type_id, count;

CREATE TABLE dropoff_by_lat_long_cab_type AS
SELECT
  cab_type_id,
  ROUND(dropoff_longitude, 4) AS dropoff_long,
  ROUND(dropoff_latitude, 4) AS dropoff_lat,
  COUNT(*) AS count
FROM trips
WHERE dropoff_nyct2010_gid IS NOT NULL
GROUP BY cab_type_id, dropoff_long, dropoff_lat
ORDER BY cab_type_id, count;