library(RPostgreSQL)

con = dbConnect(dbDriver("PostgreSQL"), dbname = "nyc-taxi-data", host = "localhost")
query = function(sql) { fetch(dbSendQuery(con, sql), n = 1e8) }

# Roughly 10,000 rows
sampled_trips = query("SELECT * FROM trips TABLESAMPLE BERNOULLI(0.01)")