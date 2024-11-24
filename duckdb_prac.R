# install duckdb R package. 

# References
# - Installation process here https://github.com/duckdb/duckdb-r
# - https://posit.co/blog/databases-with-posit

# Tips
" - Use duckdb as the data warehouse for the data model, use data.table for in memory operations
as data.table is usually much faster for memory operations. So it'll be like in memory 
is my mart and i'm pulling from my warehouse (duckdb)

- Don't use prepared statement for large data transfers into duckdb

"


library(DBI)
library(duckdb)
library(dbplyr)
library(dplyr)
library(nycflights23)

con <- dbConnect(duckdb()) # creates an in-memory database, so dbdir = ":memory:"

dbExecute(con, "CREATE TABLE items (item VARCHAR, value DECIMAL(10, 2), count INTEGER)")
dbExecute(con, "INSERT INTO items VALUES ('jeans', 20.0, 1), ('hammer', 42.2, 2)")
result <- dbGetQuery(con, "SELECT * FROM items")

# insert with prepared statement
dbExecute(con, "INSERT INTO items VALUES (?, ?, ?)", c('laptop', 2000, 1))

# to reuse prepared statement
stmt <- dbSendStatement(con, "INSERT INTO items VALUES (?, ?, ?)")
dbBind(stmt, list('iphone', 300, 2))
dbBind(stmt, list('android', 3.5, 1))
dbClearResult(stmt) # recommended to call to free resources

# query with prepared statement
dbGetQuery(con, "SELECT item FROM items WHERE value > ?", c(400))

# write a df into duckdb as a table. Its recommended to use this approach over individual insert/prepared statements
dbWriteTable(con, "iris_table", iris)
iris_df <- dbGetQuery(con, "SELECT * FROM iris_table")

# or register a data frame as a virtual table, similar to VIEW in SQL
duckdb_register(con, "iris_view", iris)
duckdb_register(con, "flights_vew", flights)
dbGetQuery(con, "SELECT count(*) FROM iris_view")

# aside from using raw sql statements, we can query with dplyr style using 
# dbplyr package
tbl(con, "flights_vew") |>
  group_by(dest) |>
  summarise(delay = mean(dep_time, na.rm = TRUE)) |>
  collect()

dbExecute(con, "DROP VIEW flights_vew")
dbExecute(con, "DROP VIEW iris_view")

# assuming we have a csv
write.csv(mtcars, "mtcars.csv")

# we can summarize in duckdb to avoid reading the entire csv into R's memory
tbl(con, "mtcars.csv") |>
  group_by(cyl) |>
  summarise(across(disp:wt, .fns = mean)) |>
  collect()

# we can do something similar to the above for parquet files as well and avoid reading large files into R memory
# we can easily for example use the sql COPY statement to move large amount of data faster into duck db.

write.csv(weather, "weather.csv")

# quick glance at the csv
tbl(con, "weather.csv") |> 
  collect() |>
  head()

# automatically derive schema from csv. Note that we can modify further like add constraints, etc
dbExecute(con, "CREATE VIEW temp_view AS SELECT * FROM read_csv_auto('weather.csv');")
schema <- dbGetQuery(con, "PRAGMA table_info('temp_view');")
table_name <- "nycfl_weather23_table"

columns <- paste(schema$name, schema$type, collapse = ", ")
create_table_cmd <- paste0("CREATE TABLE ", table_name, " (", columns, ");")
dbExecute(con, create_table_cmd)

dbExecute(con, "DROP VIEW temp_view")
dbListTables(con)

# copy data into table
dbGetQuery(con, "SELECT count(*) FROM nycfl_weather23_table") # empty
copy_cmd <- paste0("COPY ", table_name, " FROM 'weather.csv' (FORMAT CSV, HEADER TRUE);")
dbExecute(con, copy_cmd)
dbGetQuery(con, "SELECT count(*) FROM nycfl_weather23_table") # populated

"
SELECT origin, wind_dir, wind_speed, wind_gust, time_hour
FROM 'nycfl_weather23_table'
LIMIT 20;
" -> cmd
dbGetQuery(con, cmd)


# show all databases in duckdb connection
dbGetQuery(con, "SHOW DATABASES")

# show all tables in current database
dbListTables(con) # or dbGetQuery(con, "SHOW TABLES") # or dbGetQuery(con, "SHOW ALL TABLES")


# after you're done, you can now disconnect from the database. Note that duckdb also automatically disconnects
dbDisconnect(con)

# connect to database
con <- dbConnect(duckdb("dbdir" = "air_quality.db"))
ozone <- tbl(con, "ozone")
