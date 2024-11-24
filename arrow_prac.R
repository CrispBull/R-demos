# official doc - https://arrow.apache.org/docs/r/index.html

"
Arrow is primarily a data format that provides a way to store, process and move data fast.  
It allows sharing data between systems without expensive serialize/deserialize operations.
It is also a columnar format which is efficient for analytic queries and processing. Arrow isn't
a dbms like duckdb. The Arrow format is a language agnostic columnar format for in-memory or larger than memory data. 

- A rule of thumb is that you want at least twice as much memory size as the size of your data, so when working with large 
files like large csv/parquet files, we want to avoid read_csv by all means
"

library(arrow)
library(duckdb)
library(dbplyr, warn.conflicts = FALSE)
library(tidyverse)

# say we have a large csv file; this example file is about 9GB

dir.create("data", showWarnings = FALSE)

curl::multi_download(
  "https://r4ds.s3.us-west-2.amazonaws.com/seattle-library-checkouts.csv",
  "data/seattle-library-checkouts.csv", resume = TRUE
)
