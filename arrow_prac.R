# official doc - https://arrow.apache.org/docs/r/index.html
# R4DS Arrow - https://r4ds.hadley.nz/arrow
# doc guide - https://arrow.apache.org/docs/r/articles/read_write.html
# working with multi-files - https://arrow.apache.org/docs/r/articles/dataset.html
# data wrangling - https://arrow.apache.org/docs/r/articles/data_wrangling.html


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

if (!dir.exists("data")) {
  dir.create("data", showWarnings = FALSE)
}

curl::multi_download(
  "https://r4ds.s3.us-west-2.amazonaws.com/seattle-library-checkouts.csv",
  "data/seattle-library-checkouts.csv",
  resume = TRUE
)

seattle_csv <- open_dataset(
  sources = "data/seattle-library-checkouts.csv",
  col_types = schema(ISBN = string()),
  format = "csv"
)

# we can now start using this large dataset with dplyr verbs
seattle_csv |>
  glimpse() # 41.4m rows by 12 columns

seattle_csv |>
  head() |>
  collect()

# while this may be slow, it still works and doesn't take forever or cancel. On my 16GB Ram old macbook, it took about 12 seconds
# which is impressive but its still slow.
seattle_csv |>
  group_by(CheckoutYear) |>
  summarise(Checkouts = sum(Checkouts)) |>
  arrange(CheckoutYear) |>
  collect()

"
We can improve this by switching to a better format from the current csv format, the parquet format. We can switch to this format and
partition the data into multiple files. Parquet can be used to hold rectangular/tabular data like csv, but unlike csv which you can
easily see with a text editor, with parquet its a custom binary format designed for for the needs of big data.

Advantages of parquet here are;
- its usually smaller than the equivalent csv file as it uses an efficient encoding to keep the file size down and supports file compression
- Parquet has a rich type system compared to csv. For example, if I have a a column with values like '25-11-2024' csv won't be able to tell that this is a date and would just see it as a string, parquet on the other hand can store this data together with the type information
- Parquet is a columnar data format, so data are stored column by column, unlike csv where data is stored row by row. For analytic workloads this is better performance because it makes reduces the data fetched from disk as we'll only access columns we need, data is also compressed better because columns have a type so applies better compression and processing algorithms, this leads to much better performance for analytic workloads.
- Parquet files are 'chunked', which makes it possible to process different parts of the file at the same time, even potentially skip some chunks altogether.

Some other columnar data aside from parquet are Apache ORC and BigQuery Capacitor. There are also columnar databases which are used for data warehouses like duckdb, amazon redshift, google bigquery, etc.

- maybe I misunderstand your comment on 'tech decentralization doesn't mater if the main org goes down', are you saying there has to be a main organization that needs to stay up for you to see decentralization?
"

# As datasets gets large, its a good rule of thumb to split it across many files, when this is done smartly, it can lead to significant improvements in performance. Arrow generally recommend that files should be > 20mb and < 2GB, as well as avoid partitions that produce more than 10k files. Also you can partition by variables so its easy to filter by and skip some files. 

pq_path <- "data/seattle-library-checkouts"

# convert csv into parquet with partitions by the checkoutyear. 
seattle_csv |>
  group_by(CheckoutYear) |>
  write_dataset(path = pq_path, format = "parquet")

# This operation took about 50secs. The file has been partitioned into 18 partitions for the different year. Each partition is a folder which contains the parquet file. We can see all this below and their sizes. The naming convention used here is a Hive style approach "key=value", so the folders here are named for example "Checkout=2005". 

pq_partitions <- tibble(
  files = list.files(pq_path, recursive = TRUE),
  size_mb = file.size(file.path(pq_path, files)) / 1024^2
)

pq_partitions

pg_partitions |>
  summarize(total = sum(size_mb))

# Each file is now between 100 - 300mb and the total size is now 4.2GB from ~9GB. Now we have our parquet files, we can now use it for 
# arrow

seattle_pqt <- open_dataset(pq_path)

seattle_pqt |> glimpse() # this was about 2secs compared to the csv equivalent which took about 12secs

seattle_pqt |> # this taks about 2s as well compared to its csv equivalent above. Notice how this is treated like its all one file
  group_by(CheckoutYear) |>
  summarise(Checkouts = sum(Checkouts)) |>
  arrange(CheckoutYear) |>
  collect()

brief <- seattle_pqt |>
  head(20) |>
  collect()

seattle_pqt |>
  filter(CheckoutYear >= 2018, MaterialType == "BOOK") |>
  group_by(CheckoutYear, CheckoutMonth) |>
  summarise(TotalCheckouts = sum(Checkouts)) |>
  arrange(CheckoutYear, CheckoutMonth) |>
  collect()

result |> collect()

# while arrow doesn't support all R operations possible with dplyr, it continues to add more and we can see which ones are supported using the help command on acero; ?acero

# Some benchmarks with the csv
seattle_csv |> 
  filter(CheckoutYear == 2021, MaterialType == "BOOK") |>
  group_by(CheckoutMonth) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(desc(CheckoutMonth)) |>
  collect() |> 
  system.time()

#    user  system elapsed 
# 16.949   3.687  16.341 

seattle_pqt |> 
  filter(CheckoutYear == 2021, MaterialType == "BOOK") |>
  group_by(CheckoutMonth) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(desc(CheckoutMonth)) |>
  collect() |> 
  system.time()

#   user  system elapsed 
# 0.500   0.243   0.140 

# This shows massive improvement of over 100x. This are the kind of potential improvements we can see due to how we partitioned so it just reads one of the 18 partitions, also how parquet are stored in binary format which can be directly read into memory, the column wise storing means Parquet only reads necessary columns specified in our query. 


# We can also use Arrow with duckdb to easily turn an arrow dataset into a duckdb database using the to_duckdb() function. This is lazily computed until we call collect. Plus this action doesn't involve memory copying. Plus allows us to use the dplyr syntax we want that aren't supported or even use sql. 

seattle_pqt |> 
  to_duckdb() |> 
  filter(CheckoutYear == 2021, MaterialType == "BOOK") |>
  group_by(CheckoutMonth) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(desc(CheckoutMonth)) |>
  collect() 
