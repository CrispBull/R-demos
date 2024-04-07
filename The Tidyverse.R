library(dslabs)
library(tidyverse)

dim(murders)
data(co2)
class(co2)
head(co2)

## Adding a new column to a data frame
head(murders)
murders <- mutate(murders, rate = total / population * 100000)
head(murders)

# Subsetting with filter
high_rate_per_100k <- filter(murders, rate >= 0.75)

# Selecting from data frames. Notice the new pipe operator |> used here. This SO answer summarizes the difference with 
# the %>% pipe operator https://stackoverflow.com/questions/67633022/what-are-the-differences-between-rs-new-native-pipe-and-the-magrittr-pipe 

new_table <- select(murders, state, region, rate) |>
  filter(rate >= 1)

# summarizing data
data("heights")
s <- heights |> 
  filter(sex == "Female") |>
  summarize(average = mean(height), standard_deviation = sd(height))

# We can also get multiple summaries from a value. for example;
heights |>
  filter(sex == "Female") |>
  summarize(median_mean_max = quantile(height, c(0.5, 0, 1)))

quant_func <- function(n_vec) {
  quants = quantile(n_vec, c(0.5, 0, 1))
  data.frame(median = quants[1], minimum = quants[2], maximum = quants[3])
}

heights |>
  filter(sex == "Female") |>
  summarize(quant_func(height))

## We can group items using group by
height_avgs <- heights |> 
  group_by(sex) |>
  summarize(avg_height = mean(height), std_dev = sd(height))

# pull is used to retrieve single columns of outputs from data frames. It's basically $ applied on data frames
height_avgs |> pull(std_dev)

# Sorting data frames with dplyr is usually done using the arrange function, unlike sort and order used on vectors. 
murders |> 
  arrange(rate) %>% # notice this difference here, the old pipe makes it possible to call head without the parenthesis unlike new pipe
  head

murders |>
  arrange(desc(rate)) |>
  head()

murders |>
  arrange(rate, population, region) |>
  top_n(6, rate)

# tibbles are the preferred data frame types for tidyverse. Also Tibble gives us error messages for errors. Tibbles can also hold complex
# data types, like holding a function
as_tibble(murders)
tibble(id = c(1, 3, 4, 9), funcs = c(mean, median, sd, mode))

# When using pipes in R, the old pipe %>% uses . as the placeholder parameter, for the new pipe |> it's equivalent is _
# In R, the purr package is a package for functional programming, contains functions like map, flatten, etc. Another useful function is 
# case_when, which is like ifelse but multiple vectors and outputs these vectors
murders |>
  mutate(group = case_when(abb %in% c("WA", "OR", "CA") ~ "West Coast",
                           abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "New England",
                           region == "South" ~ "South",
                           TRUE ~ "Other")) |>
  group_by(group) |>
  summarize(rate = sum(total) / sum(population) * 10^5)

# another common function is the between function. Like the name suggests, checks if a certain values values betwen two values.
between(4, 2, 7) # TRUE
between(4, 7, 2) # FALSE
