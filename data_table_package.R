# https://github.com/Rdatatable/data.table/wiki

library(data.table)
library(dslabs)
data("murders")
data("heights")
murders <- setDT(murders)
murders |> head()
class(murders)

## selecting with data.table is like we would do for a matrix, not like dplyr's select
murders[, .(region, total, population)] |> head()

# Adding a column here is different from dplyr's mutate
murders[, rate := total / population * 10^5] |> head()

# adding multiple columns at once
murders[, ":="(rate = total / population * 10^5, 
               rank = rank(population))] |> head()

# With data.table data is copied by reference.
a <- data.table(a = 1)
b <- a # copied by reference
a[, a:=2]
a
b[, a:=6]
b
a
# To avoid copying by reference, we use the copy function
c <- data.table(a = 1)
d <- copy(c) # copied by value
c[, a:=2]
c
d[, a:=6]
d
c

# Filtering is done like working on a vector by just passing the logical expression into it
murders[rate >= 0.9] |> head()
murders[region == "South" & rate >= 0.9] |> head()

# We can combine filter and select 
murders[rate >= 1, .(rate, rank, population)] |> head()
murders[state %in% c("New York", "Texas")]

# Summarizing data
hgt <- setDT(heights)
head(hgt)
class(hgt)
# to summarize we add functons inside the .() function operator
heights[, .(average = mean(height), std_dev = sd(height))] |> head()

# Compare to dplyr's sumarize
heights |>
  summarize(average = mean(height), std_dev = sd(height)) |> head()

# for a dplyr equivalent that adds filter, we can do the following for data.table
heights[sex == "Female", .(average = mean(height), std_dev = sd(height))] |> head()

# We can group by and summarize like
heights[, .(average = mean(height), standard_deviation = sd(height)), by = sex]

# Sorting
murders[order(rank)]
murders[order(rank, rate)]
