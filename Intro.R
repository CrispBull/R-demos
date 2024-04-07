library(tidyverse)
library(dslabs)
library(ggplot2)
library(dplyr)

#install.packages(c("tidyverse", "dslabs"))

data(murders)
head(murders)
summary(murders)


murders %>% 
  ggplot(aes(population, total, label = abb, color = region)) +
  geom_label()

# use ?function_name to find information about a function. We can also use help(function_name).
# use args(function_name) to see the args of a function

first_n <- function(n) {
  return(n * (n + 1) / 2)
}

same_as_first_n <- function(n) {
  sum(seq(1, n))
}

class(murders)
str(murders)
summary(murders)
names(murders)

pop <- murders$population
class(pop)

regions <- murders$region
value <- murders$total

regions_reordered <- reorder(regions, value, FUN = sum)


# Lists in R are like json and an equivalent of Map<String, Any> in Kotlin
v <- list(first = "flsjf", second = "fsjf", third = 2, fourth = TRUE, last = c(4, 6, "523"))
v$first
v[1] ## $first "flsjf"
v[[1]] # "flsjf"

# The difference between both accessors above is that for the first one we know the name of the property, while the second one is useful when we don't have a name for the property we want to access.

# In R, the difference between a data frame and a Matrix is that a data frame can have more than one entry type, while a matrix can only have one type. Otherwise they're both very similar. One adavantage of Matrices over data frames is that matrices are much more efficient for performing matrix algebra operations. 
mat <- matrix(1:24, 8, 3) # 8 rows and 3 columns
mat[6, 2]
mat[4, ]
mat[, 3]
mat[, 2:3]
mat[c(1, 3, 5), ]

# We can convert a matrix into a data frame like;
as.data.frame(mat)

# Vectors are defined using the c function, bear in mind that in R, the vector or list can contain any type. It's highly recommended that vectors should be of the same type to avoid confusion with coercion. 
sibs <- c("Ikechi", 'Oluchi', "Touchukwuu", 'Chinecherem')
class(sibs)
# Vectors can also be named;
sibs <- c(first = "Ikechi", third = "Oluchi", fourth = "Touchukwu", fifth = "Chinecherem")
class(sibs)
names(sibs)
# we can also name the vector using the names function and setting a value to it
names(sibs) <- c("one", "two", "three", "four")
names(sibs)

# sequences are types created using seq function. It's similar to range in Kotlin
seq(1, 4)
# a third function provides a steps by which it creates the sequence
seq(1, 10, 3)

sibs[c(1, 3)] # subsetting are done using individual index values/vectors/sequences or the names 
sibs[(c("one", "four"))]

# Notice how the string is coerced to return an NA value for the value it couldn't parse. 
as.numeric(c(1, "fsf", 4)) #  1 NA  4

# The difference between sort and order is that sort sorts the values, order gives us the index of the sorted variable
murder_counts <- murders$total
sorted_indexes <- order(murder_counts)
murder_counts[sorted_indexes]
murders[sorted_indexes, ] # used the sorted total order to reorder the list
# The difference btw rank and order is that order returns the indexes ordered while rank returns the position of the value at that index
rank(murder_counts) 

# Other useful standard package functions
max(murders$population) # returns the value instead of the index
which.max(murders$population) # returns the index
murders[which(murders$total > 100), ]

# vector arithmetics 
values <- c(2, 4, 13, 535.1, 53, 1, 77)
values * 11
values - 4

murder_rate <- (murders$total / murders$population) * 100000
murders$rate <- murder_rate  # creates a new column - "rate" in the murders table
murders[order(murders$rate), ]

# which is useful for indexing a vector based on a condition and it returns indexes that matches that condition
murders[which(murders$rate >= 1), ]
# match is similar to which but does this based on a vector of conditions
murders[match(c("Alabama", "California", "Delaware"), murders$state), ]
# %in% is like match but instead of returning an index, returns boolean values true/false for those that matches the lhs
c("Lagos", "Alabama", "Delaware") %in% murders$state #[1] FALSE  TRUE  TRUE

# basic plots
x <- murders$population / 10^6
y <- murders$total
plot(x, y)

with(murders, plot(rate, total))

# If/else
a <- 0
if (a != 0) {
  print(1 / 0)
} else {
  print("Print nothing for 0")
}

murder_index <- which(murder_rate > 0.5)
murder_index
rates_expression <- ifelse(length(murder_index) > 20, "A lot lot!", "A lot!")
print(rates_expression)

data(na_example)
has_na <- ifelse(is.na(na_example), 0, na_example) ## replaces all NA values here with 0
sum(has_na)
sum(is.na(has_na)) #> 0
sum(na_example, na.rm = TRUE)

# Other useful standard functions are all and any, they work similarly to how their Kotlin equivalent does

search()

# for loops in R
for(index in 1:5) {
  print(index)
}

n <- 25
s_n <- vector(length = n)
for(index in 1:n) {
  s_n[index] = index^3
}
plot(1:n, sqrt(s_n))

# In R, we barely use for loops and instead use vectorized functions, which are functions that applies a function to all values in a vector an example of such is sqrt, log, etc. 

sqrt(s_n)

# To create our own vectorized functions, we use Functionals, which are functions that takes a function 
# and applies it to all elements of a list or vector There are different kinds of functionals with slightly different usecases. Example of functionals are sapply, apply, replicate, mapply, etc

n <- 1:100
sapply(n, sqrt)

