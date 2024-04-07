library(dslabs)
library(tidyverse)
# Summary Statistics
# Variables: Variables in summary statistics can be categorical or numerical. 
# Categorical variables can be Ordinal or nominal, while Numerical variables can be discrete or continuous. When categorical variables have some order we call them Ordinal, when 
# it doesn't we call them nominal. Continuous variables usually are fractions while discrete are usually whole numbers

# Distributions: For categorical variables we check the distribution by calculating
# the proportion of unique categories. 
data(murders)
prop.table(table(state.region))

data("heights")
head(heights)
dim(heights)
class(heights)
prop.table(table(heights$sex))

# For numerical data, a useful way to describe the distribution of its values is by defining a function 
# that reports the proportion of the data entries x that are below a, for all possible values of a. This 
# function is called the Empirical Cumulative Distribution function (ECDF), denoted as F in statistics. 
# F(a) = Calculates the proportion of data points that are less than or equal to a
# This is similar to the prop table shown above for categorical variables and can be visualized with an ecdf 
# plot
ggplot(heights, aes(height)) +
  stat_ecdf(geom = "step") +
  labs(title="ECDF",
       y = "F(height)", x = "Height in inches")

# ECDF plots are not very popular in practice because they don't convey certain summaries of interest like 
# what values are mostly represented, is the distribution symmetric, etc. Some others can show this, an example is 
# the histogram. Histograms plots numerical data by first breaking the data into non overlapping ranges (known as bins),
# then count the number of values that fall in each bins which now shows the bar heights in a histogram. This is 
# similar to a bar plot (for categorical variables)

ggplot(heights, aes(height, fill = sex), position = "jitter") +
  geom_histogram() 

# Smooth Density plots also convey the same information as histograms, but unlike histograms, they don't break 
# the data into bins and show the count of each bin on the y-axis, instead they show the density of the values.
# Basically a Smooth Density plot is a line that goes through all the top of a histogram bars when
# the bins are very very small. Note that the smoothness of the curve is a function that we can control
ggplot(heights, aes(height, color = sex)) +
  geom_density(position = "jitter")

# Normal Distribution describes a numerical dataset outside what is obvious in the charts above. A normal distribution 
# is well captured by two values, the mean and stand deviation. What this implies is that for a dataset approximated 
# by a normal distribution, with just the mean and standard deviation we can values of this dataset randomly. 
heights |> 
  filter(sex == "Male") |>
  summarize(
    mean = sum(height) / length(height),
    sd = sqrt(sum((height - mean) ^ 2) / length(height))
  )
# Standard units are a way of calculating how many standard deviation from the mean a value is from its parent sample. 
# This makes sense to do for a normally distributed data because it means we're like making the average 0 since we 
# know the standard deviation (distance between points) and then computing how many away from 0 a value might fall. 
x <- heights$height[466]
z <- (x - mean(heights$height)) / sd(heights$height)
z
heights <- heights |>
  mutate(standard_unit = scale(heights$height)) # Instead of computing manually, we use scale function to also do it

heights$standard_unit[466] == z # TRUE

ggplot(heights, aes(standard_unit)) +
  geom_histogram()

mean(abs(heights$standard_unit) < 2) # Proportion of values within 2 standard deviation from average

# Quantile-Quantile plots: In statistics, one way to access if a data fits a normal distribution is to check how well 
# the proportions of the observed and predicted values match. This is the idea for a qqplot. In R, we can get the 
# proportion of a standardized normally distributed data using the pnorm function. By default this uses a mean and sd of 0 and 1. 
pnorm(-1.96) # 0.025
pnorm(1.96) # 0.975

# The inverse of the distribution function (pnorm) is the quantile function (qnorm) for a normal distribution. We get 
# this by providing the proportion values
qnorm(0.025) # -1.96
qnorm(0.975) # 1.96

# With our own data values that's normally distributed, we can compute distribution and quantile functions by providing 
# our own mean and sd values
qnorm(0.975, 5, 2)

# We can extend this to distributions outside a normal distribution. We can define the quantile q associated with any
# proportion p as the q for which the proportion of values below q is p. That is, q is the value which for which 
# mean(x <= q) = p. For example,
values <- heights$height
q <- 69.5
p <- mean(values <= q) # 0.60

# note that I did't use pnorm here because that works on standardized values with known mean and sd values. Also what 
# the above showed is that the distribution function is 0.6 and the quantile function value is 69.5 

h_mean <- mean(values)
h_sd <- sd(values)

q <- qnorm(p, h_mean, h_sd) # 69.5

# The idea behind a qqplot is to check if your data is well approximated by a normal distribution. If the quantiles of 
# your data is similar to the quantiles of a normal distribution, then it is well approximated by a normal distribution.
# To create a qqplot for one's data; 

# > Define a vector of proportions
props <- seq(0.05, 0.95, 0.05)

# > Define a vector of quantiles for your data proportions. This computes outputs and the way to interpret it is by 
# reading the value as the quantile where the percentage of data is less than or equal to it. 
quants <- quantile(values, props)

# Define a vector of theoretical quantiles for the proportions for normal distribution with the data's mean and sd
theoritical_quantiles <- qnorm(props, h_mean, h_sd)

# Plot sample quantiles vs the theoretical quantiles
qplot(theoritical_quantiles, quants) + geom_abline()

# This is even better if we use standard units instead of the actual values
height_std_units <- heights$standard_unit
std_quants <- quantile(height_std_units, props)
std_theoritical_quantiles <- qnorm(props)
qplot(std_theoritical_quantiles, std_quants) + geom_abline()

# One popular form of quantiles are percentiles. Percentiles are the quantiles you get for p at 0.01, 0.02...0.99. So 
# at p = 0.25 we say it's the 25th percentile, which is the number for which 25% of the values in the vector are below.
# The most famous percentile is the 50th percentile, which is also known as the Median. 

head(murders)
murders |>
  mutate(rate = )