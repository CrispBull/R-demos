library(atrrr)
library(bskyr)
library(tidyverse)
library(purrr)
library(lubridate)
library(ggmosaic)
library(DT)

auth("justchidi.bsky.social")
get_skeets_authored_by("justchidi.bsky.social")

profile <- get_user_info("justchidi.bsky.social")
names(profile)
user_posts <- get_skeets_authored_by("kelseyhightower.com", limit = 1000L)

formatted_data <- user_posts |> 
  mutate(
    postType = map_chr(post_data, '$type'),
    wc = nchar(text), 
    createdAt = with_tz(ymd_hms(map_chr(post_data, "createdAt"), tz = "UTC"), tzone = "America/New_York"),
    dayOfWeek = wday(createdAt, label = TRUE),
    timeOfDay = case_when(
      hour(createdAt) >= 6 & hour(createdAt) < 12 ~ "Morning",
      hour(createdAt) >= 12 & hour(createdAt) < 18 ~ "Afternoon",
      TRUE ~ "Night"
    )
  ) |>  
  select(uri, text, postType, like_count, repost_count, createdAt, dayOfWeek, timeOfDay, wc) |>
  arrange(desc(like_count)) 

freq_table = table(formatted_data$dayOfWeek, factor(formatted_data$timeOfDay, levels = c("Morning", "Afternoon", "Night")))
table_df = as.data.frame(freq_table)

ggplot(table_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Kesley's bsky activity ",
       x = "Day of week",
       y = "Tme of day",
       fill = "Frequency") +
  theme_minimal()

mosaicplot(freq_table, color = TRUE)

tl <- get_own_timeline(limit = 1000L)
head(tl)
colnames(tl)

# Hard to work with data returned from atrrr
## get user profile
acc_name <- "kelseyhightower.com"
kesley_profile <- get_user_info(acc_name)
colnames(kesley_profile)

# explore profile details

kesley_profile |> 
  glimpse()

# Post data analysis
posts <- get_skeets_authored_by(acc_name, limit = 1000L)
posts |> glimpse()

original_posts <- posts |> 
  filter(is_reskeet == FALSE)

retweet_posts <- posts |>
  filter(is_reskeet == TRUE)

### Engagements
original_posts <- original_posts |> 
  mutate(
    author_handle = as.factor(author_handle),
    createdAt = with_tz(ymd_hms(map_chr(post_data, "createdAt"), tz = "UTC"), tzone = "America/New_York"),
    day = as.Date(createdAt),
    weekday = wday(createdAt, label = TRUE),
    timeOfDay = case_when(
      hour(createdAt) >= 6 & hour(createdAt) < 12 ~ "Morning",
      hour(createdAt) >= 12 & hour(createdAt) < 18 ~ "Afternoon",
      TRUE ~ "Night"
    ),
    postType = map_chr(post_data, "$type")
  ) |>
  select(-c(embed_data, author_name, author_data, tags, mentions, post_data, links, langs, labels))

print(original_posts)

original_posts |>
  select(weekday, timeOfDay, like_count) |>
  ggplot(aes(x = like_count, fill = timeOfDay)) +
  geom_histogram() +
  facet_wrap(~ weekday)
  
            

# based on the above chart, shows that distribution. of likes is heavily skewed
# so based on this, I'll mostly be using the median average

median_summary = original_posts |> 
  group_by(timeOfDay, weekday) %>%
  summarise(
    avg_likes = median(like_count, na.rm = TRUE),
    avg_reposts = median(repost_count, na.rm = TRUE),
    avg_replies = mean(reply_count, na.rm = TRUE),
    avg_post = n(),
    .groups = "drop"
  )

print(median_summary)

median_summary |>
  select(-c(timeOfDay, weekday)) |>
  pivot_longer(cols = everything(), names_to = "metric", values_to = "average") |>
  ggplot(aes(x = metric, y = average, fill = metric)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Average Engagement/Post", x = "Engagement metric", y = "Median value") +
  theme_minimal()

# Daily Engagement metric aggregation
enagement_by_day <- original_posts |>
  group_by(day) |>
  summarise(
    total_likes = sum(like_count, na.rm = TRUE),
    total_replies = sum(reply_count, na.rm = TRUE),
    total_reposts = sum(repost_count, na.rm = TRUE),
    total_posts = n()
  )

reshaped_daily_engagement <- enagement_by_day |>
  pivot_longer(cols = c(total_likes, total_replies, total_reposts),
               names_to = "engagement_type",
               values_to = "count")

ggplot(reshaped_daily_engagement, aes(x = day, y = count, color = engagement_type)) +
  geom_line() +
  theme_minimal()


# Top post by engagement
top_20_posts <- original_posts |>
  mutate(total_engagement = like_count + reply_count + repost_count) |>
  arrange(desc(total_engagement)) |>
  select(createdAt, weekday, text, total_engagement) |>
  head(20)

datatable(top_20_posts, options = list(pageLength = 20), caption = "Top 20 posts by engagment")


# Post frequency
### shiny feature here is to be able to filter out days, months or weeks

### post frequency trend
daily_frequency <- original_posts |>
  group_by(date = as.Date(createdAt)) |>
  summarize(count = n())

daily_frequency |>
  filter(as.Date(date) >= ymd("2023-12-31")) |>
  ggplot(aes(x = date, y = count)) +
  geom_line(color = "#2C3E50") +
  labs(title = "Daily Post Frequency") +
  theme_minimal()

weekly_frequency <- original_posts |>
  group_by(week = floor_date(createdAt, "week")) |>
  summarize(count = n())

weekly_frequency |>
  ggplot(aes(x = week, y = count)) +
  geom_line(color = "#2C3E50") +
  labs(title = "Weekly Post Frequency") +
  theme_minimal()

monthly_frequency <- original_posts |>
  group_by(month = floor_date(createdAt, "month")) |>
  summarize(count = n())

monthly_frequency |>
  ggplot(aes(x = month, y = count)) +
  geom_line() +
  labs(title = "Monthly Post Frequency") +
  theme_minimal()


# best time to post
original_posts |>
  mutate(hour = hour(createdAt)) |>
  group_by(weekday, hour) |>
  summarise(avg_engagement = median(like_count + repost_count + reply_count, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = hour, y = weekday, fill = avg_engagement)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#2E86C1") +
  scale_x_continuous(breaks = 1:24, labels = 1:24) +
  labs(
    title = "Average engagement by day and hour heatmap",
    x = "Hour of day",
    y = "Day of week",
    fill = "Average Engagement"
  ) +
  theme_gray()

# bskyr
# authenticate
bs_auth("justchidi.bsky.social", "j7ex-wco5-k47q-pej5")
set_bluesky_user("justchidi.bsky.social")
set_bluesky_pass("j7ex-wco5-k47q-pej5")

# get profile for analytics
just_chidi <- bs_get_profile("justchidi.bsky.social")
kesl_high <- bs_get_profile("kelseyhightower.com")

kesl_high |> glimpse()

jonzing <- get_user_info("justchidi.bsky.social")



# Engagement analysis
post_count <- as.integer(just_chidi$posts_count)
posts <- bs

