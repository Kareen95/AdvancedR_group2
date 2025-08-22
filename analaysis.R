# download the data from github
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/movies.csv')
shows <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/shows.csv')

#explore the dataset
summary(movies)
head(movies)
str(movies)

#load the libraries for what you need
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)

#organize the dataset and make it look nice
movies <- movies %>% mutate(type = "movie")
movies <- movies %>% select(-source)

shows <- shows %>% mutate(type = "show")
shows <- shows %>% select(-source)

#combine datasets
All_shows <- rbind(movies, shows)

#convert everything into minutes to ease analysing them

All_shows <- All_shows %>%
  mutate(
    runtime_minutes = 
      as.numeric(str_extract(runtime, "\\d+(?=H)")) * 60 +
      as.numeric(str_extract(runtime, "\\d+(?=M)"))
  )

All_shows <- All_shows %>% mutate(hours_per_view = hours_viewed / views)


All_shows <- All_shows %>%
  mutate( minutes_per_view = hours_per_view*60 )


All_shows <- All_shows %>% mutate(proportion_watched = minutes_per_view / runtime_minutes)

All_shows <- All_shows %>% mutate(percentage_watched = proportion_watched * 100)


#compare the shows and movies
summary_by_type <- All_shows %>%
  group_by(type) %>%
  summarise(
    total_watchtime = sum(hours_viewed, na.rm = TRUE),
    total_views = sum(views, na.rm = TRUE),
    avg_watchtime_per_view = total_watchtime / total_views,
    average_percentage = mean(percentage_watched, na.rm = TRUE)
  )
mean(All_shows$minutes_per_view)
#prepare the data for plotting
plot_data <- summary_by_type %>%
  select(type,
         total_watchtime,
         total_views
  ) %>%
  pivot_longer(
    cols = -type,
    names_to = "aspect",
    values_to = "value"
  )

plot_data_2 <- summary_by_type %>%
  select(type,
         avg_watchtime_per_view) %>%
  pivot_longer(
    cols = -type,
    names_to = "aspect",
    values_to = "value"
  )

plot_data_3 <- summary_by_type %>%
  select(type,
         average_percentage) %>%
  pivot_longer(
    cols = -type,
    names_to = "aspect",
    values_to = "value"
  )
####### make the plots ##########

#Plot1: plotting total views and total watch time for movies and shows

ggplot(plot_data, aes(x = aspect, y = value, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("movie" = "red", "show" = "blue")) +
  labs(
    title = "Movies vs Shows: total views and total watch time",
    x = "",
    y = "Value",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))


#Plot2: plotting avg watch time per view
ggplot(plot_data_2, aes(x = aspect, y = value, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("movie" = "red", "show" = "blue")) +
  labs(
    title = "Movies vs Shows: average watch time per view",
    x = "",
    y = "hours",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))


#Plot 3: plotting percentage watched
ggplot(plot_data_3, aes(x = aspect, y = value, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("movie" = "red", "show" = "blue")) +
  labs(
    title = "Movies vs Shows: proportion watched",
    x = "",
    y = "percentage",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

#Plot 4: Violin plot of movies and shows and their percentage watch

ggplot(All_shows, aes(x = type, y = percentage_watched, fill = type)) +
  geom_violin(alpha = 0.5, trim = FALSE) +
  scale_fill_manual(values = c("movie" = "red", "show" = "blue")) +
  labs(
    title = "Distribution of Percentage Watched: Movies vs Shows",
    x = "Type",
    y = "Percentage Watched per Title (%)"
  ) +
  theme_minimal()

