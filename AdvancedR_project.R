rm(list = ls())
library(tidytuesdayR)
library(tidyverse)
movies = read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/movies.csv")
glimpse(movies)
shows = read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/shows.csv")
glimpse(shows)

#drop the first column for both movies and shows
movies <- movies[,-1]
shows <- shows[, -1]

#create a column called type for both movies and shows
#movies <- movies %>% mutate(type = 'movie')
movies <- mutate(movies, type = 'movie')
shows <- mutate(movies, type = 'shows')

##Joining the movies and shows in one df
netflix <- bind_rows(
  shows %>% mutate(type = "Show"),
  movies %>% mutate(type = "Movie")
)
#or
#movies_shows <-full_join(movies, shows)

#check str and summary
str(netflix)
summary(netflix)

#check for duplicates and use only distinct values
netflix <- netflix %>%
  distinct()

#look out at the number of NAs in the data
colSums(is.na(netflix))


##spliting release date into year, month and day
netflix = netflix %>%
  separate(release_date, into = c("release_year", "release_month", "release_day"),
           sep = "-", convert = TRUE)


##Group the reports into 6 periods

netflix <- netflix %>%
  mutate(period = case_when(
    grepl("2025Jan-Jun", report) ~ "Period 4",
    grepl("2024Jul-Dec", report) ~ "Period 3",
    grepl("2024Jan-Jun", report) ~ "Period 2",
    grepl("2023Jul-Dec", report) ~ "Period 1",
    TRUE ~ NA_character_
  ))

#Convert runtime to hours and minutes
netflix <- netflix %>%
  mutate(runtime_minutes = str_extract(runtime, "\\d+(?=H)") %>% as.numeric() * 60 + 
           str_extract(runtime, "\\d+(?=M)") %>% as.numeric() + 
           str_extract(runtime, "\\d+(?=S)") %>% as.numeric()/60) %>%
  mutate(runtime_minutes = replace_na(runtime_minutes, 0)) %>%
  mutate(runtime_hours = runtime_minutes / 60)

#make plots
library(ggplot2)
theme_set(theme_bw(18))
library(cowplot)

# Main plot
p <- ggplot(netflix) +
  geom_boxplot(aes(x = period, y = views /1e9, colour = type)) +
  labs(
    x = 'Period',
    y = 'Views in Billions',
    title = 'Movies and Shows Views Within a Period'
  ) +
  theme_minimal(base_size = 14)

# Make a text-only "legend"
legend_text <- ggdraw() +
  draw_label(
    "Legend:\nPeriod 1 = 2023 Jul–Dec\nPeriod 2 = 2024 Jan–Jun\nPeriod 3 = 2024 Jul–Dec\nPeriod 4 = 2025 Jan–Jun",
    x = 0, hjust = 0, size = 12
  )

# Combine plot and legend side-by-side
plot_grid(p, legend_text, ncol = 2, rel_widths = c(3, 1))


#year vs views       
ggplot(netflix) +
  geom_line(aes(release_year, views/1e9, colour = type)) + 
  labs(x = 'year',
       y = 'views in billions', 
       title = 'movies and shows views within a period')

#Globally available or not
netflix <- netflix %>% filter(!is.na(available_globally))

netflix %>%
  group_by(type, available_globally) %>%
  summarise(total_hours = sum(hours_viewed, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = type, y = total_hours, fill = available_globally)) +
  geom_col(position = "dodge") +
  labs(
    title = "Global vs. Regional Hours Viewed",
    x = "Type",
    y = "Total Hours Viewed",
    fill = "Available Globally"
  ) +
  scale_fill_manual(values = c("No" = "red", "Yes" = "blue"))


#Correlation heat map for both shows and movies
numeric_data <- netflix %>%
  select(hours_viewed, runtime_hours, views) %>%
  na.omit()

cor_matrix <- cor(numeric_data)

ggplot(melt(cor_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(x = NULL, y = NULL,
    title = "Correlation Matrix for all")


#heat map
corr_long <-
  netflix %>%
  filter(type %in% c("Show","Movie")) %>%
  group_split(type) %>%
  lapply(function(d){
    m <- cor(select(d, hours_viewed, runtime_hours, views), use = "complete.obs")
    out <- melt(m); out$type <- unique(d$type); out
  }) %>%
  bind_rows()

ggplot(corr_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(limits = c(-1, 1),
                       low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(title = "Correlation Matrix for shows and movies", x = NULL, y = NULL) +
  facet_wrap(~ type) +
  theme_minimal(base_size = 14)


