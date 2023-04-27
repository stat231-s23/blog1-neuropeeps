library(tidyverse)
library(kableExtra)
library(ggrepel)
library(broom)
library(GGally)

# Alex Yan and Lily Wickland Shearer
# data from shiny project
scatterplot_data <- readRDS("data/scatterplot_data_without_2019.Rds")

scatter_scaled <- scatterplot_data %>%
  mutate(across(where(is.numeric),  ~scale(.)[,1], .names = "{.col}_scaled")) %>%
  select(Country, year, `Happiness score_scaled`, `Freedom to make life choices_scaled`, 
         tot_year_deaths_scaled, tot_year_cases_scaled) %>%
  drop_na()



# YEAR 2020

scatter_scaled_2020 <- filter(scatter_scaled, year == 2020) %>%
  select(-Country, -year) %>%
  drop_na()

scatter_2020 <- filter(scatterplot_data, year == 2020) %>%
  select(-year) %>%
  drop_na()

set.seed(23)

# generate elbow plot data
elbow_plot <- tibble(k = 1:10) %>%
  mutate(
    kmeans_results = purrr::map(k, ~kmeans(scatter_scaled_2020, .x)),
    # List-column of "glanced" model summaries for each kmeans object
    # (apply `glance()` to each corresponding result after running `kmeans()`)
    glanced = purrr::map(kmeans_results, glance)) %>% 
  # Turn `glanced` list-column into regular tibble columns
  unnest(cols = c(glanced))

# Construct elbow plot
ggplot(elbow_plot, aes(x = k, y = tot.withinss)) +
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Number of clusters (k)", 
       y = "Total within-cluster sum of squares")
ggsave("img/elbow.png")

scatter_2020_kmeans4 <- scatter_scaled_2020 %>% 
  kmeans(centers = 4, nstart = 20)

scatter_2020_kmeans4_summary <- tidy(scatter_2020_kmeans4) 
names(scatter_2020_kmeans4_summary) = gsub(pattern = "_scaled", replacement = "", x = names(scatter_2020_kmeans4_summary))

scatter_2020_kmeans4_country <- augment(scatter_2020_kmeans4, scatter_2020) %>%
  arrange(.cluster)

ggplot(scatter_2020_kmeans4_country, aes(x = tot_year_deaths, y = get('Freedom to make life choices'))) + 
  geom_point(aes(color = .cluster, shape = .cluster)) +
  # geom_text_repel(aes(label = Country, color = .cluster), 
  #                 size = 3, max.overlaps = 203897450, show.legend = FALSE) +
  # Add centroid labels to plot
  # geom_label(data = scatter_2020_kmeans4_summary, aes(label = cluster, color = cluster),
  #            size = 3,
  #            label.r = unit(0.5, "lines"),
  #            label.size = 1.5,
  #            label.padding = unit(0.5, "lines"),
  #            show.legend = FALSE) +
  labs(x = "Total COVID Deaths",
       y = "Freedom to make life choices score",
       color = "Cluster",
       shape = "Cluster") +
  theme_classic()
ggsave("img/freedom_death_cluster20")



# YEAR 2021

scatter_scaled_2021 <- filter(scatter_scaled, year == 2021) %>%
  select(-Country, -year) %>%
  drop_na()

scatter_2021 <- filter(scatterplot_data, year == 2021) %>%
  select(-year) %>%
  drop_na()

set.seed(23)

# generate elbow plot data
elbow_plot21 <- tibble(k = 1:10) %>%
  mutate(
    kmeans_results = purrr::map(k, ~kmeans(scatter_scaled_2021, .x)),
    # List-column of "glanced" model summaries for each kmeans object
    # (apply `glance()` to each corresponding result after running `kmeans()`)
    glanced = purrr::map(kmeans_results, glance)) %>% 
  # Turn `glanced` list-column into regular tibble columns
  unnest(cols = c(glanced))

# Construct elbow plot
ggplot(elbow_plot21, aes(x = k, y = tot.withinss)) +
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Number of clusters (k)", 
       y = "Total within-cluster sum of squares")
ggsave("img/elbow21.png")

scatter_2021_kmeans4 <- scatter_scaled_2021 %>% 
  kmeans(centers = 4, nstart = 20)

scatter_2021_kmeans4_summary <- tidy(scatter_2021_kmeans4) 
names(scatter_2021_kmeans4_summary) = gsub(pattern = "_scaled", replacement = "", x = names(scatter_2021_kmeans4_summary))

scatter_2021_kmeans4_country <- augment(scatter_2021_kmeans4, scatter_2021) %>%
  arrange(.cluster)

ggplot(scatter_2021_kmeans4_country, aes(x = tot_year_deaths, y = get('Freedom to make life choices'))) + 
  geom_point(aes(color = .cluster, shape = .cluster)) +
  labs(x = "Total COVID Deaths",
       y = "Freedom to make life choices score",
       color = "Cluster",
       shape = "Cluster") +
  theme_classic()
ggsave("img/freedom_death_cluster21")



# YEAR 2022

scatter_scaled_2022 <- filter(scatter_scaled, year == 2022) %>%
  select(-Country, -year) %>%
  drop_na()

scatter_2022 <- filter(scatterplot_data, year == 2022) %>%
  select(-year) %>%
  drop_na()

set.seed(23)

# generate elbow plot data
elbow_plot22 <- tibble(k = 1:10) %>%
  mutate(
    kmeans_results = purrr::map(k, ~kmeans(scatter_scaled_2022, .x)),
    # List-column of "glanced" model summaries for each kmeans object
    # (apply `glance()` to each corresponding result after running `kmeans()`)
    glanced = purrr::map(kmeans_results, glance)) %>% 
  # Turn `glanced` list-column into regular tibble columns
  unnest(cols = c(glanced))

# Construct elbow plot
ggplot(elbow_plot22, aes(x = k, y = tot.withinss)) +
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Number of clusters (k)", 
       y = "Total within-cluster sum of squares")
ggsave("img/elbow22.png")

scatter_2022_kmeans4 <- scatter_scaled_2022 %>% 
  kmeans(centers = 4, nstart = 20)

scatter_2022_kmeans4_summary <- tidy(scatter_2022_kmeans4) 
names(scatter_2022_kmeans4_summary) = gsub(pattern = "_scaled", replacement = "", x = names(scatter_2022_kmeans4_summary))

scatter_2022_kmeans4_country <- augment(scatter_2022_kmeans4, scatter_2022) %>%
  arrange(.cluster)

ggplot(scatter_2022_kmeans4_country, aes(x = tot_year_deaths, y = get('Freedom to make life choices'))) + 
  geom_point(aes(color = .cluster, shape = .cluster)) +
  labs(x = "Total COVID Deaths",
       y = "Freedom to make life choices score",
       color = "Cluster",
       shape = "Cluster") +
  theme_classic()
ggsave("img/freedom_death_cluster22")

