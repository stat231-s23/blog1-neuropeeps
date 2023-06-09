library(tidyverse)
library(kableExtra)
library(ggrepel)
library(broom)
library(GGally)
library(ggplot2)
library(ggrepel)

# Alex Yan and Lily Wickland Shearer
# data from shiny project
scatterplot_data <- readRDS("data/scatterplot_data_without_2019.Rds")

# make scaled dataset for clustering
scatter_scaled <- scatterplot_data %>%
  mutate(across(where(is.numeric),  ~scale(.)[,1], .names = "{.col}_scaled")) %>%
  select(Country, year, `Happiness score_scaled`, `Freedom to make life choices_scaled`, 
         tot_year_deaths_scaled, tot_year_cases_scaled) %>%
  drop_na()


# YEAR 2020

# filter by year == 2020
scatter_scaled_2020 <- filter(scatter_scaled, year == 2020) %>%
  select(-Country, -year) %>%
  drop_na()

# for later dataframe joining w/ clusters
scatter_2020 <- filter(scatterplot_data, year == 2020) %>%
  select(-year) %>%
  drop_na()

set.seed(23)

# generate elbow plot data
elbow_plot20 <- tibble(k = 1:10) %>%
  mutate(
    kmeans_results = purrr::map(k, ~kmeans(scatter_scaled_2020, .x)),
    # List-column of "glanced" model summaries for each kmeans object
    # (apply `glance()` to each corresponding result after running `kmeans()`)
    glanced = purrr::map(kmeans_results, glance)) %>% 
  # Turn `glanced` list-column into regular tibble columns
  unnest(cols = c(glanced))

# Construct elbow plot, k = 4
ggplot(elbow_plot20, aes(x = k, y = tot.withinss)) +
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Number of clusters (k)", 
       y = "Total within-cluster sum of squares")
ggsave("img/elbow20.png", scale = 1)

# generate clusters and join tables
scatter_2020_kmeans4 <- scatter_scaled_2020 %>% 
  kmeans(centers = 4, nstart = 20)
# join clustered data with country names
scatter_2020_kmeans4_country <- augment(scatter_2020_kmeans4, scatter_2020) %>%
  arrange(.cluster)

# plot clusters
ggplot(scatter_2020_kmeans4_country, aes(x = tot_year_cases, y = get('Freedom to make life choices'))) + 
  geom_point(aes(color = .cluster, shape = .cluster)) +
  labs(x = "COVID Cases (per million)",
       y = "Freedom to make life choices score",
       color = "Cluster",
       shape = "Cluster") +
  theme_classic() +
  # label countries of interest
  geom_text_repel(aes(label = ifelse(Country == "Lebanon" | 
                                       Country == "Switzerland" | 
                                       Country == "United Kingdom" |
                                       Country == "Panama" |
                                       Country == "Mongolia" |
                                       Country == "Venezuela" |
                                       Country == "Senegal",
                                     as.character(Country), "")))
# save visualization to png
ggsave("img/freedom_death_cluster20.png", scale = 0.8)



# YEAR 2021 SAME PROCESS AS 2020

# filter by year == 2021
scatter_scaled_2021 <- filter(scatter_scaled, year == 2021) %>%
  select(-Country, -year) %>%
  drop_na()

# for later dataframe joining w/ clusters
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

# Construct elbow plot, k = 4
ggplot(elbow_plot21, aes(x = k, y = tot.withinss)) +
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Number of clusters (k)", 
       y = "Total within-cluster sum of squares")
ggsave("img/elbow21.png", scale = 1)

# generate clusters and join tables
scatter_2021_kmeans4 <- scatter_scaled_2021 %>% 
  kmeans(centers = 4, nstart = 20)
scatter_2021_kmeans4_country <- augment(scatter_2021_kmeans4, scatter_2021) %>%
  # need to keep clustering the same pattern as 2020, so renumber clusters
  mutate(cluster = case_when(.cluster == 1 ~ 2,
                             .cluster == 2 ~ 3,
                             .cluster == 3 ~ 4,
                             .cluster == 4 ~ 1
  )) %>%
  select(-.cluster) %>%
  arrange(cluster) %>%
  mutate(cluster = as.factor(cluster))

# plot clusters
ggplot(scatter_2021_kmeans4_country, aes(x = tot_year_cases, y = get('Freedom to make life choices'))) + 
  geom_point(aes(color = cluster, shape = cluster)) +
  labs(x = "COVID Cases (per million)",
       y = "Freedom to make life choices score",
       color = "Cluster",
       shape = "Cluster") +
  theme_classic() +
  # countries of interest
  geom_text_repel(aes(label = ifelse(Country == "Lebanon" | 
                                       Country == "Switzerland" | 
                                       Country == "United Kingdom" |
                                       Country == "Panama" |
                                       Country == "Mongolia" |
                                       Country == "Venezuela" |
                                       Country == "Senegal",
                                     as.character(Country), "")))
# save visualization to png
ggsave("img/freedom_death_cluster21.png", scale = 0.8)



# YEAR 2022 SAME PROCESS AS 2021

# filter by year == 2022
scatter_scaled_2022 <- filter(scatter_scaled, year == 2022) %>%
  select(-Country, -year) %>%
  drop_na()

# for joining dataframes later
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
ggsave("img/elbow22.png", scale = 1)

# generated clusters, merge dataframes
scatter_2022_kmeans4 <- scatter_scaled_2022 %>% 
  kmeans(centers = 4, nstart = 20)
scatter_2022_kmeans4_country <- augment(scatter_2022_kmeans4, scatter_2022) %>%
  # need to keep clustering the same pattern as 2020, so renumber clusters
  mutate(cluster = case_when(.cluster == 1 ~ 4,
                              .cluster == 2 ~ 1,
                              .cluster == 3 ~ 3,
                              .cluster == 4 ~ 2
                              )) %>%
  select(-.cluster) %>%
  arrange(cluster) %>%
  mutate(cluster = as.factor(cluster))
    
# plot clusters
ggplot(scatter_2022_kmeans4_country, aes(x = tot_year_cases, y = get('Freedom to make life choices'))) + 
  geom_point(aes(color = cluster, shape = cluster)) + 
  labs(x = "COVID Cases (per million)",
       y = "Freedom to make life choices score",
       color = "Cluster",
       shape = "Cluster") +
  theme_classic() +
  # countries of interest
  geom_text_repel(aes(label = ifelse(Country == "Lebanon" | 
                         Country == "Switzerland" | 
                         Country == "United Kingdom" |
                         Country == "Panama" |
                         Country == "Mongolia" |
                         Country == "Venezuela" |
                         Country == "Senegal",
                         as.character(Country), "")))
# save visualization as png
ggsave("img/freedom_death_cluster22.png", scale = 0.8)
 

