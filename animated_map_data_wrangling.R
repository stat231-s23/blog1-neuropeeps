library(tidyverse)
library(maps)
library(sf)
library(viridis)

# DESCRIPTION
# This dataset contains data that will allow us to display an interactive choropleth
# of the world, where various "happiness" variables (life expectancy, gdp, freedom)
# will be displayed with shading in all countries of the world.
# The user will be able to see all countries of the world and their corresponding 
# coloring while also being able to toggle between the different variables that are displayed
# In addition, hovering over a country or clicking will display its exact value.

###############
# import data #
###############

# select the columns needed for final table
# score, GDP, social support, healthy life expectancy,
# Freedom to make choices, generosity, corruptness

# 2022 data
whr_22 <- read_csv("C:/Users/Lily/Documents/GitHub/stat231-data-science/shiny1-neuro-peeps/data_prewrangled/world-happiness-report-2022.csv") %>%
  select('Country','Happiness score', 'Explained by: GDP per capita','Explained by: Social support',
         'Explained by: Healthy life expectancy','Explained by: Freedom to make life choices',
         'Explained by: Generosity','Explained by: Perceptions of corruption') %>%
  mutate(year = 2022)

# 2021 data
whr_21 <- read_csv("C:/Users/Lily/Documents/GitHub/stat231-data-science/shiny1-neuro-peeps/data_prewrangled/world-happiness-report-2021.csv") %>%
  select('Country name','Ladder score', 'Logged GDP per capita','Explained by: Social support',
         'Explained by: Healthy life expectancy','Explained by: Freedom to make life choices',
         'Explained by: Generosity','Explained by: Perceptions of corruption') %>%
  mutate(year = 2021)

# 2020 data
whr_20 <- read_csv("C:/Users/Lily/Documents/GitHub/stat231-data-science/shiny1-neuro-peeps/data_prewrangled/world-happiness-report-2020.csv") %>%
  select('Country name','Ladder score', 'Logged GDP per capita','Explained by: Social support',
         'Explained by: Healthy life expectancy','Explained by: Freedom to make life choices',
         'Explained by: Generosity','Explained by: Perceptions of corruption') %>%
  mutate(year = 2020)
# 2019 data
whr_19 <- read_csv("C:/Users/Lily/Documents/GitHub/stat231-data-science/shiny1-neuro-peeps/data_prewrangled/world-happiness-report-2019.csv") %>%
  select(-'Overall rank') %>%
  mutate(year = 2019)

world_map <- map_data(map = "world"
                      , region = ".")

# map data for world map
world_map <- rename(world_map, Country = region) %>%
  mutate(Country = gsub("USA", "United States", Country))


# function to rename every column in each dataframe
# change format of text to make it mergeable
rename <- function(data) {
  names(data) <- c("Country", "Score", "GDP per capita", "social support",
                   "life expectancy", "freedom to make choices", "generosity",
                   "corruption", "year")
  # merge with world map data, so drawing of country is associated
  # with happiness levels
  data <- merge(data, world_map, by = "Country")
  return(data)
}

# storing all data frames in a list
# lapply runs the function on each dataframe in the list
dfs <- list(whr_19, whr_20, whr_21, whr_22)
dfs <- lapply(dfs, rename)

# combine all years into one dataset
whr_comb <- dfs %>% 
  reduce(rbind) 

# output
saveRDS(whr_comb, "C:/Users/Lily/Documents/GitHub/stat231-data-science/blog1-neuropeeps/data/world_happiness_map2.rds")