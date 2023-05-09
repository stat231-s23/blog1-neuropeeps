## TO CREATE MAP
world_map <- map_data(map = "world"
                      , region = ".")

world_map <- rename(world_map, Country = region) %>%
  mutate(Country = gsub("USA", "United States", Country))

# then merge with your data -- final dataset should be multiple rows per
# country and year, with variable lat, long, and group

ggplot(data = world_happiness_map
       , aes(x = long, y = lat, group = group
             , fill = `freedom to make choices`)) +
  geom_polygon()+
  theme_void() + # removes axes
  # more informative plot/legend/caption titles
  labs(title = "World Freedom to Make Life Choice: {closest_state}"
       , caption = "Data from Our World in Data and World Happiness Report (2019-2022)"
       , fill = "Freedom to Make Life Choices Score") +
  # coloring to make chloropleth more readable
  scale_fill_viridis(option = "mako", direction = -1) +
  theme(legend.position = "bottom") +
  transition_states(
    year, # Animates based on year
    transition_length = 1, # Sets transition length of 1
    state_length = 5 # Sets statelength to 5
  )
