library(tidyverse)
library(kableExtra)
library(rvest)
library(robotstxt)
library(purrr)
library(tidytext)

texts <- readRDS("data/texts.Rds")
scatter <- readRDS("data/scatterplot_data_without_2019.Rds")

# extract all words from the texts
text_words_all <- texts %>%
  unnest_tokens(output = word, input = quote)

# remove stopwords and numbers
text_words <- text_words_all %>%
  anti_join(stop_words, by="word") %>%
  filter(is.na(as.numeric(word)))



## SENTIMENT ANALYSIS
# load sentiment scores
afinn_lexicon <- get_sentiments("afinn")

# function to calculate average sentiment
avg_sent <- function(country) {
  # filter words from input country
  words <- filter(text_words, grepl(country, source)) %>%
    count(word, sort = TRUE)
  # assign sentiments to each word, average them across all words
  words %>%
    inner_join(afinn_lexicon, by = "word") %>%
    mutate(sentiment_net = n * value) %>%
    summarize(sentiment = sum(sentiment_net)/sum(n)) %>%
    mutate(source = country)
}

#list of countries
countries <- c('uk','leb','mong','sen','pan','switz')

# apply avg_sent to all countries, convert to df
all_sent <- sapply(countries, avg_sent) %>%
  as.data.frame()
# transpose dataframe
all_sent <- t(all_sent)
# remove row names
rownames(all_sent) <- NULL
# rename to full names of countries
country <- c("United Kingdom", "Lebanon", "Mongolia", "Senegal", "Panama", "Switzerland") 
all_sent <- cbind(all_sent, country %>% as.data.frame()) %>%
  rename(Country = ".") %>%
  select(Country, sentiment)

# join sentiments with happiness dataset
scatter_summary <- scatter %>% 
  filter(Country %in% country) %>% 
  group_by(Country) %>%
  summarize(happiness = mean(get('Happiness score')), freedom = mean(get('Freedom to make life choices')))

sent_happ_data <- inner_join(all_sent, scatter_summary, by = "Country") %>%
  mutate(sentiment = as.numeric(sentiment)) %>%
  mutate(across(where(is.numeric),  ~scale(.)[,1], .names = "{.col}_scaled")) %>%
  pivot_longer(cols = c('sentiment_scaled','happiness_scaled'),
               names_to = "variable",
               values_to = "value")

ggplot(data = sent_happ_data, aes(fill = variable, x = Country, value)) +
  geom_bar(stat = "identity", position = "dodge")
