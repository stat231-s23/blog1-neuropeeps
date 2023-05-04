library(tidyverse)
library(kableExtra)
library(rvest)
library(robotstxt)
library(purrr)

## SWITZERLAND 

# 1. Identify page where poem is listed
russia_covid_policy_url <- "https://ru.usembassy.gov/covid-19-information/#:~:text=Anyone%20testing%20positive%20for%20COVID,directly%20after%20arrival%20in%20Russia."
# 2. Confirm bots are allowed to access the page 
robotstxt::paths_allowed(russia_covid_policy_url)
# # 3. Get poem text 
# dear_march <- dear_march_url %>%               
#   read_html() %>%
#   html_elements(".poem") %>% 
#   html_text2() 

# 1. Identify page where poem is listed
united_states_covid_policy_url <- "https://www.cdc.gov/coronavirus/2019-ncov/cdcresponse/laws-regulations.html"
# 2. Confirm bots are allowed to access the page 
robotstxt::paths_allowed(united_states_covid_policy_url)
# # 3. Get poem text 
# dear_march <- dear_march_url %>%               
#   read_html() %>%
#   html_elements(".poem") %>% 
#   html_text2() 



# # Explicitly load `stop_words` into environment
# data(stop_words)
# 
# # First, take a look at the `stop_words` dataset
# head(stop_words)
# tail(stop_words)
# 
# stop_words %>% 
#   count(lexicon)
# 
# poems_words <- poems_words_all %>%
#   anti_join(stop_words, by="word")


## UNITED KINGDOM




## MONGOLIA




## SENEGAL 




## PANAMA




## VENEZUALA




## LEBANON