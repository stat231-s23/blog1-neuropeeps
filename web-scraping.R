library(tidyverse)
library(kableExtra)
library(rvest)
library(robotstxt)
library(purrr)

scrape_p <- function(url) {
  temp <- deparse(substitute(url))
  url %>%
    read_html() %>%
    html_elements("p") %>%
    html_text2() %>%
    as.data.frame() %>%
    rename(quote = ".") %>%
    mutate(source = temp) %>%
    filter(grepl('[a-zA-Z]', quote))
}

uk_url_1 <- "https://www.bma.org.uk/advice-and-support/covid-19/what-the-bma-is-doing/the-public-health-response-by-uk-governments-to-covid-19"
uk_1_text <- scrape_p(uk_url_1)

uk_url_2 <- "https://en.wikipedia.org/wiki/COVID-19_pandemic_in_the_United_Kingdom#Government"
uk_2_text <- scrape_p(uk_url_2)

switz_url_1 <- "https://blog.petrieflom.law.harvard.edu/2020/05/14/switzerland-global-responses-covid19/"
switz_1_text <- scrape_p(switz_url_1) %>%
  filter(row_number() <= n() - 2)

switz_url_2 <- "https://www.reuters.com/business/healthcare-pharmaceuticals/swiss-government-decides-lift-nearly-all-covid-19-restrictions-2022-02-16/"
switz_2_text <- scrape_p(switz_url_2) %>%
  filter(row_number() <= n() - 2)

mong_url_1 <- "https://preventepidemics.org/epidemics-that-didnt-happen-2021/covid-19-mongolia/"
mong_1_text <- scrape_p(mong_url_1) %>%
  filter(row_number() <= n() - 5)

sen_url_1 <- "https://preventepidemics.org/epidemics-that-didnt-happen-2021/covid-19-senegal/"
sen_1_text <- scrape_p(sen_url_1) %>%
  filter(row_number() <= n() - 5)

pan_url_1 <- "https://www.oecd.org/coronavirus/policy-responses/covid-19-in-latin-america-and-the-caribbean-an-overview-of-government-responses-to-the-crisis-0a2dee41/"
pan_1_text <- scrape_p(pan_url_1) %>%
  filter(!grepl('[[]', quote)) %>%
  filter(row_number() <= n() - 4 & row_number() != 3)

pan_url_2 <- "https://www.atlanticcouncil.org/blogs/new-atlanticist/panamas-coronavirus-response-must-not-affect-constitutional-order/"
pan_2_text <- scrape_p(pan_url_2) %>%
  filter(!grepl('By|\\{', quote)) %>%
  filter(row_number() <= n() - 1)

pan_url_3 <- "https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Panama"
pan_3_text <- scrape_p(pan_url_3)

leb_url_1 <- "https://www.rand.org/blog/2022/05/lebanon-challenges-and-successes-in-covid-19-pandemic.html"
leb_1_text <- scrape_p(leb_url_1) %>%
  filter(row_number() > 10) %>%
  filter(row_number() <= n() - 14)

scraped_texts <- rbind(leb_1_text, mong_1_text, pan_1_text, pan_2_text, pan_3_text,
                       sen_1_text, switz_1_text, switz_2_text, uk_1_text, uk_2_text)
saveRDS(scraped_texts, "data/texts.Rds")
