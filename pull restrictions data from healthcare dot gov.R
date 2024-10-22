#install.packages('tidyverse')
#install.packages('httr')
#install.packages('jsonlite')

library(tidyverse)
library(httr)
library(jsonlite)

path <- 'https://healthdata.gov/resource/gyqz-9u7n.json?policy_level=state&$limit=50000'

request <- GET(path)

#if this returns anything other than '200' there is an issue with the PATH
request$status_code

response <- content(request, as = 'text', encoding = 'UTF-8')

raw <- fromJSON(response, flatten = T) %>%
  data.frame

df <- raw %>%
  mutate(date = as.Date.character(substr(gsub('0020','2020',date),1,10),format = '%Y-%m-%d')) %>%
  rename(state = state_id)

#explore the different policy types
summary.factor(df$policy_type)

df %>%
  filter(date <= '2020-03-21',) %>%
  select(policy_type) %>%
  summary.factor(maxsum = 20)

df %>%
  filter(date <= '2020-03-21',
         policy_type == 'Non-Essential Businesses') %>%
  select(comments) %>%
  summary.factor(maxsum = 20)

df %>%
  filter(date <= '2020-03-21',
         policy_type == 'Food and Drink') %>%
  select(comments) %>%
  summary.factor(maxsum = 20)

df %>%
  filter(date <= '2020-03-21',
         policy_type == 'State of Emergency') %>%
  select(comments) %>%
  summary.factor(maxsum = 20)





