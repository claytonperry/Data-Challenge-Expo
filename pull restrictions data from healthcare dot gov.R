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
  mutate(fixeddate = gsub('0020','2020',date),
         newdate = as.Date.character(substr(fixeddate,1,10),format = '%Y-%m-%d')) %>%
  select(-fixeddate,-date) %>%
  rename(date = newdate,
         state = state_id)

#explore the different policy types
summary(df$policy_type)

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
  filter(policy_type == 'Food and Drink') %>%
  select(comments) %>%
  head(5)