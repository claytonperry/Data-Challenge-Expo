#install.packages('tidyverse')
#install.packages('httr')
#install.packages('jsonlite')

library(tidyverse)
library(httr)
library(jsonlite)

path <- 'https://healthdata.gov/resource/gyqz-9u7n.json?policy_level=state&$limit=50000'

request <- GET(path)

request$status_code

response <- content(request, as = 'text', encoding = 'UTF-8')

df <- fromJSON(response, flatten = T) %>%
  data.frame


