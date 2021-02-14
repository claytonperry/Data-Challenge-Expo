setwd('C:/Users/admin/Folders/Personal Projects/2021/Economic Data/CPS ASEC')

install.packages('tidycensus')
install.packages('tidyverse')

library(tidycensus)
library(tidyverse)

census_api_key('5dc30770cc0133357b32b987a7bdd7f51e1b4a80', install = TRUE, overwrite = TRUE)

acs19 <- load_variables(year = 2019, dataset = 'acs1', cache = TRUE)
