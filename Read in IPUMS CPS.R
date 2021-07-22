
#install.packages('expss')
#install.packages('tidyverse')
#install.packages('ipumsr')
#install.packages('googledrive')

library(expss)
library(tidyverse)
library(tidycensus)
library(ipumsr)
library(googledrive)

fips <- tidycensus::fips_codes %>%
  select(state,state_code,state_name) %>%
  distinct() %>%
  rename(stusps = state,
         fips = state_code,
         state = state_name)

cps_files <- drive_find(pattern = 'cps_00003')

1

temp1 <- tempfile(fileext = '.csv.gz')
dl <- drive_download(
  as_id(cps_files[[which(cps_files$name == 'cps_00003.csv.gz'),2]]), path = temp1, overwrite = T
)

temp2 <- tempfile(fileext = '.xml')
dl <- drive_download(
  as_id(cps_files[[which(cps_files$name == 'cps_00003.xml'),2]]), path = temp2, overwrite = T
)

CPS <- read_ipums_micro(temp2, data_file = temp1)  %>%
  mutate(yearmonth = paste(YEAR, month.name[MONTH],sep = " "),
         fips = str_pad(STATEFIP,2,side = 'left',pad = '0'),
         agebin = case_when(AGE < 37 ~ 1,
                            AGE < 48 ~ 2,
                            AGE < 58 ~ 3,
                            AGE < 68 ~ 4,
                            TRUE ~ 5),
         raceth = case_when(99 < HISPAN & HISPAN < 613 ~ 1,
                            RACE == 100 ~ 2,
                            RACE == 200 ~ 3,
                            RACE == 651 ~ 4,
                            RACE == 999 & HISPAN %in% c('901','902') ~ 9,
                            TRUE ~ 5)) %>%
  inner_join(fips, by = 'fips')

