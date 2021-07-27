
library(tidyverse)
library(lubridate)

raw <- read.csv('https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv')

restrictions <- read.csv('https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv') %>%
  filter(Jurisdiction == 'STATE_WIDE') %>%
  mutate(date = as.Date(as.character(Date),format = '%Y%m%d'),
         state = recode(RegionName, 'Washington DC' = 'District of Columbia')) %>%
  rename(C1 = C1_School.closing,
         C2 = C2_Workplace.closing,
         C3 = C3_Cancel.public.events,
         C4 = C4_Restrictions.on.gatherings,
         C5 = C5_Close.public.transport,
         C6 = C6_Stay.at.home.requirements,
         C7 = C7_Restrictions.on.internal.movement,
         C8 = C8_International.travel.controls) %>%
  mutate(across(C1:C8, function(x) ifelse(is.na(x),9,x))) %>%
  select(state,date,C1,C2,C3,C4,C5,C6,C7,C8)

summary.factor(restrictions$state)

##different set
raw2 <- read.csv('https://raw.githubusercontent.com/COVID19StatePolicy/SocialDistancing/master/data/USstatesCov19distancingpolicy.csv')


## Create state-month summarized dataframe

# Create functions to iterate over C variables

cnt_0 <- function(x, na.rm = TRUE) {sum(x == 0)}
cnt_1 <- function(x, na.rm = TRUE) {sum(x == 1)}
cnt_2 <- function(x, na.rm = TRUE) {sum(x == 2)}
cnt_3 <- function(x, na.rm = TRUE) {sum(x == 3)}
cnt_4 <- function(x, na.rm = TRUE) {sum(x == 4)}
sum1 <- function(x, na.rm = TRUE) {sum(ifelse(x < 9, x, 0))}
fctsum <- function(x, na.rm = TRUE) {sum(ifelse(x < 9, x^2, 0))}

restrictions_mnth <- restrictions %>% 
  mutate(yearmonth = paste0(year(date), ' ',month(date, label = T, abbr = F))) %>%
  group_by(state, yearmonth) %>%
  summarise(across(C1:C8, list(c0 = cnt_0, c1 = cnt_1, c2 = cnt_2, c3 = cnt_3, c4 = cnt_4, sum1 = sum1, sum2 = fctsum), na.rm = TRUE))

summary(restrictions_mnth)
summary(restrictions$C8[which(restrictions$state == 'Alabama')])
