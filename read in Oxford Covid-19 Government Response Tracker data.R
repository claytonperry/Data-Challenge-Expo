
library(tidyverse)
library(lubridate)

raw <- read.csv('https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv')

restrictions <- read.csv('https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv') %>%
  filter(Jurisdiction == 'STATE_WIDE') %>%
  mutate(date = as.Date(as.character(Date),format = '%Y%m%d'),
         state = recode(RegionName, 'Washington DC' = 'District of Columbia'),
         mon = month(date),
         yr = year(date)) %>%
  rename(C1 = C1_School.closing,
         C2 = C2_Workplace.closing,
         C3 = C3_Cancel.public.events,
         C4 = C4_Restrictions.on.gatherings,
         C5 = C5_Close.public.transport,
         C6 = C6_Stay.at.home.requirements,
         C7 = C7_Restrictions.on.internal.movement,
         C8 = C8_International.travel.controls) %>%
  mutate(across(C1:C8, function(x) ifelse(is.na(x),9,x)),
         C1_as1 = 0,
         C2_as1 = 0,
         C3_as1 = 0,
         C4_as1 = 0,
         C5_as1 = 0,
         C6_as1 = 0,
         C7_as1 = 0,
         C8_as1 = 0,
         C1_as2 = case_when(yr == 2020 & mon < 6 ~ 0,
                            yr == 2020 & mon == 6 ~ 1,
                            yr == 2020 & mon == 7 ~ 2,
                            TRUE ~ 3),
         C2_as2 = case_when(yr == 2020 & mon < 6 ~ 0,
                            yr == 2020 & mon == 6 ~ 1,
                            yr == 2020 & mon == 7 ~ 2,
                            TRUE ~ 3),
         C3_as2 = case_when(yr == 2020 & mon < 6 ~ 0,
                            yr == 2020 & mon == 6 ~ 1,
                            TRUE ~ 2),
         C4_as2 = case_when(yr == 2020 & mon < 6 ~ 0,
                            yr == 2020 & mon == 6 ~ 1,
                            yr == 2020 & mon == 7 ~ 2,
                            yr == 2020 & mon == 8 ~ 3,
                            TRUE ~ 4),
         C5_as2 = case_when(yr == 2020 & mon < 6 ~ 0,
                            yr == 2020 & mon == 6 ~ 1,
                            TRUE ~ 2),
         C6_as2 = case_when(yr == 2020 & mon < 6 ~ 0,
                            yr == 2020 & mon == 6 ~ 1,
                            yr == 2020 & mon == 7 ~ 2,
                            TRUE ~ 3),
         C7_as2 = case_when(yr == 2020 & mon < 6 ~ 0,
                            yr == 2020 & mon == 6 ~ 1,
                            TRUE ~ 2),
         C8_as2 = case_when(yr == 2020 & mon < 6 ~ 0,
                            yr == 2020 & mon == 6 ~ 1,
                            yr == 2020 & mon == 7 ~ 2,
                            yr == 2020 & mon == 8 ~ 3,
                            TRUE ~ 4)) %>%
  select(state,date,mon,yr,
         C1,C2,C3,C4,C5,C6,C7,C8,
         C1_as1,C2_as1,C3_as1,C4_as1,C5_as1,C6_as1,C7_as1,C8_as1,
         C1_as2,C2_as2,C3_as2,C4_as2,C5_as2,C6_as2,C7_as2,C8_as2)

#QC
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(restrictions, row.vars = "mon", col.vars = "C1_as2", type = "f")
summary.factor(restrictions$C1_fs1)
summary.factor(restrictions$C1)
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
  summarise(across(C1:C8, list(c0 = cnt_0, c1 = cnt_1, c2 = cnt_2, c3 = cnt_3, c4 = cnt_4, sum1 = sum1, sum2 = fctsum), na.rm = TRUE),
            across(C1_as1:C8_as1, list(sum1 = sum1), na.rm = TRUE),
            across(C1_as2:C8_as2, list(sum1 = sum1), na.rm = TRUE))

summary(restrictions_mnth)
summary(restrictions$C8[which(restrictions$state == 'Alabama')])
