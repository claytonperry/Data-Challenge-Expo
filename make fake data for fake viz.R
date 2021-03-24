#Making fake data for fake visualization
library(tidyverse)
library(googledrive)
library(googlesheets4)
drive_auth()
gs4_auth()


st_abbrevs <- read.csv('https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv')

states <- data.frame(st_abbrevs$Abbreviation,1)

years <- data.frame(c('2020','2021'),1)

months <- data.frame(as.character(seq(from = 1, to = 12)),1)

df1 <- full_join(states,years, by = 'X1')
df2 <- full_join(df1,months, by = 'X1') %>%
  rename(state = st_abbrevs.Abbreviation,
         month = as.character.seq.from...1..to...12..,
         year = c..2020....2021..) %>%
  select(-X1) %>%
  mutate(case_prop = runif(1224,min=0,max=0.08),
         death_prop = runif(1224,min=0,max=case_prop*.01),
         unemp_rate = runif(1224,min=0.01,max=0.1)) %>%
  gather(key = "measurement", "value", -c(1:3))

write_sheet(df2,ss = '1quOOZ_JeNH10GUAhTxLBWgD_aIXeLdOdaZ8-T_KH6Eg', sheet = 'Sheet1')
