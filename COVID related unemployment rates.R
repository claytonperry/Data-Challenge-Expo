#COVID related unemployment rates
#estimates of no work by state per week
#HH Pulse Survey Data and Rep Weights

#tapply(puf_df.$PWEIGHT,list(puf_df.$RSNNOWRK,puf_df.$EST_ST),sum) -- commented this out bc not necessary

#install.packages('tidyverse') -- commenting this out bc we don't want to run this every time, but should keep commented out in final save
library(tidyverse)

#1. Num unemployed because of covid
#filter to rsnnowork
cmt_puf1 <- puf_df %>%
  filter(RSNNOWRK %in% c(8,9,10,11))
#summarize to sum pweight by state by week
covid_unemp <- cmt_puf1 %>%
  group_by(EST_ST, WEEK) %>%
  summarize(covid_unemp_n = sum(PWEIGHT))
#CP: ^^this can all be piped together

#2. Total unemployed
#filter to unemployed (what's the variable)
cmt_puf2 <- puf_df %>%
  filter(ANYWORK == 2)
#summarize to sum pweight by state by week
total_unemp <- cmt_puf2 %>%
  group_by(EST_ST, WEEK) %>%
  summarize(total_unemp_n = sum(PWEIGHT))
#CP: ^^this can all be piped together

#3. Total pop
#summarize to sum pweight by state by week
total_pop <- puf_df %>%
  group_by(EST_ST, WEEK) %>%
  summarize(total_pop_n = sum(PWEIGHT))


#combine all data frames
#calculate total unemployment rate (2/3)
#calculate unemployment by covid (1/3)
total1 <- full_join(covid_unemp,total_unemp, by = c('EST_ST'='EST_ST', 'WEEK'='WEEK')) %>%
  select(EST_ST,WEEK,total_unemp_n,covid_unemp_n)
total2 <- full_join(total1,total_pop, by = c('EST_ST'='EST_ST', 'WEEK'='WEEK')) %>%
  select(EST_ST,WEEK,total_unemp_n,covid_unemp_n,total_pop_n) %>%
  mutate(covid_unemp_rt = covid_unemp_n/total_pop_n,
         total_unemp_rt = total_unemp_n/total_pop_n)

#CP: Need to add a gather step at the end to make the data long but otherwise this is great!

#CP: Check out my version at the bottom that omits any join steps

# ^^ this can all be one step
#check values
min(total2$covid_unemp_rt)
max(total2$covid_unemp_rt)
min(total2$total_unemp_rt)
max(total2$total_unemp_rt)


#save to google sheet
write_sheet(total2, ss = '1M_WzK_o4eRZm1aLTX3Ey7P-jXQsxjBwO4MXD4gdaGWw', sheet = 'Sheet1')


#Clay's Version:

#install.packages('tidyverse')
#install.packages('googlesheets4')
#library(tidyverse)
#library(googlesheets4)

#gs4_auth()

#df <- puf_df %>%
#  group_by(EST_ST,WEEK) %>%
#  summarise(total_pop_n = sum(PWEIGHT),
#            total_unemp_n = sum(PWEIGHT[ANYWORK == 2]),
#            covid_unemp_n = sum(PWEIGHT[RSNNOWRK %in% c(8,9,10,11)])) %>%
#  rename(week = WEEK) %>%
#  mutate(fips = str_pad(EST_ST,2,side = 'left',pad = '0'),
#         covid_unemp_rt = covid_unemp_n/total_pop_n,
#         total_unemp_rt = total_unemp_n/total_pop_n,
#         covid_unemp_prop = covid_unemp_n/total_unemp_n) %>%
#  full_join(states, by = 'fips') %>%
#  ungroup() %>%
#  select(state, week, covid_unemp_rt, total_unemp_rt, covid_unemp_prop) %>%
#  gather('measurement','value',-c(1:2))

#write_sheet(df, ss = '1M_WzK_o4eRZm1aLTX3Ey7P-jXQsxjBwO4MXD4gdaGWw', sheet = 'Sheet1')

