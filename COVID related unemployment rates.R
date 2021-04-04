#COVID related unemployment rates
#estimates of no work by state per week
#HH Pulse Survey Data and Rep Weights

tapply(puf_df.$PWEIGHT,list(puf_df.$RSNNOWRK,puf_df.$EST_ST),sum)

install.packages('tidyverse')
library(tidyverse)

#1. Num unemployed because of covid
#filter to rsnnowork
cmt_puf1 <- puf_df %>%
  filter(RSNNOWRK %in% c(8,9,10,11))
#summarize to sum pweight by state by week
covid_unemp <- cmt_puf1 %>%
  group_by(EST_ST, WEEK) %>%
  summarize(covid_unemp_n = sum(PWEIGHT))


#2. Total unemployed
#filter to unemployed (what's the variable)
cmt_puf2 <- puf_df %>%
  filter(ANYWORK == 2)
#summarize to sum pweight by state by week
total_unemp <- cmt_puf2 %>%
  group_by(EST_ST, WEEK) %>%
  summarize(total_unemp_n = sum(PWEIGHT))


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

#check values
min(total2$covid_unemp_rt)
max(total2$covid_unemp_rt)
min(total2$total_unemp_rt)
max(total2$total_unemp_rt)


#save to google sheet
write_sheet(total2,ss = '1M_WzK_o4eRZm1aLTX3Ey7P-jXQsxjBwO4MXD4gdaGWw', sheet = 'Sheet1')
