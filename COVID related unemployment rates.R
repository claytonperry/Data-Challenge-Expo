#COVID related unemployment rates
#estimates as defined in the estimates spec

#tapply(puf_df.$PWEIGHT,list(puf_df.$RSNNOWRK,puf_df.$EST_ST),sum) -- commented this out bc not necessary

#install.packages('tidyverse') -- commenting this out bc we don't want to run this every time, but should keep commented out in final save
library(tidyverse)

#1. Num unemployed because of covid
#filter to rsnnowork and week
#summarize to sum pweight by state by week
cmt_puf1 <- puf_df %>%
  filter(RSNNOWRK %in% c(8,9,10,11) &
         WEEK %in% c(2,6,10,13,14,17,19,21,22,24,26)) %>%
  group_by(EST_ST, WEEK) %>%
  summarize(covid_unemp_n = sum(PWEIGHT))

#save to google sheet
write_sheet(cmt_puf1, ss = '1M_WzK_o4eRZm1aLTX3Ey7P-jXQsxjBwO4MXD4gdaGWw', sheet = 'Sheet1')


#Clay's Version:

#install.packages('tidyverse')
#install.packages('googlesheets4')
#library(tidyverse)
#library(googlesheets4)

#gs4_auth()

df <- puf_df %>%
  group_by(EST_ST,WEEK) %>%
  summarise(total_pop_n = sum(PWEIGHT),
            total_unemp_n = sum(PWEIGHT[ANYWORK == 2]),
            covid_unemp_n = sum(PWEIGHT[RSNNOWRK %in% c(8,9,10,11)])) %>%
  rename(week = WEEK) %>%
  mutate(fips = str_pad(EST_ST,2,side = 'left',pad = '0'),
         covid_unemp_rt = covid_unemp_n/total_pop_n,
         total_unemp_rt = total_unemp_n/total_pop_n,
         covid_unemp_prop = covid_unemp_n/total_unemp_n) %>%
  full_join(states, by = 'fips') %>%
  full_join(schedule, by = 'week') %>%
  ungroup() %>%
  select(state, week, midpoint, covid_unemp_rt, total_unemp_rt, covid_unemp_prop) %>%
  gather('measurement','value',-c(1:3))

write_sheet(df, ss = '1M_WzK_o4eRZm1aLTX3Ey7P-jXQsxjBwO4MXD4gdaGWw', sheet = 'Sheet1')

