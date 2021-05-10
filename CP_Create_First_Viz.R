#Run these programs beforehand:
#  1. Pull HH Pulse Data into R.R
#  2. Pull Covid Data.R
#  3. Pull_BLS_Data.R

states <- unique(tidycensus::fips_codes[,1:3])[0:51,] %>%
  rename(stusps = state,
         fips = state_code,
         state = state_name)

df <- puf_df %>%
  group_by(EST_ST,WEEK) %>%
  summarise(total_pop_n = sum(PWEIGHT),
            total_unemp_n = sum(PWEIGHT[ANYWORK == 2]),
            covid_unemp_n = sum(PWEIGHT[RSNNOWRK %in% c(8,9,10,11)])) %>%
  rename(week = WEEK) %>%
  mutate(fips = str_pad(EST_ST,2,side = 'left',pad = '0')) %>%
  full_join(states, by = 'fips') %>%
  inner_join(schedule, by = 'week') %>%
  inner_join(civtotal, by = c('yearmonth','state')) %>%
  rename(civpop = value) %>%
  mutate(covid_unemp_rt = covid_unemp_n/civpop,
         total_unemp_rt = total_unemp_n/civpop,
         covid_unemp_prop = covid_unemp_n/total_unemp_n) %>%
  ungroup() %>%
  select(state, yearmonth, monthname, covid_unemp_rt, total_unemp_rt, covid_unemp_prop) %>%
  gather('empmeasurement','empvalue',-c(1:3)) %>%
  inner_join(confmonthly, by = c('yearmonth', 'state')) %>%
  inner_join(deathmonthly, by = c('yearmonth', 'state')) %>%
  gather('covidmeasure','covidvalue', -c(1:5))

write_sheet(df, ss = '1M_WzK_o4eRZm1aLTX3Ey7P-jXQsxjBwO4MXD4gdaGWw', sheet = 'Sheet1')

