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
  summarise(hhp_total_unemp_n = sum(PWEIGHT[ANYWORK == 2]),
            hhp_covid_unemp_n = sum(PWEIGHT[RSNNOWRK %in% c(8,9,10,11)])) %>%
  rename(week = WEEK) %>%
  mutate(fips = str_pad(EST_ST,2,side = 'left',pad = '0')) %>%
  full_join(states, by = 'fips') %>%
  inner_join(schedule, by = 'week') %>%
  inner_join(civtotal, by = c('yearmonth','state')) %>%
  rename(bls_civpop = value) %>%
  mutate(hhp_total_unemp_rt = hhp_total_unemp_n/bls_civpop,
         hhp_covid_unemp_rt = hhp_covid_unemp_n/bls_civpop,
         hhp_covid_unemp_prop = hhp_covid_unemp_n/hhp_total_unemp_n) %>%
  inner_join(unemprate, by = c('yearmonth','state')) %>%
  rename(bls_unemp_rt = value) %>%
  inner_join(unemplevel, by = c('yearmonth','state')) %>%
  rename(bls_unemp_n = value) %>%
  mutate(bls_unemp_rt = bls_unemp_rt *0.01,
         bls_unemp_rt_calc = bls_unemp_n/bls_civpop, 
         bls_hhp_unemp_prop = bls_unemp_n/hhp_total_unemp_n,
         adj_covid_unemp_n = bls_hhp_unemp_prop*hhp_covid_unemp_n,
         adj_covid_unemp_rt = adj_covid_unemp_n/bls_civpop,
         adj_covid_unemp_prop = adj_covid_unemp_n/bls_unemp_n) %>%
  ungroup() %>%
  select(state, yearmonth, monthname, 
         hhp_covid_unemp_rt, hhp_total_unemp_rt, hhp_covid_unemp_prop,
         bls_unemp_rt, bls_unemp_rt_calc,
         adj_covid_unemp_rt, adj_covid_unemp_prop) %>%
  gather('empmeasurement','empvalue',-c(1:3)) %>%
  inner_join(confmonthly, by = c('yearmonth', 'state')) %>%
  inner_join(deathmonthly, by = c('yearmonth', 'state')) %>%
  gather('covidmeasure','covidvalue', -c(1:5))

write_sheet(df, ss = '1M_WzK_o4eRZm1aLTX3Ey7P-jXQsxjBwO4MXD4gdaGWw', sheet = 'Sheet1')

