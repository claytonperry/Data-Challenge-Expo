#Compare BLS and HHS Pulse unemployment numbers

#Step 0: Prerequisites
# Pull_BLS_Data.R
# Pull HH Pulse Data into R.R


df <- puf_df %>%
  group_by(EST_ST,WEEK) %>%
  summarise(hhp_total_unemp_n = sum(PWEIGHT[ANYWORK == 2]),
            hhp_covid_unemp_n = sum(PWEIGHT[RSNNOWRK %in% c(8,9,10,11)])) %>%
  rename(week = WEEK) %>%
  mutate(fips = str_pad(EST_ST,2,side = 'left',pad = '0')) %>%
  full_join(states, by = 'fips') %>%
  inner_join(schedule, by = 'week') %>%
  inner_join(labforce, by = c('yearmonth','state')) %>%
  rename(bls_labforce = value) %>%
  mutate(hhp_total_unemp_rt = hhp_total_unemp_n/bls_labforce,
         hhp_covid_unemp_rt = hhp_covid_unemp_n/bls_labforce,
         hhp_covid_unemp_prop = hhp_covid_unemp_n/hhp_total_unemp_n) %>%
  inner_join(unemprate, by = c('yearmonth','state')) %>%
  rename(bls_unemp_rt = value) %>%
  inner_join(unemplevel, by = c('yearmonth','state')) %>%
  rename(bls_unemp_n = value) %>%
  mutate(bls_unemp_rt = bls_unemp_rt *0.01,
         bls_unemp_rt_calc = bls_unemp_n/bls_labforce, 
         bls_hhp_unemp_prop = bls_unemp_n/hhp_total_unemp_n,
         adj_covid_unemp_n = bls_hhp_unemp_prop*hhp_covid_unemp_n,
         adj_covid_unemp_rt = adj_covid_unemp_n/bls_labforce,
         adj_covid_unemp_prop = adj_covid_unemp_n/bls_unemp_n) %>%
  ungroup() %>%
  select(state, yearmonth, bls_labforce, bls_unemp_n, hhp_total_unemp_n, bls_hhp_unemp_prop, bls_unemp_rt, bls_unemp_rt_calc, hhp_total_unemp_rt,
         hhp_covid_unemp_n, hhp_covid_unemp_rt, hhp_covid_unemp_prop, adj_covid_unemp_n, adj_covid_unemp_rt, adj_covid_unemp_prop)

write_sheet(df, ss = '1-bP6OPVrStqe0O_tlzFG3QYLXRfOdqg-4lYvXEraXfI', sheet = 'Compare Unemp')  

summary(df$hhp_total_unemp_rt-df$bls_unemp_rt)


#########

# Finding completeness in HHPulse data
`%notin%` <- Negate(`%in%`)

empvarswork <- puf_df %>%
  rename(week = WEEK) %>%
  mutate(fips = str_pad(EST_ST,2,side = 'left',pad = '0')) %>%
  inner_join(states, by = 'fips') %>%
  left_join(schedule, by = 'week') %>%
  group_by(stusps,week) %>%
  summarise(n = n(),
            anywork_notNA_n = sum(ANYWORK %notin% c(-88,-99)),
            anywork_notNA_prop = sum(ANYWORK %notin% c(-88,-99))/n(),
            rsnnowrk_notNA_n = sum(RSNNOWRK %notin% c(-88,-99)),
            rsnnowrk_notNA_prop = sum(RSNNOWRK  %notin% c(-88,-99))/sum(ANYWORK == 2))

write_sheet(empvarswork, ss = '1-bP6OPVrStqe0O_tlzFG3QYLXRfOdqg-4lYvXEraXfI', sheet = 'Emp Vars Completeness')  
