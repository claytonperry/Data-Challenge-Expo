# Investigating the HH Pulse data

#install.packages('googlesheets4')

library(googlesheets4)

gs4_auth()

df <- puf_df %>%
  rename(week = WEEK) %>%
  mutate(fips = str_pad(EST_ST,2,side = 'left',pad = '0'),
         sex = ifelse(EGENDER == 1, 'male','female'))  %>%
  full_join(states, by = 'fips') %>%
  inner_join(schedule, by = 'week') %>%
  select(state,week,year,EGENDER,yearmonth,monthname,sex,ANYWORK,RSNNOWRK,PWEIGHT) %>%
  mutate(labforceif = ifelse(ANYWORK %in% c(1,2), 1, 0),
         anyworkif = ifelse(ANYWORK == 2, 1, 0),
         rsnnowrkif = ifelse(RSNNOWRK %in% c(8,9,10,11), 1, 0)) %>%
  group_by(state,year,monthname,yearmonth,sex) %>%
  summarise(n = n(),
            labforce = sum(ANYWORK %in% c(1,2)),
            anywork_2 = sum(ANYWORK == 2),
            rsnnowrk_covid = sum(RSNNOWRK %in% c(8,9,10,11)),
            weighted_n = sum(PWEIGHT),
            weighted_lab = sum(labforceif*PWEIGHT),
            weighted_anywork2 = sum(anyworkif * PWEIGHT),
            weighted_rsnnowrkcov = sum(rsnnowrkif * PWEIGHT)) %>%
  mutate(labprop = weighted_lab/weighted_n,
         unemp_outof_lab = weighted_anywork2/weighted_lab,
         covid_outof_lab = weighted_rsnnowrkcov/weighted_lab,
         covid_outof_unemp = weighted_rsnnowrkcov/weighted_anywork2)

write_sheet(df, ss = '1-bP6OPVrStqe0O_tlzFG3QYLXRfOdqg-4lYvXEraXfI', sheet = 'Compare HH Pulse Values')  
