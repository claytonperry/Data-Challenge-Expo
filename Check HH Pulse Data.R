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
  select(state,week,year,EGENDER,yearmonth,monthname,sex,ANYWORK,RSNNOWRK) %>%
  group_by(state,year,monthname,yearmonth,sex) %>%
  summarise(n = n(),
            anywork_2 = sum(ANYWORK == 2),
            rsnnowrk_covid = sum(RSNNOWRK %in% c(9,10,11,12)))

write_sheet(df, ss = '1-bP6OPVrStqe0O_tlzFG3QYLXRfOdqg-4lYvXEraXfI', sheet = 'Compare HH Pulse Values')  
