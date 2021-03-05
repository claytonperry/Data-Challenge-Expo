conf_raw <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv')
death_raw <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv')

conf <- conf_raw %>%
  select(7,starts_with('X')) %>%
  gather(zdate, zcases, -1) %>%
  mutate(date = strptime(sub('X','',zdate),format = '%m.%d.%y'),
          state = Province_State) %>%
  select(state,date,zcases) %>%
  group_by(state,date) %>%
  summarise(cases = sum(zcases))

death <- death_raw %>%
  select(7,starts_with('X')) %>%
  gather(zdate, zdeaths, -1) %>%
  mutate(date = strptime(sub('X','',zdate),format = '%m.%d.%y'),
         state = Province_State) %>%
  select(state,date,zdeaths) %>%
  group_by(state,date) %>%
  summarise(deaths = sum(zdeaths))

#final dataframes have columns for state, date, and confirmed cases or deaths
