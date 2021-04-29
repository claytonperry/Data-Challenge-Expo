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

#4/20: Chrys edit (this doesn't work yet...)
#create estimates for monthly covid cases and deaths
library(lubridate)
conf_est <- aggregate(cases~cbind(state,month(date)),data=conf,FUN=sum)
conf_est0 <- transform(conf,month=as.numeric(format(as.Date(Date),"%m")))
conf_est0 <- transform(conf,month=as.numeric(format(as.Date(Date),"%m")))


conf_month_ct <- conf_raw %>%
  select(7,starts_with('X')) %>%
  gather(zdate, zcases, -1) %>%
  mutate(date = strptime(sub('X','',zdate),format = '%m.%d.%y'),
         state = Province_State) %>%
  select(state,date,zcases) %>%
  group_by(state,month(date)) %>%
  summarise(cases = sum(zcases))
#  summarise(month_est = sum(cases))
