
#install.packages('tidyverse')
#install.packages('stringr')
#install.packages('googledrive')
#install.packages('googlesheets4')
#install.packages('lubridate')
library(tidyverse)
library(stringr)
library(googledrive)
library(googlesheets4)
library(lubridate)

conf_raw <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv')
death_raw <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv')

# Create daily state aggregated datasets

confdaily <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv') %>%
  select(7,starts_with('X')) %>%
  gather(zdate, zcases, -1) %>%
  mutate(date = strptime(sub('X','',zdate),format = '%m.%d.%y'),
          state = Province_State, 
         yearmonth = paste0(year(date), ' ',month(date, label = T, abbr = F))) %>%
  select(state,yearmonth,date,zcases) %>%
  group_by(state,yearmonth,date) %>%
  summarise(cases_d = sum(zcases)) %>%
  ungroup() %>%
  arrange(state,date) %>%
  group_by(state) %>%
  mutate(newcases_d = cases_d - lag(cases_d,default = 0),
         newcases_d = ifelse(newcases_d < 0, NA_integer_,newcases_d)) %>%
  select(state, yearmonth, date, cases_d, newcases_d)

deathdaily <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv') %>%
  select(7,starts_with('X')) %>%
  gather(zdate, zdeaths, -1) %>%
  mutate(date = strptime(sub('X','',zdate),format = '%m.%d.%y'),
         state = Province_State, 
         yearmonth = paste0(year(date), ' ',month(date, label = T, abbr = F))) %>%
  select(state,yearmonth,date,zdeaths) %>%
  group_by(state,yearmonth,date) %>%
  summarise(deaths_d = sum(zdeaths)) %>%
  ungroup() %>%
  arrange(state,date) %>%
  group_by(state) %>%
  mutate(newdeaths_d = deaths_d - lag(deaths_d,default = 0)) %>%
  select(state, yearmonth, date, deaths_d, newdeaths_d)

# Create monthly state aggregated datasets

confmonthly <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv') %>%
  select(6,7,starts_with('X')) %>%
  gather(zdate, zcases, -c(1,2)) %>%
  mutate(date = strptime(sub('X','',zdate),format = '%m.%d.%y'),
         yearmonth = paste0(year(date), ' ',month(date, label = T, abbr = F))) %>%
  rename(state = Province_State,
         county = Admin2) %>%
  select(state,county,yearmonth,zcases) %>%
  group_by(state,county,yearmonth) %>%
  summarise(ccases = max(zcases)) %>%
  ungroup() %>%
  inner_join(schedule, by = 'yearmonth') %>%
  group_by(state,year,monthname,yearmonth) %>%
  summarise(cases = sum(ccases)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(newcases = cases - lag(cases,default = 0)) %>%
  select(state, yearmonth, cases, newcases)

deathmonthly <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv') %>%
  select(6,7,starts_with('X')) %>%
  gather(zdate, zdeaths, -c(1,2)) %>%
  mutate(date = strptime(sub('X','',zdate),format = '%m.%d.%y'),
         yearmonth = paste0(year(date),' ',month(date, label = T, abbr = F))) %>%
  rename(state = Province_State,
         county = Admin2) %>%
  select(state,county,yearmonth,zdeaths) %>%
  group_by(state,county,yearmonth) %>%
  summarise(cdeaths = max(zdeaths)) %>%
  ungroup() %>%
  inner_join(schedule, by = 'yearmonth') %>%
  group_by(state,year,monthname,yearmonth) %>%
  summarise(deaths = sum(cdeaths)) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(newdeaths = deaths - lag(deaths,default = 0))%>%
  select(state, yearmonth, deaths, newdeaths)

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
