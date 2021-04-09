#Set up and Pull BLS data

#install.packages('blsAPI')
library(blsAPI)

states <- read.csv('https://gist.githubusercontent.com/dantonnoriega/bf1acd2290e15b91e6710b6fd3be0a53/raw/11d15233327c8080c9646c7e1f23052659db251d/us-state-ansi-fips.csv',
                   colClasses = 'character',
                   strip.white = T)

measures <- data.frame(code = c('03','04','05','06','07','08','09'),
                       measure = c('unemployment rate','unemployment count','employment count',
                         'labor force','employment-population ratio',
                         'labor force participation rate','civilian noninstitutional population'))

Sys.setenv(BLS_KEY = '9ecd688f35134033b93b83dfd06aaaee')

#Set product we're interested, in this case the LAU tables

product <- 'LAU'

#Set area we're interested in, in this case every state


areas <- data.frame(paste0('ST',states$st,'00000000000'),states$stusps)
                    
colnames(areas) <- c('code','state')

#Set measurement we're interested in, in this case unemployment rate

measurement <- measures %>%
  filter(measure == 'unemployment rate') %>%
  pull(code)

#Put it all together as seriesIDs


series1 <- paste0(product,areas$code[areas$state <= 'MO'],measurement)
series2 <- paste0(product,areas$code[areas$state > 'MO' & areas$state < 'WY'],measurement)
series3 <- paste0(product,areas$code[areas$state == 'WY'],measurement)

#Create API payload

payload1 <- list('seriesid' = series1,
        'startyear' = 2020,
        'endyear' = 2021)

payload2 <- list('seriesid' = series2,
                 'startyear' = 2020,
                 'endyear' = 2021)

payload3 <- list('seriesid' = series3,
                 'startyear' = 2020,
                 'endyear' = 2021)

#pull API response from payload

df1 <- blsAPI(payload1,return_data_frame = TRUE)

df2 <- blsAPI(payload2,return_data_frame = TRUE)

df3 <- blsAPI(payload3,return_data_frame = TRUE)

#add normal statecode back

unemprate <- df1 %>%
  rbind(df2) %>%
  rbind(df3) %>%
  mutate(st = substr(seriesID,6,7),
         code = substr(seriesID,19,20),
         value = as.numeric(value),
         month = substr(periodName,0,3)) %>%
  left_join(measures, by = 'code') %>%
  left_join(states, by = 'st') %>%
  mutate(state = stusps) %>%
  select(year,month,value,measure,state)

## Pull unemployment total

measurement <- measures %>%
  filter(measure == 'unemployment count') %>%
  pull(code)

#Put it all together as seriesIDs


series1 <- paste0(product,areas$code[areas$state <= 'MO'],measurement)
series2 <- paste0(product,areas$code[areas$state > 'MO' & areas$state < 'WY'],measurement)
series3 <- paste0(product,areas$code[areas$state == 'WY'],measurement)

#Create API payload

payload1 <- list('seriesid' = series1,
                 'startyear' = 2020,
                 'endyear' = 2021)

payload2 <- list('seriesid' = series2,
                 'startyear' = 2020,
                 'endyear' = 2021)

payload3 <- list('seriesid' = series3,
                 'startyear' = 2020,
                 'endyear' = 2021)

#pull API response from payload

df1 <- blsAPI(payload1,return_data_frame = TRUE)

df2 <- blsAPI(payload2,return_data_frame = TRUE)

df3 <- blsAPI(payload3,return_data_frame = TRUE)


unemplevel <- df1 %>%
  rbind(df2) %>%
  rbind(df3) %>%
  mutate(st = substr(seriesID,6,7),
         code = substr(seriesID,19,20),
         value = as.numeric(value),
         month = substr(periodName,0,3)) %>%
  left_join(measures, by = 'code') %>%
  left_join(states, by = 'st') %>%
  mutate(state = stusps) %>%
  select(year,month,value,measure,state)