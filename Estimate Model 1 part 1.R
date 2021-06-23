#Analytic File for Modeling
#This code combines data into one or 2 analytic files used for modeling

#install.packages('tidyverse') 
library(tidyverse)

#######
#PART 1
#HH Pulse data: filter to weeks specified in spec, create Imsi indicator for DV
#requires running 'Pull HH Pulse Data into R'
pulse <- puf_df %>%
  filter(WEEK %in% c(2,6,10,13,14,17,19,21,22,24,26)) %>%
  mutate(st = str_pad(EST_ST,2,side = 'left',pad = '0'),
         Imsi = ifelse(RSNNOWRK==8|RSNNOWRK==9|RSNNOWRK==10|RSNNOWRK==11, 1, 0),
         month = ifelse(WEEK==2,'May',
                      ifelse(WEEK==6,'June',
                      ifelse(WEEK==10,'July',
                      ifelse(WEEK==13,'August',
                      ifelse(WEEK==14,'September',
                      ifelse(WEEK==17,'October',
                      ifelse(WEEK==19,'November',
                      ifelse(WEEK==21,'December',
                      ifelse(WEEK==22,'January',
                      ifelse(WEEK==24,'February',
                      ifelse(WEEK==26,'March',NA)))))))))))) %>%
  select(WEEK,RSNNOWRK,EST_ST,EGENDER,PWEIGHT,Imsi,st,month) %>%
  full_join(states, by = 'st') 
  

#?? stuck here, getting the month of the date isn't working
#Covid data: summarize covid cases by month and state
#requires running 'Pull covid data'
conf_month <- conf %>%
  mutate(month = month(date)) %>%
  group_by(state,month) %>%
  summarize(Cms = sum(cases)) %>%
  select(Cms,state,month)
  
#merge pulse and conf_month data to get analytic file for part 1
  
  
puf_df %>%
  rename(week = WEEK) %>%
  inner_join(schedule, by = 'week') %>%
  filter(ANYWORK %in% c(1,2)) %>%
  mutate(Imsi = ifelse(RSNNOWRK %in% c(8,9,10,11), 1, 0)) %>%
  summarise(sum(Imsi * PWEIGHT) / sum(PWEIGHT))

analytic %>%
  summarise(sum(Imsi * PWEIGHT) / sum(PWEIGHT))

analytic %>%
  summarise(sum(Imsi)/n())


########### Clay taking over
#install.packages('modelr')
#install.packages('tidymodels')
#install.packages('memisc')


library(tidymodels)
library(modelr)
library(broom)
library(tidyverse)
library(survey)

analytic <- schedule %>%
  inner_join(puf_df %>%
               filter(ANYWORK %in% c(1,2)) %>%
               rename(week = WEEK) %>%
               mutate(fips = str_pad(EST_ST,2,side = 'left',pad = '0'),
                      Imsi = as.integer(ifelse(RSNNOWRK %in% c(8,9,10,11), 1, 0)),
                      sex = EGENDER - 1,
                      weight = PWEIGHT/sum(PWEIGHT)) %>%
               select(Imsi,sex,week,fips, PWEIGHT, weight),
             by = 'week') %>%
  full_join(states, by = 'fips') %>%
  inner_join(confmonthly, by = c('yearmonth', 'state')) %>%
  select(Imsi, sex, state, yearmonth, newcases, PWEIGHT, weight) %>%
  mutate(id = row_number())

glm_list <- list()
predictions_list <- list()
final_list <- list()

d <- svydesign(ids = ~1, weights = ~PWEIGHT, data = analytic)

for (i in unique(analytic$state)) {
  glm_list[[i]] <- svyglm(Imsi ~ newcases + sex, subset = state == i, design = d, family = binomial)
  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
  attributes(predictions_list[[i]]) <- NULL
  final_list[[i]] <- cbind(analytic %>% filter(state == i),data.frame(predict = predictions_list[[i]]))
  }

expected_v_df <- do.call('rbind',final_list)

expected_v_df %>%
  summarise(sum(Imsi * PWEIGHT)/sum(predict * PWEIGHT))

results_list <- list()

for (i in unique(analytic$state)) {
  a <- tidy(svyglm(Imsi ~ newcases + sex, subset = state == i, design = d, family = binomial))
  results_list[[i]] <- cbind(i,a)
}

results <- do.call('rbind',results_list)

write_sheet(results, ss = '1wZFsYoKQyGQJPBU0dqJq0gI8CHadbWUfaUFVzHV6It0', sheet = 'Model 1 (SS)')

model1monthly %>%
  group_by(state,yearmonth) %>%
  summarise(unemp_predict = sum(predict*PWEIGHT))


#Model Estimates
#overall predicted covid unemp
model1total <- expected_v_df %>%
  summarise(SS1_total = sum(predict*PWEIGHT))

write_sheet(model1total, ss = '1wZFsYoKQyGQJPBU0dqJq0gI8CHadbWUfaUFVzHV6It0', sheet = 'SS1.0 Part 1 Estimates')

HHPtotal <- analytic %>%
  summarise(HHP_total = sum(Imsi*PWEIGHT))

range_write(HHPtotal, ss = '1wZFsYoKQyGQJPBU0dqJq0gI8CHadbWUfaUFVzHV6It0', sheet = 'SS1.0 Part 1 Estimates', range='B')

#state predictions
model1state <- expected_v_df %>%
  group_by(state) %>%
  summarise(SS1_state = sum(predict*PWEIGHT))

range_write(model1state, ss = '1wZFsYoKQyGQJPBU0dqJq0gI8CHadbWUfaUFVzHV6It0', sheet = 'SS1.0 Part 1 Estimates', range = 'D1')

HHP_state <- analytic %>%
  group_by(state) %>%
  summarise(HHP_state = sum(Imsi*PWEIGHT))

range_write(HHP_state, ss = '1wZFsYoKQyGQJPBU0dqJq0gI8CHadbWUfaUFVzHV6It0', sheet = 'SS1.0 Part 1 Estimates', range = 'F1')

#state-month predictions
model1monthly <- expected_v_df %>%
  group_by(state,yearmonth) %>%
  summarise(SS1_statemonth = sum(predict*PWEIGHT))

range_write(model1monthly, ss = '1wZFsYoKQyGQJPBU0dqJq0gI8CHadbWUfaUFVzHV6It0', sheet = 'SS1.0 Part 1 Estimates', range = 'I1')

HHP_statemonth <- analytic %>%
  group_by(state,yearmonth) %>%
  summarise(HHP_statemonth = sum(Imsi*PWEIGHT))

range_write(HHP_statemonth, ss = '1wZFsYoKQyGQJPBU0dqJq0gI8CHadbWUfaUFVzHV6It0', sheet = 'SS1.0 Part 1 Estimates', range = 'L1')
