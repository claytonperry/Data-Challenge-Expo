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
library(memisc)
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
  select(Imsi, sex, stusps, yearmonth, newcases, PWEIGHT, weight) %>%
  mutate(id = row_number())

glm_list <- list()
predictions_list <- list()
final_list <- list()

d <- svydesign(ids = ~1, weights = ~PWEIGHT, data = analytic)

for (i in unique(analytic$stusps)) {
  glm_list[[i]] <- svyglm(Imsi ~ newcases + sex, subset = stusps == i, design = d, family = binomial)
  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
  attributes(predictions_list[[i]]) <- NULL
  predictions_list[[i]] <- data.frame(state = i,predict = predictions_list[[i]])
  final_list[[i]] <- cbind(analytic %>% filter(stusps == i),predictions_list[[i]])
  }

expected_v_df <- do.call('rbind',final_list)

expected_v_df %>%
  summarise(sum(Imsi * PWEIGHT)/sum(predict * PWEIGHT))


d <- svydesign(ids = ~1, weights = ~PWEIGHT, data = analytic)

glm <- svyglm(Imsi ~ newcases + sex, design = d, subset = stusps == 'AL', family = binomial)

summary(glm)

summary(predict(glm, type = 'response'))

results_list <- list()

for (i in unique(analytic$stusps)) {
  a <- tidy(svyglm(Imsi ~ newcases + sex, subset = stusps == i, design = d, family = binomial))
  results_list[[i]] <- cbind(i,a)
}

results <- do.call('rbind',results_list)

write_sheet(results, ss = '1wZFsYoKQyGQJPBU0dqJq0gI8CHadbWUfaUFVzHV6It0', sheet = 'Model 1 (SS)')
