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
               select(Imsi,sex,week,fips, PWEIGHT, weight, raceth, agebin, EEDUC),
             by = 'week') %>%
  full_join(states, by = 'fips') %>%
  inner_join(confmonthly, by = c('yearmonth', 'state')) %>%
  select(Imsi, sex, state, yearmonth, newcases, PWEIGHT, weight, EEDUC, raceth, agebin) %>%
  mutate(id = row_number())

glm_list <- list()
predictions_list <- list()
final_list <- list()

d <- svydesign(ids = ~1, weights = ~PWEIGHT, data = analytic)

for (i in unique(analytic$state)) {
  glm_list[[i]] <- svyglm(Imsi ~ newcases + sex + agebin + raceth + EEDUC, subset = state == i, design = d, family = binomial)
  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
  attributes(predictions_list[[i]]) <- NULL
  final_list[[i]] <- cbind(analytic %>% filter(state == i),data.frame(predict = predictions_list[[i]]))
  }

expected_v_df <- do.call('rbind',final_list)

expected_v_df %>%
  summarise(sum(Imsi * PWEIGHT)/sum(predict * PWEIGHT))

results_list <- list()

for (i in unique(analytic$state)) {
  a <- tidy(svyglm(Imsi ~ newcases + sex + agebin + raceth + EEDUC, subset = state == i, design = d, family = binomial))
  results_list[[i]] <- cbind(i,a)
}

results <- do.call('rbind',results_list)

write_sheet(results, ss = '1wZFsYoKQyGQJPBU0dqJq0gI8CHadbWUfaUFVzHV6It0', sheet = 'Model 1 (SS) with demos')

model1monthly %>%
  group_by(state,yearmonth) %>%
  summarise(unemp_predict = sum(predict*PWEIGHT))


#Model Estimates
#overall predicted covid unemp
model1total <- expected_v_df %>%
  summarise(SS1_total = sum(predict*PWEIGHT)) %>%
  write_sheet(model1total, ss = '1wZFsYoKQyGQJPBU0dqJq0gI8CHadbWUfaUFVzHV6It0', sheet = 'SS1.0 Part 1 Est w demos')

HHPtotal <- analytic %>%
  summarise(HHP_total = sum(Imsi*PWEIGHT)) %>%
  range_write(HHPtotal, ss = '1wZFsYoKQyGQJPBU0dqJq0gI8CHadbWUfaUFVzHV6It0', sheet = 'SS1.0 Part 1 Est w demos', range='B')

#state predictions
model1state <- expected_v_df %>%
  group_by(state) %>%
  summarise(SS1_state = sum(predict*PWEIGHT)) %>%
  range_write(model1state, ss = '1wZFsYoKQyGQJPBU0dqJq0gI8CHadbWUfaUFVzHV6It0', sheet = 'SS1.0 Part 1 Est w demos', range = 'D1')

HHP_state <- analytic %>%
  group_by(state) %>%
  summarise(HHP_state = sum(Imsi*PWEIGHT)) %>%
  range_write(HHP_state, ss = '1wZFsYoKQyGQJPBU0dqJq0gI8CHadbWUfaUFVzHV6It0', sheet = 'SS1.0 Part 1 Est w demos', range = 'F1')

#state-month predictions
model1monthly <- expected_v_df %>%
  group_by(state,yearmonth) %>%
  summarise(SS1_statemonth = sum(predict*PWEIGHT)) %>%
  range_write(model1monthly, ss = '1wZFsYoKQyGQJPBU0dqJq0gI8CHadbWUfaUFVzHV6It0', sheet = 'SS1.0 Part 1 Est w demos', range = 'I1')

HHP_statemonth <- analytic %>%
  group_by(state,yearmonth) %>%
  summarise(HHP_statemonth = sum(Imsi*PWEIGHT)) %>%
  range_write(HHP_statemonth, ss = '1wZFsYoKQyGQJPBU0dqJq0gI8CHadbWUfaUFVzHV6It0', sheet = 'SS1.0 Part 1 Est w demos', range = 'L1')
