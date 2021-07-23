## Estimate Model 2: Logistic regression to model whether a person is in the civilian labor force on number of confirmed COVID cases and gender.

## Run First:
## 1. Read in IPUMS CPS.R
## 2. Pull covid data.R

#necessary library calls 

library(tidyverse)
library(survey)
library(broom)
library(googlesheets4)
library(labelled)
library(dplyr)

attach()
#build analyic dataframe

analytic2 <- schedule %>% #start with schedule 
  inner_join(confmonthly, by = 'yearmonth') %>% #join in monthly new confirms for relevant months
  inner_join(CPS, by = c('yearmonth', 'state')) %>% #join in CPS data
  filter(LABFORCE %in% c(1,2)) %>% #filter for individuals with relevant LABFORCE values
  mutate(CLF = ifelse(LABFORCE == 1, 0, 1)) %>% #create dependent variable
  select(CLF, SEX, EEDUC, agebin, raceth, WTFINL, newcases, yearmonth, state) %>% #retain relevant variables
  remove_attributes(.,c('label','var_desc'))
  
#create empty lists to hold state-wise GLMs, predictions, and final dataframes for comparing predictions to actuals

glm_list <- list()
predictions_list <- list()
final_list <- list()

#create suvey design for unweighted model

d_unweight <- svydesign(ids = ~1,  data = analytic2)

#run state-wise loop to produce final dataframes

for (i in unique(analytic2$state)) {
  glm_list[[i]] <- svyglm(CLF ~ newcases + SEX + agebin + raceth + EEDUC, subset = state == i, design = d_unweight, family = binomial)
  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
  attributes(predictions_list[[i]]) <- NULL
  final_list[[i]] <- cbind(analytic2 %>% filter(state == i),data.frame(predict = predictions_list[[i]]))
}

#combine final dataframes to national comparison set

unweight_v_df <- do.call('rbind',final_list)

#check prediction vs reality overall

unweight_v_df %>%
  summarise(sum(CLF * WTFINL)/sum(predict * WTFINL)) %>%
  range_write(., ss = '1eDMiNvb8-3oa_nYIBRMtsbVFIrMGhsOv-tghR5y-SKg', sheet = 'unweight checks', range = 'A:A')

#check predictino vs reality by state and sex

unweight_v_df %>%
  group_by(stusps,SEX) %>%
  summarise(sum(CLF * WTFINL)/sum(predict * WTFINL)) %>%
  range_write(., ss = '1eDMiNvb8-3oa_nYIBRMtsbVFIrMGhsOv-tghR5y-SKg', sheet = 'unweight checks', range = 'B:D')

#create empty list for coefficients

results_list <- ls()

#create statewise dataframes of coefficient estimates and significance via loop

for (i in unique(analytic2$state)) {
  a <- tidy(svyglm(CLF ~ newcases + SEX + agebin + raceth + EEDUC, subset = state == i, design = d_unweight, family = binomial))
  results_list[[i]] <- cbind(i,a)
}

#combine statewise dataframes into one dataframe

results <- do.call('rbind',results_list)

#write coefficients to a google sheets tab

write_sheet(results, ss = '1eDMiNvb8-3oa_nYIBRMtsbVFIrMGhsOv-tghR5y-SKg', sheet = 'unweighted')


#Weighted model now:

#create empty lists to hold state-wise GLMs, predictions, and final dataframes for comparing predictions to actuals

glm_list <- list()
predictions_list <- list()
final_list <- list()

#create suvey design for unweighted model

d_weight <- svydesign(ids = ~1, weights = ~WTFINL, data = analytic2)

#run state-wise loop to produce final dataframes

for (i in unique(analytic2$state)) {
  glm_list[[i]] <- svyglm(CLF ~ newcases + SEX + agebin + raceth + EEDUC, subset = state == i, design = d_weight, family = binomial)
  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
  attributes(predictions_list[[i]]) <- NULL
  final_list[[i]] <- cbind(analytic2 %>% filter(state == i),data.frame(predict = predictions_list[[i]]))
}

#combine final dataframes to national comparison set

weight_v_df <- do.call('rbind',final_list)

#check prediction vs reality overall

weight_v_df %>%
  summarise(sum(CLF * WTFINL)/sum(predict * WTFINL)) %>%
  range_write(., ss = '1eDMiNvb8-3oa_nYIBRMtsbVFIrMGhsOv-tghR5y-SKg', sheet = 'weight checks', range = 'A:A')

#check predictino vs reality by state and sex

weight_v_df %>%
  group_by(state,SEX) %>%
  summarise(sum(CLF * WTFINL)/sum(predict * WTFINL)) %>%
  range_write(., ss = '1eDMiNvb8-3oa_nYIBRMtsbVFIrMGhsOv-tghR5y-SKg', sheet = 'weight checks', range = 'B:D')

#create empty list for coefficients

results_list <- ls()

#create statewise dataframes of coefficient estimates and significance via loop

for (i in unique(analytic2$state)) {
  a <- tidy(svyglm(CLF ~ newcases + SEX + agebin + raceth + EEDUC, subset = state == i, design = d_weight, family = binomial))
  results_list[[i]] <- cbind(i,a)
}

#combine statewise dataframes into one dataframe

results <- do.call('rbind',results_list)

#write coefficients to a google sheets tab

write_sheet(results, ss = '1eDMiNvb8-3oa_nYIBRMtsbVFIrMGhsOv-tghR5y-SKg', sheet = 'weighted')

#make state-month predictions

model2monthly <- weight_v_df %>%
  group_by(state,yearmonth) %>%
  summarise(labforce_predict = sum(predict*WTFINL))

