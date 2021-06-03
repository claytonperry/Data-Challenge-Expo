## Estimate Model 2: Logistic regression to model whether a person is in the civilian labor force on number of confirmed COVID cases and gender.

## Run First:
## 1. Read in IPUMS CPS.R
## 2. Pull covid data.R

# library(tidyverse)
# library(survey)
# library(broom)
# library(googlesheets4)

analytic2 <- schedule %>%
  inner_join(confmonthly, by = 'yearmonth') %>%
  inner_join(CPS, by = c('yearmonth', 'state')) %>%
  filter(LABFORCE %in% c(1,2)) %>%
  mutate(CLF = ifelse(LABFORCE == 1, 0, 1)) %>%
  select(CLF, SEX, WTFINL, newcases, yearmonth, stusps)


glm_list <- list()
predictions_list <- list()
final_list <- list()

d_unweight <- svydesign(ids = ~1,  data = analytic2)

for (i in unique(analytic2$stusps)) {
  glm_list[[i]] <- svyglm(CLF ~ newcases + SEX, subset = stusps == i, design = d_unweight, family = binomial)
  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
  attributes(predictions_list[[i]]) <- NULL
  predictions_list[[i]] <- data.frame(state = i,predict = predictions_list[[i]])
  final_list[[i]] <- cbind(analytic2 %>% filter(stusps == i),predictions_list[[i]])
}

unweight_v_df <- do.call('rbind',final_list)

unweight_v_df %>%
  summarise(sum(CLF * WTFINL)/sum(predict * WTFINL))

for (i in unique(analytic2$stusps)) {
  a <- tidy(svyglm(CLF ~ newcases + SEX, subset = stusps == i, design = d_unweight, family = binomial))
  results_list[[i]] <- cbind(i,a)
}

results <- do.call('rbind',results_list)

write_sheet(results, ss = '1eDMiNvb8-3oa_nYIBRMtsbVFIrMGhsOv-tghR5y-SKg', sheet = 'unweighted')
1
#Weighted

glm_list <- list()
predictions_list <- list()
final_list <- list()

d_weight <- svydesign(ids = ~1, weights = ~WTFINL, data = analytic2)

for (i in unique(analytic2$stusps)) {
  glm_list[[i]] <- svyglm(CLF ~ newcases + SEX, subset = stusps == i, design = d_weight, family = binomial)
  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
  attributes(predictions_list[[i]]) <- NULL
  predictions_list[[i]] <- data.frame(state = i,predict = predictions_list[[i]])
  final_list[[i]] <- cbind(analytic2 %>% filter(stusps == i),predictions_list[[i]])
}

weight_v_df <- do.call('rbind',final_list)

weight_v_df %>%
  summarise(sum(CLF * WTFINL)/sum(predict * WTFINL))

weight_v_df %>%
  group_by(SEX) %>%
  summarise(sum(CLF * WTFINL)/sum(predict * WTFINL))

for (i in unique(analytic2$stusps)) {
  a <- tidy(svyglm(CLF ~ newcases + SEX, subset = stusps == i, design = d_weight, family = binomial))
  results_list[[i]] <- cbind(i,a)
}

results <- do.call('rbind',results_list)

write_sheet(results, ss = '1eDMiNvb8-3oa_nYIBRMtsbVFIrMGhsOv-tghR5y-SKg', sheet = 'weighted')
1
#CPS %>%
#  filter(LABFORCE %in% c(1,2),
#         paste0(YEAR,month.abb[MONTH]) %in% paste0(schedule$year,schedule$monthname),
#         state %in% confmonthly$state) %>%
#  count()

#CPS %>%
#  inner_join(confmonthly, by = c('yearmonth', 'state')) %>%
#  inner_join(schedule, by = 'yearmonth') %>%
#  count(LABFORCE == 0)
