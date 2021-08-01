
library(tidyverse)
library(survey)
library(broom)
library(googlesheets4)
library(labelled)
library(dplyr)



analytic2_2 <- schedule %>% #start with schedule 
  inner_join(m2_0_monthly, by = 'yearmonth') %>% #join in monthly new confirms for relevant months
  inner_join(CPS, by = c('yearmonth', 'state')) %>% #join in CPS data
  inner_join(restrictions_mnth, by = c('yearmonth', 'state')) %>% #join in restrictions aggregates
  filter(LABFORCE %in% c(1,2)) %>% #filter for individuals with relevant LABFORCE values
  mutate(CLF = ifelse(LABFORCE == 1, 0, 1)) %>% #create dependent variable
  select(CLF, SEX, EEDUC, agebin, raceth, WTFINL, newcases_hat, yearmonth, state,
         C1_c0, C1_c1, C1_c2, C1_c3, C2_c0, C2_c1, C2_c2, C2_c3, C3_c0, C3_c1, C3_c2,
         C4_c0, C4_c1, C4_c2, C4_c3, C4_c4, C5_c0, C5_c1, C5_c2, C6_c0, C6_c1, C6_c2,
         C6_c3, C7_c0, C7_c1, C7_c2, C8_c0, C8_c1, C8_c2, C8_c3, C8_c4, C1_sum1, C2_sum1,
         C3_sum1, C4_sum1, C5_sum1, C6_sum1, C7_sum1, C8_sum1, C1_sum2, C2_sum2, C3_sum2,
         C4_sum2, C5_sum2, C6_sum2, C7_sum2, C8_sum2) %>% #retain relevant variables
  remove_attributes(.,c('label','var_desc'))


glm_list <- list()
predictions_list <- list()
final_list <- list()


#run state-wise loop to produce final dataframes

#for (i in unique(analytic2_2$state)) {
#  glm_list[[i]] <- glm(CLF ~ newcases_hat + SEX + agebin + raceth + EEDUC +
#                         C1_c0 + C1_c1 + C1_c2 + C1_c3 +
#                         C2_c0 + C2_c1 + C2_c2 + C2_c3 +
#                         C3_c0 + C3_c1 + C3_c2 + 
#                         C4_c0 + C4_c1 + C4_c2 + C4_c3 + C4_c4 +
#                         C5_c0 + C5_c1 + C5_c2 + 
#                         C6_c0 + C6_c1 + C6_c2 + C6_c3 +
#                         C7_c0 + C7_c1 + C7_c2 + 
#                         C8_c0 + C8_c1 + C8_c2 + C8_c3 + C8_c4, subset = state == i, data = analytic2_2, family = binomial)
#  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
#  attributes(predictions_list[[i]]) <- NULL
#  final_list[[i]] <- cbind(analytic2_2 %>% filter(state == i),data.frame(predict = predictions_list[[i]]))
#}

#m2_2_expected_v_df_v1 <- do.call('rbind',final_list)

#results_list <- list()

#for (i in unique(analytic2_2$state)) {
#  glm <- glm(CLF ~ newcases_hat + SEX + agebin + raceth + EEDUC +
#               C1_c0 + C1_c1 + C1_c2 + C1_c3 +
#               C2_c0 + C2_c1 + C2_c2 + C2_c3 +
#               C3_c0 + C3_c1 + C3_c2 + 
#               C4_c0 + C4_c1 + C4_c2 + C4_c3 + C4_c4 +
#               C5_c0 + C5_c1 + C5_c2 + 
#               C6_c0 + C6_c1 + C6_c2 + C6_c3 +
#               C7_c0 + C7_c1 + C7_c2 + 
#               C8_c0 + C8_c1 + C8_c2 + C8_c3 + C8_c4, subset = state == i, data = analytic2_2, family = binomial)
#  a <- tidy(glm)
#  aic <- AIC(glm)
#  results_list[[i]] <- cbind(i,a)
#  aic_list[[i]] <- cbind(i,aic)
#}


#m2_2_results_v1 <- do.call('rbind',results_list)
#m2_2_aic_v1 <- data.frame(do.call('rbind',aic_list))
#m2_2_expecteds_v1_compare <- m2_2_expected_v_df_v1 %>%
#  group_by(state,yearmonth) %>%
#  summarise(CLF = sum(CLF),
#            predictions = sum(predict))

#range_write(m2_2_results_v1, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.2 v1!A:F')
#range_write(m2_2_aic_v1, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.2 v1!H:I')
#range_write(m2_2_expecteds_v1_compare, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.2 v1!K:N')

#version using sum1

for (i in unique(analytic2_2$state)) {
  glm_list[[i]] <- glm(CLF ~ newcases_hat + SEX + agebin + raceth + EEDUC +
                         C1_sum1 + C2_sum1 + C3_sum1 + C4_sum1 +
                         C5_sum1 + C6_sum1 + C7_sum1 + C8_sum1, subset = state == i, data = analytic2_2, family = binomial)
  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
  attributes(predictions_list[[i]]) <- NULL
  final_list[[i]] <- cbind(analytic2_2 %>% filter(state == i),data.frame(predict = predictions_list[[i]]))
}

m2_2_expected_v_df_v2 <- do.call('rbind',final_list)

results_list <- list()

for (i in unique(analytic2_2$state)) {
  glm <- glm(CLF ~ newcases_hat + SEX + agebin + raceth + EEDUC +
               C1_sum1 + C2_sum1 + C3_sum1 + C4_sum1 +
               C5_sum1 + C6_sum1 + C7_sum1 + C8_sum1, subset = state == i, data = analytic2_2, family = binomial)
  a <- tidy(glm)
  aic <- AIC(glm)
  results_list[[i]] <- cbind(i,a)
  aic_list[[i]] <- cbind(i,aic)
}


m2_2_results_v2 <- do.call('rbind',results_list)
m2_2_aic_v2 <- data.frame(do.call('rbind',aic_list))
m2_2_expecteds_v2_compare <- m2_2_expected_v_df_v1 %>%
  group_by(state,yearmonth) %>%
  summarise(samp_size = n(),
            CLF = sum(CLF),
            predictions = sum(predict))

range_write(m2_2_results_v2, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.2 v2!A:F')
range_write(m2_2_aic_v2, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.2 v2!H:I')
range_write(m2_2_expecteds_v2_compare, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.2 v2!K:N')

#version using sum2

#for (i in unique(analytic2_2$state)) {
#  glm_list[[i]] <- glm(CLF ~ newcases_hat + SEX + agebin + raceth + EEDUC +
#                         C1_sum2 + C2_sum2 + C3_sum2 + C4_sum2 +
#                         C5_sum2 + C6_sum2 + C7_sum2 + C8_sum2, subset = state == i, data = analytic2_2, family = binomial)
#  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
#  attributes(predictions_list[[i]]) <- NULL
#  final_list[[i]] <- cbind(analytic2_2 %>% filter(state == i),data.frame(predict = predictions_list[[i]]))
#}

#m2_2_expected_v_df_v3 <- do.call('rbind',final_list)

#results_list <- list()

#for (i in unique(analytic2_2$state)) {
#  glm <- glm(CLF ~ newcases_hat + SEX + agebin + raceth + EEDUC +
#               C1_sum2 + C2_sum2 + C3_sum2 + C4_sum2 +
#               C5_sum2 + C6_sum2 + C7_sum2 + C8_sum2, subset = state == i, data = analytic2_2, family = binomial)
#  a <- tidy(glm)
#  aic <- AIC(glm)
#  results_list[[i]] <- cbind(i,a)
#  aic_list[[i]] <- cbind(i,aic)
#}


#m2_2_results_v3 <- do.call('rbind',results_list)
#m2_2_aic_v3 <- data.frame(do.call('rbind',aic_list))
#m2_2_expecteds_v3_compare <- m2_2_expected_v_df_v1 %>%
#  group_by(state,yearmonth) %>%
#  summarise(CLF = sum(CLF),
#            predictions = sum(predict))

#range_write(m2_2_results_v3, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.2 v3!A:F')
#range_write(m2_2_aic_v3, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.2 v3!H:I')
#range_write(m2_2_expecteds_v3_compare, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.2 v3!K:N')


#CHRYS - subset analysis for Richard 
#version using sum1

analytic2_2_ca <- analytic2_2 %>%
  filter(state == 'California')
#glm
for (i in unique(analytic2_2_ca$state)) {
  glm_list[[i]] <- glm(CLF ~ newcases_hat + SEX + agebin + raceth + EEDUC +
                         C1_sum1 + C2_sum1 + C3_sum1 + C4_sum1, subset = state == i, data = analytic2_2, family = binomial)
  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
  attributes(predictions_list[[i]]) <- NULL
  final_list[[i]] <- cbind(analytic2_2 %>% filter(state == i),data.frame(predict = predictions_list[[i]]))
}

m2_2_expected_v_df_v2_ca <- do.call('rbind',final_list)

results_list <- list()

for (i in unique(analytic2_2_ca$state)) {
  glm <- glm(CLF ~ newcases_hat + SEX + agebin + raceth + EEDUC +
               C1_sum1 + C2_sum1 + C3_sum1 + C4_sum1, subset = state == i, data = analytic2_2, family = binomial)
  a <- tidy(glm)
  aic <- AIC(glm)
  results_list[[i]] <- cbind(i,a)
  aic_list[[i]] <- cbind(i,aic)
}


m2_2_results_v2_ca <- do.call('rbind',results_list)
m2_2_aic_v2_ca <- data.frame(do.call('rbind',aic_list))
m2_2_expecteds_v2_ca_compare <- m2_2_expected_v_df_v2_ca %>%
  group_by(state,yearmonth) %>%
  summarise(samp_size = n(),
            CLF = sum(CLF),
            predictions = sum(predict))

range_write(m2_2_results_v2_ca, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.2 v2 CAglm!A:F')
range_write(m2_2_aic_v2_ca, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.2 v2 CAglm!H:I')
range_write(m2_2_expecteds_v2_ca_compare, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.2 v2 CAglm!K:O')

