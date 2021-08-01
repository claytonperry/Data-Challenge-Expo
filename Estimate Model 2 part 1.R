library(tidyverse)
library(survey)
library(broom)
library(googlesheets4)

analytic2_1 <- schedule %>%
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
  inner_join(m2_0_monthly, by = c('yearmonth', 'state')) %>%
  inner_join(restrictions_mnth, by = c('yearmonth','state')) %>%
  select(Imsi, sex, state, yearmonth, newcases_hat, PWEIGHT, weight, EEDUC, raceth, agebin, contains('C')) %>%
  mutate(id = row_number())

glm_list <- list()
predictions_list <- list()
final_list <- list()

d <- svydesign(ids = ~1, weights = ~PWEIGHT, data = analytic2_1)

# Run with indicators for each level

#for (i in unique(analytic2_1$state)) {
#  glm_list[[i]] <- glm(Imsi ~ newcases_hat + sex + agebin + raceth + EEDUC +
#                          C1_c0 + C1_c1 + C1_c2 + C1_c3 +
#                          C2_c0 + C2_c1 + C2_c2 + C2_c3 +
#                          C3_c0 + C3_c1 + C3_c2 + 
#                          C4_c0 + C4_c1 + C4_c2 + C4_c3 + C4_c4 +
#                          C5_c0 + C5_c1 + C5_c2 + 
#                          C6_c0 + C6_c1 + C6_c2 + C6_c3 +
#                          C7_c0 + C7_c1 + C7_c2 + 
#                          C8_c0 + C8_c1 + C8_c2 + C8_c3 + C8_c4, subset = state == i, data = analytic2_1, family = binomial)
#  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
#  attributes(predictions_list[[i]]) <- NULL
#  final_list[[i]] <- cbind(analytic2_1 %>% filter(state == i),data.frame(predict = predictions_list[[i]]))
#}


#m2_1_expected_v_df_v1 <- do.call('rbind',final_list)

#results_list <- list()

#for (i in unique(analytic2_1$state)) {
#  glm <- glm(Imsi ~ newcases_hat + sex + agebin + raceth + EEDUC +
#                     C1_c0 + C1_c1 + C1_c2 + C1_c3 +
#                     C2_c0 + C2_c1 + C2_c2 + C2_c3 +
#                     C3_c0 + C3_c1 + C3_c2 + 
#                     C4_c0 + C4_c1 + C4_c2 + C4_c3 + C4_c4 +
#                     C5_c0 + C5_c1 + C5_c2 + 
#                     C6_c0 + C6_c1 + C6_c2 + C6_c3 +
#                     C7_c0 + C7_c1 + C7_c2 + 
#                     C8_c0 + C8_c1 + C8_c2 + C8_c3 + C8_c4, subset = state == i, data = analytic2_1, family = binomial)
#  a <- tidy(glm)
#  aic <- AIC(glm)
#  results_list[[i]] <- cbind(i,a)
#  aic_list[[i]] <- cbind(i,aic)
#}

#results_v1 <- do.call('rbind',results_list)
#aic_v1 <- data.frame(do.call('rbind',aic_list))
#m2_1_expecteds_v1_compare <- m2_1_expected_v_df_v1 %>%
#  group_by(state,yearmonth) %>%
#  summarise(imsi = sum(Imsi),
#            predictions = sum(predict))

#range_write(results_v1, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.1 v1!A:F')
#range_write(aic_v1, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.1 v1!H:I')
#range_write(m2_1_expecteds_v1_compare, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.1 v1!K:N')

## Run using sum version 1

for (i in unique(analytic2_1$state)) {
  glm_list[[i]] <- glm(Imsi ~ newcases_hat + sex + agebin + raceth + EEDUC +
                            C1_sum1 + C2_sum1 + C3_sum1 + C4_sum1 +
                            C5_sum1 + C6_sum1 + C7_sum1 + C8_sum1, subset = state == i, data = analytic2_1, family = binomial)
  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
  attributes(predictions_list[[i]]) <- NULL
  final_list[[i]] <- cbind(analytic2_1 %>% filter(state == i),data.frame(predict = predictions_list[[i]]))
}

m2_1_expected_v_df_v2 <- do.call('rbind',final_list)

results_list <- list()

for (i in unique(analytic2_1$state)) {
  glm <- glm(Imsi ~ newcases_hat + sex + agebin + raceth + EEDUC +
                     C1_sum1 + C2_sum1 + C3_sum1 + C4_sum1 +
                     C5_sum1 + C6_sum1 + C7_sum1 + C8_sum1, subset = state == i, data = analytic2_1, family = binomial)
  a <- tidy(glm)
  aic <- AIC(glm)
  results_list[[i]] <- cbind(i,a)
  aic_list[[i]] <- cbind(i,aic)
}

results_v2 <- do.call('rbind',results_list)
aic_v2 <- data.frame(do.call('rbind',aic_list))
m2_1_expecteds_v2_compare <- m2_1_expected_v_df_v2 %>%
  group_by(state,yearmonth) %>%
  summarise(samp_size = n(),
            imsi = sum(Imsi),
            predictions = sum(predict))

range_write(results_v2, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.1 v2!A:F')
range_write(aic_v2, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.1 v2!H:I')
range_write(m2_1_expecteds_v2_compare, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.1 v2!K:N')

## Run using sum version 2

#for (i in unique(analytic2_1$state)) {
#  glm_list[[i]] <- glm(Imsi ~ newcases_hat + sex + agebin + raceth + EEDUC +
#                            C1_sum2 + C2_sum2 + C3_sum2 + C4_sum2 +
#                            C5_sum2 + C6_sum2 + C7_sum2 + C8_sum2, subset = state == i, data = analytic2_1, family = binomial)
#  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
#  attributes(predictions_list[[i]]) <- NULL
#  final_list[[i]] <- cbind(analytic2_1 %>% filter(state == i),data.frame(predict = predictions_list[[i]]))
#}

#m2_1_expected_v_df_v3 <- do.call('rbind',final_list)

#results_list <- list()

#for (i in unique(analytic2_1$state)) {
#  glm <- glm(Imsi ~ newcases_hat + sex + agebin + raceth + EEDUC +
#                     C1_sum2 + C2_sum2 + C3_sum2 + C4_sum2 +
#                     C5_sum2 + C6_sum2 + C7_sum2 + C8_sum2, subset = state == i, data = analytic2_1, family = binomial)
#  a <- tidy(glm)
#  aic <- AIC(glm)
#  results_list[[i]] <- cbind(i,a)
#  aic_list[[i]] <- cbind(i,aic)
#}

#results_v3 <- do.call('rbind',results_list)
#aic_v3 <- data.frame(do.call('rbind',aic_list))
#m2_1_expecteds_v3_compare <- m2_1_expected_v_df_v3 %>%
#  group_by(state,yearmonth) %>%
#  summarise(imsi = sum(Imsi),
#            predictions = sum(predict))

#range_write(results_v3, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.1 v3!A:F')
#range_write(aic_v3, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.1 v3!H:I')
#range_write(m2_1_expecteds_v3_compare, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.1 v3!K:N')



##CHRYS - subset analysis for Richard
## Run using sum version 1

analytic2_1_ca <- analytic2_1 %>%
  filter(state == 'California')
#glm
for (i in unique(analytic2_1_ca$state)) {
  glm_list[[i]] <- glm(Imsi ~ newcases_hat + sex + agebin + raceth + EEDUC +
                         C1_sum1 + C2_sum1 + C3_sum1 + C4_sum1, subset = state == i, data = analytic2_1, family = binomial)
  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
  attributes(predictions_list[[i]]) <- NULL
  final_list[[i]] <- cbind(analytic2_1 %>% filter(state == i),data.frame(predict = predictions_list[[i]]))
}

m2_1_expected_v_df_v2_ca <- do.call('rbind',final_list)

results_list <- list()

for (i in unique(analytic2_1_ca$state)) {
  glm <- glm(Imsi ~ newcases_hat + sex + agebin + raceth + EEDUC +
               C1_sum1 + C2_sum1 + C3_sum1 + C4_sum1, subset = state == i, data = analytic2_1, family = binomial)
  a <- tidy(glm)
  aic <- AIC(glm)
  results_list[[i]] <- cbind(i,a)
  aic_list[[i]] <- cbind(i,aic)
}

results_v2_ca <- do.call('rbind',results_list)
aic_v2_ca <- data.frame(do.call('rbind',aic_list))
m2_1_expecteds_v2_ca_compare <- m2_1_expected_v_df_v2_ca %>%
  group_by(state,yearmonth) %>%
  summarise(samp_size = n(),
            imsi = sum(Imsi),
            predictions = sum(predict))

range_write(results_v2_ca, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.1 v2 CAglm!A:F')
range_write(aic_v2_ca, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.1 v2 CAglm!H:I')
range_write(m2_1_expecteds_v2_ca_compare, ss = '1w89IU3xGa__wGtFBG-sHBmo7QSGs1NXF9rItS0m2fdo', range = 'Model 2.1 v2 CAglm!K:O')


#svyglm
for (i in unique(analytic2_1_ca$state)) {
  glm_list[[i]] <- svyglm(Imsi ~ newcases_hat + sex + agebin + raceth + EEDUC +
                         C1_sum1 + C2_sum1 + C3_sum1 + C4_sum1, d, subset = state == i, data = analytic2_1, family = binomial)
  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
  attributes(predictions_list[[i]]) <- NULL
  final_list[[i]] <- cbind(analytic2_1 %>% filter(state == i),data.frame(predict = predictions_list[[i]]))
}
#add more code once you get the svyglm to work...
