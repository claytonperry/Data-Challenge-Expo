#Scenario 1: Reality

#Programs to run first:
# 1. Pull ACS data.R
# 2. Read in IPUMS CPS.R
# 3. Pull HH Pulse Data into R.R
# 4. Pull_BLS_Data.R
# 5. read in Oxford Covid-19 Government Response Tracker data.R
# 6. Pull covid data.R

# Step 1. Call Relevant Libraries and define functions

library(tidyverse)
library(survey)
library(broom)
library(googlesheets4)
library(labelled)
library(dplyr)

l0_ind <- function(x, na.rm = FALSE) {ifelse(x == 0, 1, 0)}
l1_ind <- function(x, na.rm = FALSE) {ifelse(x == 1, 1, 0)}
l2_ind <- function(x, na.rm = FALSE) {ifelse(x == 2, 1, 0)}
l3_ind <- function(x, na.rm = FALSE) {ifelse(x == 3, 1, 0)}
l4_ind <- function(x, na.rm = FALSE) {ifelse(x == 4, 1, 0)}



#Step 2. Create Analytic File

st_pop <- acs_19_1yr_pums %>%
  group_by(state) %>%
  summarise(pop = sum(PWGTP))


analytic2_0 <- st_pop %>%
  inner_join(confdaily, by = 'state') %>%
  inner_join(restrictions, by = c('state','date')) %>%
  group_by(state) %>%
  mutate(d = row_number()) %>%
  ungroup() %>%
  mutate(sinfd = 0.5 * (sin((d/20.69) - 1.82) + 1),
         sfd = ifelse(d <= 366,
                      (abs(d - 183)/366) + 0.5,
                      (abs(d - 549)/365) + 0.5),
         x1d = ifelse(d >= 150,
                      sfd * sinfd,
                      (d/150) * (0.5 * (sin((150/20.69) - 1.82) + 1)) * (abs(150 - 183)/366) + 0.5),
         pds = newcases_d/pop,
         across(C1:C8,list(i0 = l0_ind, i1 = l1_ind, i2 = l2_ind, i3 = l3_ind, i4 = l4_ind))) %>%
  group_by(state) %>%
  mutate(C1_min0 = min(d[which(C1==0)]),
         C1_min1 = min(d[which(C1==1)]),
         C1_min2 = min(d[which(C1==2)]),
         C1_min3 = min(d[which(C1==3)]),
         C2_min0 = min(d[which(C2==0)]),
         C2_min1 = min(d[which(C2==1)]),
         C2_min2 = min(d[which(C2==2)]),
         C2_min3 = min(d[which(C2==3)]),
         C3_min0 = min(d[which(C3==0)]),
         C3_min1 = min(d[which(C3==1)]),
         C3_min2 = min(d[which(C3==2)]),
         C4_min0 = min(d[which(C4==0)]),
         C4_min1 = min(d[which(C4==1)]),
         C4_min2 = min(d[which(C4==2)]),
         C4_min3 = min(d[which(C4==3)]),
         C4_min4 = min(d[which(C4==4)]),
         C5_min0 = min(d[which(C5==0)]),
         C5_min1 = min(d[which(C5==1)]),
         C5_min2 = min(d[which(C5==2)]),
         C6_min0 = min(d[which(C6==0)]),
         C6_min1 = min(d[which(C6==1)]),
         C6_min2 = min(d[which(C6==2)]),
         C6_min3 = min(d[which(C6==3)]),
         C7_min0 = min(d[which(C7==0)]),
         C7_min1 = min(d[which(C7==1)]),
         C7_min2 = min(d[which(C7==2)]),
         C8_min0 = min(d[which(C8==0)]),
         C8_min1 = min(d[which(C8==1)]),
         C8_min2 = min(d[which(C8==2)]),
         C8_min3 = min(d[which(C8==3)]),
         C8_min4 = min(d[which(C8==4)])) %>%
  ungroup() %>%
  mutate(across(contains('C'), function(x) replace(x, is.infinite(x), 0))) %>%
  mutate(C1_fade0 = 1 / (1 + exp(d - C1_min0 - 8)),
         C1_fade1 = 1 / (1 + exp(d - C1_min1 - 8)),
         C1_fade2 = 1 / (1 + exp(d - C1_min2 - 8)),
         C1_fade3 = 1 / (1 + exp(d - C1_min3 - 8)),
         C2_fade0 = 1 / (1 + exp(d - C2_min0 - 8)),
         C2_fade1 = 1 / (1 + exp(d - C2_min1 - 8)),
         C2_fade2 = 1 / (1 + exp(d - C2_min2 - 8)),
         C2_fade3 = 1 / (1 + exp(d - C2_min3 - 8)),
         C3_fade0 = 1 / (1 + exp(d - C3_min0 - 8)),
         C3_fade1 = 1 / (1 + exp(d - C3_min1 - 8)),
         C3_fade2 = 1 / (1 + exp(d - C3_min2 - 8)),
         C4_fade0 = 1 / (1 + exp(d - C4_min0 - 8)),
         C4_fade1 = 1 / (1 + exp(d - C4_min1 - 8)),
         C4_fade2 = 1 / (1 + exp(d - C4_min2 - 8)),
         C4_fade3 = 1 / (1 + exp(d - C4_min3 - 8)),
         C4_fade4 = 1 / (1 + exp(d - C4_min4 - 8)),
         C5_fade0 = 1 / (1 + exp(d - C5_min0 - 8)),
         C5_fade1 = 1 / (1 + exp(d - C5_min1 - 8)),
         C5_fade2 = 1 / (1 + exp(d - C5_min2 - 8)),
         C6_fade0 = 1 / (1 + exp(d - C6_min0 - 8)),
         C6_fade1 = 1 / (1 + exp(d - C6_min1 - 8)),
         C6_fade2 = 1 / (1 + exp(d - C6_min2 - 8)),
         C6_fade3 = 1 / (1 + exp(d - C6_min3 - 8)),
         C7_fade0 = 1 / (1 + exp(d - C7_min0 - 8)),
         C7_fade1 = 1 / (1 + exp(d - C7_min1 - 8)),
         C7_fade2 = 1 / (1 + exp(d - C7_min2 - 8)),
         C8_fade0 = 1 / (1 + exp(d - C8_min0 - 8)),
         C8_fade1 = 1 / (1 + exp(d - C8_min1 - 8)),
         C8_fade2 = 1 / (1 + exp(d - C8_min2 - 8)),
         C8_fade3 = 1 / (1 + exp(d - C8_min3 - 8)),
         C8_fade4 = 1 / (1 + exp(d - C8_min4 - 8)),
         C1_z0 = C1_fade0 * C1_i0,
         C1_z1 = C1_fade1 * C1_i1,
         C1_z2 = C1_fade2 * C1_i2,
         C1_z3 = C1_fade3 * C1_i3,
         C2_z0 = C2_fade0 * C2_i0,
         C2_z1 = C2_fade1 * C2_i1,
         C2_z2 = C2_fade2 * C2_i2,
         C2_z3 = C2_fade3 * C2_i3,
         C3_z0 = C3_fade0 * C3_i0,
         C3_z1 = C3_fade1 * C3_i1,
         C3_z2 = C3_fade2 * C3_i2,
         C4_z0 = C4_fade0 * C4_i0,
         C4_z1 = C4_fade1 * C4_i1,
         C4_z2 = C4_fade2 * C4_i2,
         C4_z3 = C4_fade3 * C4_i3,
         C4_z4 = C4_fade4 * C4_i4,
         C5_z0 = C5_fade0 * C5_i0,
         C5_z1 = C5_fade1 * C5_i1,
         C5_z2 = C5_fade2 * C5_i2,
         C6_z0 = C6_fade0 * C6_i0,
         C6_z1 = C6_fade1 * C6_i1,
         C6_z2 = C6_fade2 * C6_i2,
         C6_z3 = C6_fade3 * C6_i3,
         C7_z0 = C7_fade0 * C7_i0,
         C7_z1 = C7_fade1 * C7_i1,
         C7_z2 = C7_fade2 * C7_i2,
         C8_z0 = C8_fade0 * C8_i0,
         C8_z1 = C8_fade1 * C8_i1,
         C8_z2 = C8_fade2 * C8_i2,
         C8_z3 = C8_fade3 * C8_i3,
         C8_z4 = C8_fade4 * C8_i4) %>%
  filter(pds > 0, complete.cases(.), date >= '2020-03-01')


# Step 3: Prepare for loop

glm_list <- list()
predictions_list <- list()
final_list <- list()

# Step 4: Run loop through states

for (i in unique(analytic2_0$state)) {
  glm_list[[i]] <- glm(pds ~ x1d + C1_z1 + C1_z2 + C1_z3 +
                         C2_z1 + C2_z2 + C2_z3 + C3_z1 + C3_z2 +
                         C4_z1 + C4_z2 + C4_z3 + C4_z4 + C5_z1 + C5_z2 +
                         C6_z1 + C6_z2 + C6_z3 + C7_z1 + C7_z2 +
                         C8_z1 + C8_z2 + C8_z3 + C8_z4,
                       subset = state == i, data = analytic2_0,  family = Gamma)
  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
  attributes(predictions_list[[i]]) <- NULL
  final_list[[i]] <- cbind(analytic2_0 %>% filter(state == i),data.frame(predict = predictions_list[[i]]))
}

# Step 5: Bind final lists and create predicted daily proportion

m2_0_v_df <- do.call('rbind',final_list) %>%
  mutate(cds_hat = predict * pop)


m2_0_monthly <- v_df %>%
  select(state,yearmonth,cds_hat) %>%
  group_by(state,yearmonth) %>%
  summarise(newcases = sum(cds_hat))

# Step 6: Create analytic file for model 2.1

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
  select(Imsi, sex, state, yearmonth, newcases_hat, PWEIGHT, weight, EEDUC, raceth, agebin, C1_sum1:C8_sum1) %>%
  mutate(id = row_number())

# Step 7:  Re-initialize lists for next loop

glm_list <- list()
predictions_list <- list()
final_list <- list()

# Step 8: Run loop for model 2.1

for (i in unique(analytic2_1$state)) {
  glm_list[[i]] <- glm(Imsi ~ newcases_hat + sex + agebin + raceth + EEDUC +
                         C1_sum1 + C2_sum1 + C3_sum1 + C4_sum1 +
                         C5_sum1 + C6_sum1 + C7_sum1 + C8_sum1, subset = state == i, data = analytic2_1, family = binomial)
  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
  attributes(predictions_list[[i]]) <- NULL
  final_list[[i]] <- cbind(analytic2_1 %>% filter(state == i),data.frame(predict = predictions_list[[i]]))
}

m2_1_v_df <- do.call('rbind',final_list)

#Step 9: Create analytic file for model 2.2

analytic2_2 <- schedule %>% #start with schedule 
  inner_join(m2_0_monthly, by = 'yearmonth') %>% #join in monthly new confirms for relevant months
  inner_join(CPS, by = c('yearmonth', 'state')) %>% #join in CPS data
  inner_join(restrictions_mnth, by = c('yearmonth', 'state')) %>% #join in restrictions aggregates
  filter(LABFORCE %in% c(1,2)) %>% #filter for individuals with relevant LABFORCE values
  mutate(CLF = ifelse(LABFORCE == 1, 0, 1)) %>% #create dependent variable
  select(CLF, SEX, EEDUC, agebin, raceth, WTFINL, newcases_hat, yearmonth, state,
         C1_sum1, C2_sum1, C3_sum1, C4_sum1, C5_sum1, C6_sum1, C7_sum1, C8_sum1) %>% #retain relevant variables
  remove_attributes(.,c('label','var_desc'))

# Step 10: Re-initialize lists for next loop

glm_list <- list()
predictions_list <- list()
final_list <- list()

# Step 11: Run loop for model 2.2

for (i in unique(analytic2_2$state)) {
  glm_list[[i]] <- glm(CLF ~ newcases_hat + SEX + agebin + raceth + EEDUC +
                         C1_sum1 + C2_sum1 + C3_sum1 + C4_sum1 +
                         C5_sum1 + C6_sum1 + C7_sum1 + C8_sum1, subset = state == i, data = analytic2_2, family = binomial)
  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
  attributes(predictions_list[[i]]) <- NULL
  final_list[[i]] <- cbind(analytic2_2 %>% filter(state == i),data.frame(predict = predictions_list[[i]]))
}

m2_2_v_df <- do.call('rbind',final_list)


# Step 12: Join dataframes

scenario1_viz <- schedule %>%
  inner_join(conf_monthly, by = c('state','yearmonth')) %>%
  inner_join(m2_1_v_df, by = c('state','yearmonth')) %>%
  inner_join(m2_2_v_df, by = c('state','yearmonth')) %>%
  mutate(scenario = 'scenario 1')
