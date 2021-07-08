## Estimate Simple Model 2 Part 1

# Step 0: Run the following programs
# 1. Pull ACS Data.R
# 2. Read in Oxford Covid-19 Goernment Response tracker data.R
# 3. Pull covid data.R

# Step 1: Call relevant libraries
library(tidyverse)
library(survey)
library(broom)
library(googlesheets4)
library(labelled)
library(dplyr)

# Step 2: Create necessary functions
l1_ind <- function(x, na.rm = FALSE) (ifelse(x = 1, 1, 0))
l2_ind <- function(x, na.rm = FALSE) (ifelse(x = 2, 1, 0))
l3_ind <- function(x, na.rm = FALSE) (ifelse(x = 3, 1, 0))

min1_ind <- function(x, na.rm = FALSE) (min(d[x == 1]))
min2_ind <- function(x, na.rm = FALSE) (min(d[x == 2]))
min3_ind <- function(x, na.rm = FALSE) (min(d[x == 3]))

fade1 <- function(x, na.rm = FALSE) (1 / (1 + exp(d - min1_ind - 8)))
fade2 <- function(x, na.rm = FALSE) (1 / (1 + exp(d - min2_ind - 8)))
fade3 <- function(x, na.rm = FALSE) (1 / (1 + exp(d - min3_ind - 8)))

z1  <- function(x, na.rm = FALSE) (fade1 * l1_ind)
z2  <- function(x, na.rm = FALSE) (fade2 * l2_ind)
z3  <- function(x, na.rm = FALSE) (fade3 * l3_ind)

comprehensive1 <- function(x, d, na.rm = FALSE) ((1/ (1 + exp(d - min(d[x == 1]) - 8))) * ifelse(x = 1, 1, 0))
comprehensive2 <- function(x, d, na.rm = FALSE) ((1/ (1 + exp(d - min(d[x == 2]) - 8))) * ifelse(x = 2, 1, 0))
comprehensive3 <- function(x, d, na.rm = FALSE) ((1/ (1 + exp(d - min(d[x == 3]) - 8))) * ifelse(x = 3, 1, 0))

st_pop <- acs_19_1yr_pums %>%
  group_by(state) %>%
  summarise(pop = sum(PWGTP))

analytic2_1 <- st_pop %>%
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
         cds = newcases/pop,
         across('C',list(i1 = l1_ind, i2 = l2_ind, i3 = l3_ind))) %>%
  group_by(state)
  summarise(across(C1:C8, list(min1 = min1_ind, min2 = min2_ind, min3 = min3_ind))) %>%
  ungroup() %>%
  mutate(across('C', list(f1 = fade1, f2 = fade2, f3 = fade3)),
         across('C', list(z1 = z1, z2 = z2, z3 = z3)))

## COMPREHENSIVE FUNCTION VERSION

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
           cds = newcases_d/pop)
    
  analytic2_1 <- analytic2_0 %>%
    group_by(state) %>%
    mutate(across(C1:C8,list(z1 = comprehensive1(.,'d'), z2 = comprehensive2(.,'d'), z3 = comprehensive3(.,'d')))) %>%
    ungroup()
  
# Step 2: Define Survey Design

design <- svydesign(ids = ~1, weights = PWGTP, data = analytic2_1)

# Step 3: Prepare for loop

glm_list <- list()
predictions_list <- list()
final_list <- list()

# Step 4: Run loop through states

for (i in unique(analytic2_1$state)) {
  glm_list[[i]] <- glm(cds ~ x1d + C1_z1 + C1_z2 + C1_z3 +
                         C2_z1 + C2_z2 + C2_z3 + C3_z1 + C3_z2 + C3_z3 +
                         C4_z1 + C4_z2 + C4_z3 + C5_z1 + C5_z2 + C5_z3 +
                         C6_z1 + C6_z2 + C6_z3 + C1_z1 + C7_z2 + C7_z3 +
                         C8_z1 + C8_z2 + C8_z3, family = gamma())
  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
  attributes(predictions_list[[i]]) <- NULL
  final_list[[i]] <- cbind(analytic2 %>% filter(state == i),data.frame(predict = predictions_list[[i]]))
}

# Step 5: Bind final lists and create predicted daily proportion

v_df <- do.call('rbind',final_list) %>%
  mutate(cds_hat = predict * pop)
  