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

st_pop <- acs_2019_1yr_pums %>%
  group_by(ST) %>%
  summarise(pop = sum(PWGTP))

analytic2_1 <- st_pop %>%
  inner_join(confdaily, by = 'state') %>%
  inner_join(restrictions, by = c('state','date')) %>%
  group_by(state) %>%
  mutate(d = row_number(),
         cds = newcases/pop) %>%
  ungroup() %>%
  mutate(sinfd = 0.5 * (sin((d/20.69) - 1.82) + 1),
         sfd = ifelse(d <= 366,
                      (abs(d - 183)/366) + 0.5,
                      (abs(d - 549)/365) + 0.5),
         x1d = ifelse(d >= 150,
                      sfd * sinfd,
                      (d/150) * (0.5 * (sin((150/20.69) - 1.82) + 1)) * (abs(150 - 183)/366) + 0.5),
         I1_1 = ifelse(C1 = 1, 1, 0),
         I1_2 = ifelse(C1 = 2, 1, 0),
         I1_3 = ifelse(C1 = 3, 1, 0)
         ) %>%
  summarise(D1_1 = min(d[I1_1 == 1]),
            D1_2 = min(d[I1_2 == 1]),
            D1_3 = min(d[I1_3 == 1])
            ) %>%
  mutate(fade1_1 = 1 / (1 + exp(d - D1_1 - 8)),
         fade1_2 = 1 / (1 + exp(d - D1_2 - 8)),
         fade1_3 = 1 / (1 + exp(d - D1_3 - 8)),
         z1_1 = fade1_1 * I1_1,
         z1_2 = fade1_2 * I1_2,
         z1_3 = fade1_3 * I1_3)

# Step 2: Define Survey Design

design <- svydesign(ids = ~1, data = analytic2_1)

# Step 3: Prepare for loop

glm_list <- list()
predictions_list <- list()
final_list <- list()

# Step 4: Run loop through states

for (i in unique(analytic2_1$state)) {
  glm_list[[i]] <- glm(cds ~ x1d + z1_1 + z1_2 + z1_3, family = gamma())
  predictions_list[[i]] <- predict(glm_list[[i]], type = 'response')
  attributes(predictions_list[[i]]) <- NULL
  final_list[[i]] <- cbind(analytic2 %>% filter(state == i),data.frame(predict = predictions_list[[i]]))
}

# Step 5: Bind final lists and create predicted daily proportion

v_df <- do.call('rbind',final_list) %>%
  mutate(cds_hat = predict * pop)
  