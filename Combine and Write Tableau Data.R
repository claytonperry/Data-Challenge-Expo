
library(googlesheets4)
library(tidyverse)

gs4_auth()

1

viz_data <- scenario1_viz %>%
  rename(newcases1 = newcases,
         UE_unweight1 = UE_unweight,
         UE_weight1 = UE_weight,
         CLF_unweight1 = CLF_unweight,
         CLF_weight1 = CLF_weight) %>%
  inner_join(scenario2_viz, by = c('state','yearmonth')) %>%
  rename(newcases2 = newcases,
         UE_unweight2 = UE_unweight,
         UE_weight2 = UE_weight,
         CLF_unweight2 = CLF_unweight,
         CLF_weight2 = CLF_weight) %>%
  inner_join(scenario3_viz, by = c('state','yearmonth')) %>%
  rename(newcases3 = newcases,
         UE_unweight3 = UE_unweight,
         UE_weight3 = UE_weight,
         CLF_unweight3 = CLF_unweight,
         CLF_weight3 = CLF_weight)

write_sheet(viz_data, ss = '1U_qzjiqKhBiPqc7CMXay9NMITL949iK6MEPt8jv7hcI', sheet = 'Sheet1')
