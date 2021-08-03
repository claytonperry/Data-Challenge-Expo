
library(googlesheets4)
library(tidyverse)

gs4_auth()

1

viz_data <- rbind(scenario1_viz, scenario2_viz, scenario3_viz)

write_sheet(viz_data, ss = '1U_qzjiqKhBiPqc7CMXay9NMITL949iK6MEPt8jv7hcI', sheet = 'Sheet1')
