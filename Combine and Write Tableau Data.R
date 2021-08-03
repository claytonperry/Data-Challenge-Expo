
library(googlesheets4)
library(tidyverse)

viz_data <- rbind(scenario1_viz, scenario2_viz, scenario3_viz)

write_sheet(viz_data, ss = , sheet = 'Sheet1')