#install.packages('tidyverse')
#install.packages('tidycensus')

library(tidyverse)
library(tidycensus)

#using tidycensus: https://cran.r-project.org/web/packages/tidycensus/tidycensus.pdf

census_api_key('5dc30770cc0133357b32b987a7bdd7f51e1b4a80', install = TRUE, overwrite = TRUE)

# ACS 2019 PUMS Data Dictionary: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2019.pdf

#example loading PUMS
acs_5yr_19_pums <- get_pums(variables = c("AGEP","ANC1P",'COW'), state ="VT", survey = "acs5", year = 2019, recode ="TRUE")

