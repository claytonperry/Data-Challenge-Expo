#Set up and pull ACS data

states <- unique(tidycensus::fips_codes[,1:3])[0:51,] %>%
  rename(stusps = state,
         fips = state_code,
         state = state_name)

#install.packages('tidyverse')
#install.packages('tidycensus')

library(tidyverse)
library(tidycensus)

#using tidycensus: https://cran.r-project.org/web/packages/tidycensus/tidycensus.pdf

Sys.setenv(CENSUS_API_KEY = '5dc30770cc0133357b32b987a7bdd7f51e1b4a80')

# ACS 2019 PUMS Data Dictionary: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2019.pdf

#example loading PUMS
acs_list <- list()

for (i in (unique(tidycensus::fips_codes$state)[0:51])) {
acs_list[[i]] <- get_pums(variables = c('SEX',"AGEP",'ESR','COW', 'RAC1P','NWAV','SCHL','FESRP','HISP','POWSP','ST','PWGTP','PUMA'),
                            state = i, survey = "acs1", year = 2019, recode ="TRUE", rep_weights = 'person')
}

acs_19_1yr_pums <- do.call(rbind,acs_list) %>%
  rename(fips = ST) %>%
  inner_join(states, by = 'fips')
