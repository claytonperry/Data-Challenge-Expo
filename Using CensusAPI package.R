install.packages('censusapi')
library(censusapi)

Sys.setenv(CENSUS_KEY = '5dc30770cc0133357b32b987a7bdd7f51e1b4a80')

readRenviron('~/.Renviron')
Sys.getenv('CENSUS_API_KEY')

apis <- listCensusApis()
view(apis)

acs5_vars <-  listCensusMetadata(
  name = 'acs/acs5',
  vintage = 2015
)

view(acs_vars)

pov_vars <- listCensusMetadata(
  name = 'timeseries/poverty/histpov2',
  type = 'variables'
)

head(pov_vars)

cps_asec <- getCensus(
  name = 'timeseries/poverty/histpov2',
  vars = c('YEAR','FEMHHPOV','RACE','PCTPOV','PCTFEMHHPOV')
)

cps_asec <- cps_asec %>%
  mutate(Race = recode(RACE, `1` = 'All Race', `2` = 'White Alone', `3` = 'White', `4` = 'White Alone, Not Hispanic', `5` = 'White, Not Hispanic', `6` = 'Black Alone or in Combination', `7` = 'Black Alone', `8` = 'Black', `9` = 'Asian Alone or in Combination', `10` = 'Asian Alone', `11` = 'Asian and Pacific Islander', `12` = 'Hispanic (of any race)'))

attach(cps_asec)
plot(YEAR,PCTFEMHHPOV,)

ggplot(cps_asec, aes(YEAR,PCTFEMHHPOV,group = Race, colour = Race)) +
  geom_point() +
  geom_smooth() +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  scale_y_discrete(guide = guide_axis(check.overlap = TRUE))

cov(PCTFEMHHPOV,PCTPOV)

