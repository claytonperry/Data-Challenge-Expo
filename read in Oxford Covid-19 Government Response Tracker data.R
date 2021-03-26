raw <- read.csv('https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv')

raw %>%
  mutate(Date = as.Date(as.character(Date),format = '%Y%m%d')) %>%
  filter(Jurisdiction == 'STATE_WIDE') %>%
  select(Date,C2_Workplace.closing)
