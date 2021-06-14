raw <- read.csv('https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv')

restrictions <- read.csv('https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv') %>%
  filter(Jurisdiction == 'STATE_WIDE') %>%
  mutate(date = as.Date(as.character(Date),format = '%Y%m%d'),
         state = recode(RegionName, 'Washington DC' = 'District of Columbia')) %>%
  rename(C1 = C1_School.closing,
         C2 = C2_Workplace.closing,
         C3 = C3_Cancel.public.events,
         C4 = C4_Restrictions.on.gatherings,
         C5 = C5_Close.public.transport,
         C6 = C6_Stay.at.home.requirements,
         C7 = C7_Restrictions.on.internal.movement,
         C8 = C8_International.travel.controls) %>%
  select(state,date,C1,C2,C3,C4,C5,C6,C7,C8)

summary.factor(restrictions$state)

##different set
raw2 <- read.csv('https://raw.githubusercontent.com/COVID19StatePolicy/SocialDistancing/master/data/USstatesCov19distancingpolicy.csv')
