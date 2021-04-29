#Analytic File for Modeling
#This code combines data into one or 2 analytic files used for modeling

#install.packages('tidyverse') 
library(tidyverse)

#######
#PART 1
#HH Pulse data: filter to weeks specified in spec, create Imsi indicator for DV
#requires running 'Pull HH Pulse Data into R'
pulse <- puf_df %>%
  filter(WEEK %in% c(2,6,10,13,14,17,19,21,22,24,26)) %>%
  mutate(st = str_pad(EST_ST,2,side = 'left',pad = '0'),
         Imsi = ifelse(RSNNOWRK==8|RSNNOWRK==9|RSNNOWRK==10|RSNNOWRK==11, 1, 0),
         month = ifelse(WEEK==2,'May',
                      ifelse(WEEK==6,'June',
                      ifelse(WEEK==10,'July',
                      ifelse(WEEK==13,'August',
                      ifelse(WEEK==14,'September',
                      ifelse(WEEK==17,'October',
                      ifelse(WEEK==19,'November',
                      ifelse(WEEK==21,'December',
                      ifelse(WEEK==22,'January',
                      ifelse(WEEK==24,'February',
                      ifelse(WEEK==26,'March',NA)))))))))))) %>%
  select(WEEK,RSNNOWRK,EST_ST,EGENDER,PWEIGHT,Imsi,st,month) %>%
  full_join(states, by = 'st') 
  

#?? stuck here, getting the month of the date isn't working
#Covid data: summarize covid cases by month and state
#requires running 'Pull covid data'
conf_month <- conf %>%
  mutate(month = month(date)) %>%
  group_by(state,month) %>%
  summarize(Cms = sum(cases)) %>%
  select(Cms,state,month)
  
#merge pulse and conf_month data to get analytic file for part 1
  
  
  
  
#######
#PART 2
