#Analytic File for Modeling
#This code combines data into one or 2 analytic files used for modeling

#install.packages('tidyverse') 
library(tidyverse)

#HH Pulse data
#requires running 'Pull HH Pulse Data into R'
#requires running 'Pull covid data'
pulse <- puf_df %>%
  filter(WEEK %in% c(2,6,10,13,14,17,19,21,22,24,26)) %>%
  mutate(Imsi = ifelse(RSNNOWRK==8|RSNNOWRK==9|RSNNOWRK==10|RSNNOWRK==11, 1, 0)) %>%
  select(WEEK,RSNNOWRK,EST_ST,EGENDER,PWEIGHT,Imsi)
