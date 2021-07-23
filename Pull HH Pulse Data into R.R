#install/call packages, grant access to R

#install.packages('tidyverse')
#install.packages('stringr')
#install.packages('googledrive')
#install.packages('googlesheets4')
#install.packages('lubridate')
library(tidyverse)
library(stringr)
library(googledrive)
library(googlesheets4)
library(lubridate)

drive_auth()
gs4_auth()

#create sequences for pulls and dataframes

puf_files <- drive_find(pattern = 'pulse2020_puf|pulse2021_puf')
repwgt_files <- drive_find(pattern = 'pulse2020_rep|pulse2021_rep')

numweeks <- puf_files %>%
  mutate(week = as.numeric(substr(name,str_locate(name,'f_')+2,str_locate(name,'f_')+3))) %>%
  pull(week)

#create puf df

puf_list <- list()

for (i in numweeks) {
  temp <- tempfile(fileext = ".csv")
  id <- puf_files[[i,2]]
  dl <- drive_download(
    as_id(id), path = temp, overwrite = TRUE)
  j <- read.csv(temp)
  puf_list[[i]] <- j %>%
    select(WEEK,RHISPANIC,RRACE,EEDUC,ANYWORK,RSNNOWRK,EST_ST,PWEIGHT,EGENDER,TBIRTH_YEAR)
}

puf_df <- do.call('rbind',puf_list) %>%
  mutate(agecont = 2021 - TBIRTH_YEAR,
         agebin = case_when(agecont < 37 ~ 1,
                            agecont < 48 ~ 2,
                            agecont < 58 ~ 3,
                            agecont < 68 ~ 4,
                            TRUE ~ 5),
         raceth = case_when(RHISPANIC == 1 ~ 1,
                            RRACE == 1 ~ 2,
                            RRACE == 2 ~ 3,
                            RRACE == 3 ~ 4,
                            RRACE == 4 ~ 5,
                            TRUE ~ 9))

#QC creation of agebin and raceth
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(puf_df, row.vars = "agecont", col.vars = "agebin", type = "f")
crosstab(puf_df, row.vars = c("RHISPANIC","RRACE"), col.vars = "raceth", type = "f")

#create repweight df

repwgt_list <- list()

for (i in numweeks) {
  temp <- tempfile(fileext = ".csv")
  id <- repwgt_files[[i,2]]
  dl <- drive_download(
    as_id(id), path = temp, overwrite = TRUE)
  j <- read.csv(temp)
  repwgt_list[[i]] <- j %>%
    select(WEEK,SCRAM,contains('PWEIGHT'))
}

repwgt_df <- do.call('rbind',repwgt_list)


## Schedule import

schedule <- read_sheet('1x0dA4IFh_pLkXd5nsBIg5oYmBnqx5m7sGOL8_RllNWA',
                       range = 'Schedule!A:D') %>%
  rename(week = Week) %>%
  filter(week %in% c(2,6,10,13,14,17,19,21,22,24,26)) %>%
  mutate(year = year(Time_Period_Begin),
         monthname = month(Time_Period_Begin,label = TRUE),
         yearmonth = paste0(year(Time_Period_Begin),' ',month(Time_Period_Begin, label = T, abbr = F))) %>%
  select(Phase,week,year,monthname,yearmonth)

  