#install/call packages, grant access to R

#install.packages('tidyverse')
#install.packages('stringr')
#install.packages('googledrive')
#install.packages('googlesheets4')
library(tidyverse)
library(stringr)
library(googledrive)
library(googlesheets4)

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
    select(WEEK,RHISPANIC,RRACE,EEDUC,ANYWORK,RSNNOWRK,EST_ST,PWEIGHT,EGENDER)
}

puf_df <- do.call('rbind',puf_list)

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
  mutate(midpoint = as.Date(as.POSIXct((as.numeric(Time_Period_Begin) + as.numeric(Time_Period_End)) / 2,
                               origin = '1970-01-01'))) %>%
  select(Phase,week,midpoint)

  