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
  select(week)


puf_list <- list()

for (i in numweeks$week) {
  temp <- tempfile(fileext = ".csv")
  id <- puf_files[[i,2]]
  dl <- drive_download(
    as_id(id), path = temp, overwrite = TRUE)
  puf_list[[i]] <- read.csv(temp)
}

puf_df <- do.call('rbind',puf_list)

repwgt_list <- list()

for (i in numweeks$week) {
  temp <- tempfile(fileext = ".csv")
  id <- repwgt_files[[i,2]]
  dl <- drive_download(
    as_id(id), path = temp, overwrite = TRUE)
  puf_list[[i]] <- read.csv(temp)
}

repwgt_df <- do.call('rbind',repwgt_list)
