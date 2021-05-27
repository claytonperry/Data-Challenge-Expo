

#install.packages('tidyverse')
#install.packages('ipumsr')
#install.packages('googledrive')


library(tidyverse)
library(ipumsr)
library(googledrive)

cps_files <- drive_find(pattern = 'cps_00001')

temp1 <- tempfile(fileext = '.csv.gz')
dl <- drive_download(
  as_id(cps_files[[which(cps_files$name == 'cps_00001.csv.gz'),2]]), path = temp1, overwrite = T
)

temp2 <- tempfile(fileext = '.xml')
dl <- drive_download(
  as_id(cps_files[[which(cps_files$name == 'cps_00001.xml'),2]]), path = temp2, overwrite = T
)

CPS <- read_ipums_micro(temp2, data_file = temp1)
