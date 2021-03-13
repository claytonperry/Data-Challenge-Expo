#Set up and Pull BLS data

#install.packages('blsAPI')
library(blsAPI)

Sys.setenv(BLS_KEY = '9ecd688f35134033b93b83dfd06aaaee') #will get this when i get back to DC

#Set product we're interested, in this case the LAU tables

product <- 'LAU'

#Set area we're interested in, in this case every state

FIPS <- c('01','02','04','05','06','08','09','10','12','13','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31','32'
,'33','34','35','36','37','38','39','40','41','42','44','45','46','47','48','49','50','51','53','54','55','56')

areas <- paste0('ST',FIPS,'00000000000')

#Set measurement we're interested in, in this case unemployment rate

measurement <- '03'

#Put it all together as seriesIDs

series <- paste0(product,areas,measurement)

#Create API payload

payload <- list('seriesid' = series,
        'startyear' = 2020,
        'endyear' = 2021)

#pull API response from payload

df <- blsAPI(payload,return_data_frame = TRUE)

#add normal statecode back

