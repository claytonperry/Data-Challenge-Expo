#Install relevant libraries
#install.packages('readxl')
#install.packages('httr')

#Call relevant libraries
library(readxl)
library(httr)

#set relevant column name vector, skip number, vector of week numbers

table <- #table you're pulling (eg. employ2)
colnames <- paste0(table,'_weekx_cols')
skips <- paste0(table,'_weekx_skip')
weeks <- seq(1,16)


#create online read function for relevant dataset

readxl_online <- function(url, type = NULL, ...) {
  test <- stringr::str_detect(url, "[.]xls|[.]zip")
  if (test == FALSE) {
    print(message("Expecting file extension of type .xlsx, .xls or .zip. Check the URL or the data source for the correct file extension and use the type argument"))
  }
  # test for individual file extensions for xls use look forward, xls not
  # followed by x
  t1 <- stringr::str_detect(url, "[.]xlsx")
  t2 <- stringr::str_detect(url, "[.]xls(?!x)")
  tz <- stringr::str_detect(url, "[.]zip")
  if (t1 == TRUE) {
    type = ".xlsx"
  }
  if (t2 == TRUE) {
    type = ".xls"
  }
  if (tz == TRUE) {
    httr::GET(url, write_disk("tmp.zip", overwrite = TRUE))
    tmp <- unzip("tmp.zip")
    # On osx more than one file name is returned, select first element.
    df <- readxl::read_excel(tmp[[1]])
    return(df)
  }
  if (!is.null(type)) {
    type = type
    
  }
  df <- httr::GET(url, write_disk(paste0("tmp", type), overwrite = TRUE))
  df <- readxl::read_excel(paste0("tmp", type),skip=7,col_names = colnames, skip = skips)
  
}

# loop to create a df for each week's table

for (i in weeks) {
  url <- paste0('https://www2.census.gov/programs-surveys/demo/tables/hhp/2020/wk',i,'/employ2_week',i,'.xlsx')
  framename <- paste0(table,'_week',i)
  df <- readxl_online(url = url)
  assign(framename,df)
}

