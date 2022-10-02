############ PREPARE STRAVA DATA #############
# Libabies----
library(tidyverse)
library(magrittr)
library(lubridate)
library(R.utils) # To decompress gz files
library(cycleRtools) # To parse Garmin .fit files
library(here)
# Download all strava data from personal account. They will email you a zip file.


readDataPath <- './export_38658371/activities'
# Replace the compressed .gz files with the uncompressed files----
# Only need to run this once. The result is that the data files should all be changed from .fit.gz to .fit
compFiles <- list.files(readDataPath) %>% .[which(grepl('.gz', x = .))] # Only keeps it if there are zipped files
for(i in compFiles){
  R.utils::gunzip(filename = paste0(readDataPath, '/', i)
                  # , destname = paste0(readDataPath, '/', i)
  )
  cat(i, '\r')
}

# Read in a list of activities----
activities <- read.csv2(paste0(gsub('/\\w{1,}$', '', readDataPath), '/activities.csv')
                        , header = T, sep = ',', stringsAsFactors = F) %>%
  dplyr::mutate(
    date = mdy_hms(Activity.Date) #%>% with_tz('America/Denver') # Convert to Utah time
  )
names(activities) <- gsub('\\.', '_', names(activities)) %>% tolower()
# Read in the files and wrangle (Can skip this if you've already done it once, and just read in the .rds file)----
uncompFiles <- list.files(readDataPath)
ufLength <- length(uncompFiles)
allData <- data.frame(stringsAsFactors = F)
j <- 1
for(i in uncompFiles){
  cat('Starting', j, 'of', ufLength, ':   ', i)
  tryCatch({
    temp <- cycleRtools::read_fit(paste0(readDataPath, '/', i), format = F)
    colnames(temp) <- gsub('\\.$', '', colnames(temp)) %>% gsub('\\.', '_', .) %>% gsub('record_', '', .)
    
    temp %<>% dplyr::mutate(
      timeStampUTC = lubridate::as_datetime(timestamp_s
                                            , origin = ymd_hms('1989-12-31 00:00:00') # https://forums.garmin.com/forum/into-sports/cycling/1245820-how-to-convert-timestamp
                                            , tz = 'UTC')
      , lap = as.character(lap) %>% as.numeric()
      , activityNumber = j
      , activityName = activities$Activity.Name[j]
      , rowId = row.names(.)
    ) %>%
      dplyr::select(-timestamp_s)
    temp$date = as.character(temp[1,'timeStampUTC']) %>% gsub(' UTC', '', .)
    
    if('position_lat_semicircles' %in% colnames(temp)){
      temp %<>% dplyr::mutate(
        lat = position_lat_semicircles*(180/2^31)
        , long = position_long_semicircles*(180/2^31)
      ) %>%
        dplyr::select(-position_lat_semicircles, -position_long_semicircles)
    }
    
    
    allData %<>% bind_rows(temp)
  }, error = function(e){
    cat('Problem with', i)
  })
  
  cat('Finished\n', '_____________________________\n')
  j <- j+1
}
system('say Done my man!')
allData %<>% mutate(
  date = ymd_hms(date)
)

# Add in activity data----
# Read in a list of activities----
activities <- read.csv2(paste0(gsub('/\\w{1,}$', '', readDataPath), '/activities.csv')
                        , header = T, sep = ',', stringsAsFactors = F) %>%
  dplyr::mutate(
    date = mdy_hms(Activity.Date) #%>% with_tz('America/Denver') # Convert to Utah time
  )
names(activities) <- gsub('\\.', '_', names(activities)) %>% tolower()
# Add in activity type----
allData %<>% left_join(activities[,c('date', 'activity_id', 'activity_name')], by = 'date') %>%
  dplyr::arrange(activityNumber)
# Wrangle data----
allData %<>% 
  dplyr::mutate(
    # date = ymd_hms(date) %>% with_tz('America/Denver')
    altitude_f = altitude_m/.328084
    , rowId = as.numeric(rowId)
    , distance_mi = distance_m*.000621371
    , speed_m_h = speed_m_s*60*60*.000621371
    , activityTitle = paste0(activityNumber, '-'
                             , activity_name, ', '
                             , month(date), '/'
                             , mday(date), '/'
                             , as.character(year(date)) %>% substr(., 3, 4)
    )
  )
# Write data to file----
write_rds(allData, 'activityData.rds', compress = 'gz')


