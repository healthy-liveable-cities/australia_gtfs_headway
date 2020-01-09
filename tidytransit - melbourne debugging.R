# Carl Higgs, Alan Both 20200106
#  Following http://tidytransit.r-transit.org/articles/servicepatterns.html
# in order to de-bug Adelaide metro GTFS analysis (suprising result for Oct 2019)
# using
# http://tidytransit.r-transit.org/articles/timetable.html
# http://tidytransit.r-transit.org/articles/servicepatterns.html
# https://cran.r-project.org/web/packages/tidytransit/vignettes/frequency.html
# Carl Higgs 20200106
#  Following http://tidytransit.r-transit.org/articles/servicepatterns.html
# in order to de-bug Adelaide metro GTFS analysis (suprising result for Oct 2019)
# using
# http://tidytransit.r-transit.org/articles/timetable.html
# http://tidytransit.r-transit.org/articles/servicepatterns.html
# https://cran.r-project.org/web/packages/tidytransit/vignettes/frequency.html

library('tidytransit')
library('dplyr')
# library('tidyverse')
library('hablar')
library('sf')
# library('ggplot2')

gtfs_feeds <- list.files(recursive=TRUE, pattern="*.zip")
# gtfs_feeds <- c(
#   'gtfs/custom_gtfs_vic_ptv/gtfs_au_vic_ptv_20191004.zip'
# )

# gpkg_out <- 'D:/ntnl_li_2018_template/data/destinations/gtfs_au_ntnl_20191008_20191205/2020-01-07_tidy_transit_headway.gpkg'
gpkg_out <- '2020-01-07_tidy_transit_headway.gpkg'

all_stops <- NULL
frequent_stops <- NULL

for (feed in gtfs_feeds) {
  # feed=gtfs_feeds[2]
  gtfs <- read_gtfs(feed)
  gtfs <- gtfs %>% set_hms_times()  %>% set_date_service_table()
  # date_stub = paste0(gtfs$feed_info$feed_start_date,'_',gtfs$feed_info$feed_end_date)
  stub = strsplit(basename(feed),".",fixed=TRUE)[[1]][1]
  fileInfo = strsplit(stub,"_")[[1]]
  currentState = fileInfo[3]
  authority = fileInfo[4]
  publicationDate = as.numeric(fileInfo[5])
  

  valid_days <- c('Monday','Tuesday','Wednesday','Thursday','Friday')
  valid_service_ids <- gtfs$.$date_service_table %>% 
    mutate(dow = weekdays(date)) %>% 
    filter(dow %in% valid_days) %>%
    filter(date >= "2019-08-10" & date <= "2019-12-05" ) %>%    ### NEW ADDITION
    pull(service_id)
  
  # Mode table storing differences in GTFS feeds
  modes <- tribble(
    ~state  , ~mode  , ~agencyId, ~routeTypes   ,
    "vic"  , "tram" ,  c(3)    , c(0)          ,
    "vic"  , "train",  c(1,2)  , c(1,2)        ,
    "vic"  , "bus"  ,  c(4,6)  , c(3)          ,
    "nsw"  , "tram" ,  NA      , c(0)          ,
    "nsw"  , "train",  NA      , c(2,401)      ,
    "nsw"  , "bus"  ,  NA      , c(700,712,714),
    "nsw"  , "ferry",  NA      , c(4)          ,
    "taz"  , "bus"  ,  NA      , c(3)          ,
    "other", "tram" ,  NA      , c(0)          ,
    "other", "train",  NA      , c(1,2)        ,
    "other", "bus"  ,  NA      , c(3)          ,
    "other", "ferry",  NA      , c(4)
  )
  
  
  
  
  # currentState="vic"
  modesCurrent <- modes %>%
    filter(state==currentState)
  if(NROW(modesCurrent)==0) { # if there are no rows, then we need to use 'other' as the state
    modesCurrent <- modes %>%
      filter(state=="other")
  }
  
  mode_services <- gtfs$trips %>%
    left_join(gtfs$routes, by='route_id') %>% 
    filter(service_id%in%valid_service_ids) %>%
    mutate(agency_id=as.numeric(agency_id))
  
  
  # some feeds need filtering on just route type, some need filtering on route type and agency id (e.g., Vic)
  for (transitMode in modesCurrent$mode) {
    # transitMode="train"
    startTime=Sys.time()
    ifelse(any(is.na(modesCurrent$agencyId)), # if any of the agency ids are NA
           mode_services2 <- mode_services %>%
             filter(route_type %in% (modesCurrent%>%filter(mode==transitMode)%>%pull(routeTypes))[[1]] ) %>%
             pull(service_id) %>%
             unique(),
           
           mode_services2 <- mode_services %>%
             filter(agency_id %in% (modesCurrent%>%filter(mode==transitMode)%>%pull(agencyId))[[1]] &
                      route_type %in% (modesCurrent%>%filter(mode==transitMode)%>%pull(routeTypes))[[1]] ) %>%
             pull(service_id) %>%
             unique()
    )
    
    
    daytime_freq <- get_stop_frequency(gtfs, start_hour = 7, end_hour = 19,service_ids = mode_services2,by_route = FALSE)
    # get service days of week
    service_dow <- gtfs$calendar%>% filter(service_id%in%(daytime_freq%>%pull(service_id))) ## this braks for TAZ!
    # join service days of week to frequency table
    daytime_freq <- daytime_freq %>% left_join(service_dow%>%select(service_id,tolower(valid_days)),by='service_id')
    
    daytime_freq <- daytime_freq %>%
      mutate(monday   =ifelse(monday   ==0,NA,headway*monday),
             tuesday  =ifelse(tuesday  ==0,NA,headway*tuesday),
             wednesday=ifelse(wednesday==0,NA,headway*wednesday),
             thursday =ifelse(thursday ==0,NA,headway*thursday),
             friday   =ifelse(friday   ==0,NA,headway*friday)) %>% 
      group_by(stop_id) %>% 
      summarise(monday=min(monday,na.rm=TRUE),
                tuesday=min(tuesday,na.rm=TRUE),
                wednesday=min(wednesday,na.rm=TRUE),
                thursday=min(thursday,na.rm=TRUE),
                friday=min(friday,na.rm=TRUE))
    daytime_freq <- daytime_freq %>% 
      mutate(freq = rowMeans(.[,grep("monday", colnames(daytime_freq)):grep("friday", colnames(daytime_freq))])) %>%
      select(stop_id,freq)
    
    
    # create spatial features for all stops
    stops_sf <- stops_as_sf(gtfs$stops %>% filter(stop_id %in% (daytime_freq %>% pull(stop_id)))) %>%
      mutate(state=currentState,
             authority=authority,
             publication_date=publicationDate,
             mode=transitMode)
    # identify frequent stops
    all_stops_current <- stops_sf %>% right_join(daytime_freq, by="stop_id") 
    frequent_stops_current <- all_stops_current %>% filter(freq <= 30)
    all_stops <- rbind(all_stops, all_stops_current)
    frequent_stops <- rbind(frequent_stops, frequent_stops_current)
    print(paste0(currentState," ",transitMode," complete in ", (Sys.time()-startTime), " seconds"))
  }  
}



# To restrict to valid days





# all_stops2 <- all_stops %>%
#   as.data.frame() %>%
#   st_sf()

st_write(all_stops, dsn=gpkg_out, layer="all_stops",  layer_options = "OVERWRITE=YES" )
st_write(frequent_stops, dsn=gpkg_out, layer="frequent_stops",  layer_options = "OVERWRITE=YES" )


# summarise(avg_frequency = sum(tmp)/sum(wgt))

# Also note that the choice to select the minimum headway of the services is naive,
# More appropriate is
# 1. filter out weekend service IDs
# 2. Multiply calendar matrix by 