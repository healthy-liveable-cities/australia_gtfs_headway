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
library('hablar')
library('sf')

gtfs_feeds <- list.files(recursive=TRUE, pattern="*.zip")
gpkg_out <- paste0(Sys.Date(),'_tidy_transit_headway.gpkg')

all_stops <- NULL
frequent_stops <- NULL
# feed = gtfs_feeds[2]
for (feed in gtfs_feeds) {
  # feed=gtfs_feeds[2]
  gtfs <- read_gtfs(feed)
  gtfs <- gtfs %>% set_hms_times()  %>% set_date_service_table()
  # date_stub = paste0(gtfs$feed_info$feed_start_date,'_',gtfs$feed_info$feed_end_date)
  stub = strsplit(basename(feed),".",fixed=TRUE)[[1]][1]
  fileInfo = strsplit(stub,"_")[[1]]
  currentState = fileInfo[3]
  # note that 'authority' does not uniquely identify feed, 
  # e.g. translink in QLD has additional argument specifiers,
  # these potential additional identifiers are retrieved and 
  # concatenated for the record using the below code
  authority = paste(fileInfo[4:(length(fileInfo)-1)],collapse="_")
  publicationDate = as.numeric(fileInfo[length(fileInfo)])

  valid_days <- c('Monday','Tuesday','Wednesday','Thursday','Friday')
  valid_services <- gtfs$.$date_service_table %>% 
    mutate(dow = weekdays(date)) %>% 
    filter(dow %in% valid_days) %>%
    filter(date >= "2019-08-10" & date <= "2019-12-05" )
  
  valid_service_ids <- valid_services  %>% pull(service_id)
  # valid_service_dates <- valid_services  %>% pull(date)
  
  # Mode table storing differences in GTFS feeds
  modes <- tribble(
    ~state  , ~mode  , ~agencyId, ~routeTypes  ,
    "vic"  , "tram" ,  c(3)    , c(0)          ,
    "vic"  , "train",  c(1,2)  , c(1,2)        ,
    "vic"  , "bus"  ,  c(4,6)  , c(3)          ,
    "nsw"  , "tram" ,  NA      , c(0)          ,
    "nsw"  , "train",  NA      , c(2,401)      ,
    "nsw"  , "bus"  ,  NA      , c(700,712,714),
    "nsw"  , "ferry",  NA      , c(4)          ,
    "tas"  , "bus"  ,  NA      , c(3)          ,
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
    filter(service_id%in%valid_service_ids) 
  
  
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
             mutate(agency_id=as.numeric(agency_id)) %>%
             filter(agency_id %in% (modesCurrent%>%filter(mode==transitMode)%>%pull(agencyId))[[1]] &
                      route_type %in% (modesCurrent%>%filter(mode==transitMode)%>%pull(routeTypes))[[1]] ) %>%
             pull(service_id) %>%
             unique()
    )
    
    
    daytime_freq <- get_stop_frequency(gtfs, start_hour = 7, end_hour = 19,service_ids = mode_services2,by_route = FALSE)
    
    # skip to next mode if there are no records for this mode (e.g. no ferries in Tasmania)
    if (nrow(daytime_freq)==0) next
    # weight service headways for each stop by the number of days on which they are valid for that stop
    daytime_freq <- daytime_freq %>% 
      left_join(valid_services %>% count(service_id),by='service_id') %>%
      group_by(stop_id) %>% 
      summarise(frequency=weighted.mean(headway,n))
    
    
    # create spatial features for all stops
    stops_sf <- stops_as_sf(gtfs$stops %>% filter(stop_id %in% (daytime_freq %>% pull(stop_id)))) %>%
      mutate(state=currentState,
             authority=authority,
             publication_date=publicationDate,
             mode=transitMode)
    # identify frequent stops
    all_stops_current <- stops_sf %>% 
      right_join(daytime_freq, by="stop_id") %>% 
      select(c(stop_id,mode,state,authority,publication_date,frequency,geometry))
    frequent_stops_current <- all_stops_current %>% filter(frequency <= 30)
    all_stops <- rbind(all_stops, all_stops_current)
    frequent_stops <- rbind(frequent_stops, frequent_stops_current)
    print(paste0(currentState," (",authority,") ",transitMode," complete in ", (Sys.time()-startTime), " seconds"))
  }  
}

st_write(all_stops, dsn=gpkg_out, layer="all_stops",  layer_options = "OVERWRITE=YES" )
st_write(frequent_stops, dsn=gpkg_out, layer="frequent_stops",  layer_options = "OVERWRITE=YES" )
