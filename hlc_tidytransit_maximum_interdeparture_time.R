# Carl Higgs, Alan Both 20200106
#  Following http://tidytransit.r-transit.org/articles/servicepatterns.html
# using
# http://tidytransit.r-transit.org/articles/timetable.html
# http://tidytransit.r-transit.org/articles/servicepatterns.html
# https://cran.r-project.org/web/packages/tidytransit/vignettes/frequency.html

library('tidytransit')
library('dplyr')
library('hablar')
library('sf')

for (gtfs_year in c('2018','2019')) {
  prefix_dir = paste0('./',gtfs_year,'/')
  gtfs_feeds <- list.files(path=prefix_dir,recursive=TRUE, pattern="*.zip")
  gpkg_out <- paste0(Sys.Date(),paste0('_tidy_transit_maximum_interdeparture_',gtfs_year,'.gpkg'))
  
  # note the application of the same date to all feeds assumes these all have coverage
  # this may not be the case, and should perhaps be set as a configurable parameter
  # for each feed
  analysis_start = paste0(gtfs_year,'-10-08')
  analysis_end   = paste0(gtfs_year,'-12-05')
  
  # GTFS mode configuration table
  modes <- tribble(
    ~state  , ~mode  , ~agencyId, ~routeTypes  ,
    "vic"  , "tram" ,  c(3)    , c(0)          ,
    "vic"  , "train",  c(1,2)  , c(1,2)        ,
    "vic"  , "bus"  ,  c(4,6)  , c(3)          ,
    "nsw"  , "tram" ,  NA      , c(0)          ,
    "nsw"  , "train",  NA      , c(2,401)      ,
    "nsw"  , "bus"  ,  NA      , c(700,712,714),
    "nsw"  , "ferry",  NA      , c(4)          ,
    "other", "tram" ,  NA      , c(0)          ,
    "other", "train",  NA      , c(1,2)        ,
    "other", "bus"  ,  NA      , c(3)          ,
    "other", "ferry",  NA      , c(4)
  )
  
  
  # Using tidy transit function from
  
  # Unchanged from https://github.com/r-transit/tidytransit/blob/master/R/time.R
  #' Filter stop times by hour of the day
  #' 
  #' @param stop_times a gtfs_obj$stop_times dataframe with arrival_time and departure_time 
  #'                   created by [set_hms_times()].
  #' @return dataframe with only stop times within the hours specified, with time columns as lubridate periods
  #' @keywords internal
  filter_stop_times_by_hour <- function(stop_times, 
                                        start_hour, 
                                        end_hour) {
    # TODO use set_hms_times during import to avoid errors here?
    stopifnot("arrival_time_hms" %in% colnames(stop_times), "departure_time_hms" %in% colnames(stop_times))
    # it might be easier to just accept hms() objects
    stop_times %>% filter(arrival_time_hms > 
                            hms::hms(hours = start_hour) & 
                            departure_time_hms < hms::hms(hours = end_hour))
  }
  
  regular_stops <- NULL
  # # feed = gtfs_feeds[2]
  for (feed in gtfs_feeds) {
    feed=gtfs_feeds[1]
    feed <- paste0(prefix_dir,'/',feed)
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
      filter(date >= analysis_start & date <= analysis_end )
    
    valid_service_ids <- valid_services  %>% pull(service_id)
    valid_dates <- valid_services  %>% select(date) %>% unique()
    
    modesCurrent <- modes %>%
      filter(state==currentState)
    if(NROW(modesCurrent)==0) { # if there are no rows, then we need to use 'other' as the state
      modesCurrent <- modes %>%
        filter(state=="other")
    }
    
    mode_services <- gtfs$trips %>%
      left_join(gtfs$routes, by='route_id') %>% 
      filter(service_id%in%valid_service_ids) 
    
    
    # # some feeds need filtering on just route type, some need filtering on route type and agency id (e.g., Vic)
    # for (transitMode in modesCurrent$mode) {
    transitMode <- 'bus'
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
    gtfs_obj <- gtfs
    start_hour <- 7
    end_hour <- 19
    start_date <- analysis_start
    end_date <- analysis_end
    dow <-  c('Monday','Tuesday','Wednesday','Thursday','Friday')
    service_ids <- mode_services2
    
    gtfs_obj <- set_hms_times(gtfs_obj)
    trips <- gtfs_obj$trips
    stop_times <- gtfs_obj$stop_times
    stop_times <- filter_stop_times_by_hour(stop_times,
                                           start_hour,
                                           end_hour)
    # custom HLC code to allow for consideration of date range
    gtfs_days <- dow
    gtfs_services <- gtfs$.$date_service_table %>% 
     mutate(dow = weekdays(date)) %>% 
     filter(dow %in% gtfs_days) %>%
     filter(date >= start_date & date <= end_date )  %>% 
     filter(service_id %in% service_ids)
    
    trips <- gtfs_services %>% left_join(gtfs$trips,by="service_id")
    trips <- trips %>%
     dplyr::filter(.data$service_id %in% service_ids) %>%
     count_service_trips()
    stop_time_trips <- dplyr::inner_join(stop_times,
                                        trips,
                                        by = "trip_id")
    stop_time_dates <- stop_time_trips %>% 
        select(date,stop_id,direction_id,trip_id,departure_time)   %>%
        arrange(date,stop_id,departure_time)  %>%
        mutate(numeric_time = as.POSIXct(paste0(date," ",departure_time),format="%Y-%m-%d %H:%M:%S"))
    
    # on each date, for each stop in each direction
    # find the gap between departures in minutes
    # for each departure between 7am and 7pm
    stop_time_intervals <- stop_time_dates %>%
      group_by(date,stop_id,direction_id) %>%
      mutate(
             gap = as.numeric(numeric_time - lag(numeric_time, default = first(numeric_time)))/60
             )
        
    # for each date, stop and direction, find the maximum departure gap
    stop_date_max_interval <- stop_time_intervals %>%
      group_by(stop_id,date,direction_id) %>%
      summarise(max_gap=max(gap),
                before_0730_after_1830 = (min(departure_time) < '07:30') & (max(departure_time) > '18:30'))
    
    
    stops_percent_days_no_more_than_30_max_gap <- 
      stop_date_max_interval %>%
      group_by(stop_id,direction_id)%>%
      summarise(
                percent_no_more_than_30_max = mean((max_gap<=30)&(before_0730_after_1830==TRUE))
                )
    
    # take the largest value of percent days with no more than 30
    # across directions for each stop
    # --- gives benefit of the doubt; and this is what counts spatially
    usually_no_more_than_30_min_wait <- stops_percent_days_no_more_than_30_max_gap %>%
      group_by(stop_id) %>%
      summarise(pct_no_more_than_30_min_wait = max(percent_no_more_than_30_max)) %>%
      # we filter down to those stops with no more than 30 min wait across 90% of dates
      filter(pct_no_more_than_30_min_wait > 0.9)
    
    # create spatial features for all stops
    stops_sf <- stops_as_sf(gtfs$stops %>% filter(stop_id %in% (usually_no_more_than_30_min_wait %>% pull(stop_id)))) %>%
      mutate(state=currentState,
             authority=authority,
             publication_date=publicationDate,
             mode=transitMode)
    # identify frequent stops
    regular_stops <- stops_sf %>% 
      right_join(usually_no_more_than_30_min_wait, by="stop_id") %>% 
      select(c(stop_id,mode,state,authority,publication_date,pct_no_more_than_30_min_wait,geometry))
    regular_stops <- rbind(regular_stops, regular_stops_current)
    print(paste0(currentState," (",authority,") ",transitMode," complete in ", (Sys.time()-startTime), " seconds"))
  }
}
  
st_write(regular_stops, dsn=gpkg_out, layer="usually no more than 30 min gap",  layer_options = "OVERWRITE=YES" )





