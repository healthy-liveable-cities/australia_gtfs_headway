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


# Modifying tidy transit functions from
# https://github.com/r-transit/tidytransit/blob/master/R/frequencies.R

# Unchanged
#' Filter a gtfs calendar dataframe to service ids for specific days of the week.
#' 
#' @param gtfs_object object made by join_all_gtfs_tables
#' @param dow default to "weekday" (1,1,1,1,1,0,0)
#' @return service ids that match the schedule specified
#' @keywords internal
service_by_dow <- function(calendar,
                           dow=c(1,1,1,1,1,0,0)){
  calendar <- subset(calendar, 
                     calendar$monday == dow[1] & 
                       calendar$tuesday == dow[2] & 
                       calendar$wednesday == dow[3] & 
                       calendar$thursday == dow[4] & 
                       calendar$friday == dow[5] &
                       calendar$saturday == dow[6] &
                       calendar$sunday == dow[7])
  return(calendar$service_id)
}

# Unchanged
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

# Modified
get_hlc_stop_frequency <- function(gtfs_obj,
                                   start_hour=7,
                                   end_hour=19,
                                   start_date = '2019-10-08',
                                   end_date = '2019-12-05',
                                   dow =  c('Monday','Tuesday','Wednesday','Thursday','Friday'),
                                   service_ids=c()
) {
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
  # here, we take out the by route logic, and instead of grouping by service ID, group by service date
  stop_time_trips <- stop_time_trips %>%
    dplyr::group_by(.data$direction_id,
                    .data$stop_id,
                    .data$date) %>%
    dplyr::summarise(departures = dplyr::n())
  # TODO we should only use seconds
  # or hms objects to avoid confusion
  t1 <- end_hour - start_hour
  minutes1 <- 60 * t1
  stop_time_trips$headway <-
    as.integer(round(minutes1 / stop_time_trips$departures,
                     digits = 0))
  # Now, for HLC method, for each stop we average headway over dates 
  # We take the best (smallest) headway out of the two possible of the stop
  # this is because many stops have frequent service in one direction 
  # and infrequent in the other (ie. inbound vs outbound differences)
  stop_time_trips <- stop_time_trips %>% 
    group_by(stop_id,direction_id) %>% 
    summarise(headway=mean(headway)) %>%
    group_by(stop_id) %>% 
    summarise(headway=min(headway))
  stops_headway <- stop_time_trips %>% 
    tibble::as_tibble()
  return(stops_headway)
}

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
    filter(date >= "2019-10-08" & date <= "2019-12-05" )
  
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
    
    
    daytime_freq <- get_hlc_stop_frequency(gtfs,
                    start_hour=7,
                    end_hour=19,
                    start_date = '2019-10-08',
                    end_date = '2019-12-05',
                    dow =  c('Monday','Tuesday','Wednesday','Thursday','Friday'),
                    service_ids=mode_services2
    )
    # skip to next mode if there are no records for this mode (e.g. no ferries in Tasmania)
    if (nrow(daytime_freq)==0) next
    
    # create spatial features for all stops
    stops_sf <- stops_as_sf(gtfs$stops %>% filter(stop_id %in% (daytime_freq %>% pull(stop_id)))) %>%
      mutate(state=currentState,
             authority=authority,
             publication_date=publicationDate,
             mode=transitMode)
    # identify frequent stops
    all_stops_current <- stops_sf %>% 
      right_join(daytime_freq, by="stop_id") %>% 
      select(c(stop_id,mode,state,authority,publication_date,headway,geometry))
    frequent_stops_current <- all_stops_current %>% filter(headway <= 30)
    all_stops <- rbind(all_stops, all_stops_current)
    frequent_stops <- rbind(frequent_stops, frequent_stops_current)
    print(paste0(currentState," (",authority,") ",transitMode," complete in ", (Sys.time()-startTime), " seconds"))
  }  
}

st_write(all_stops, dsn=gpkg_out, layer="all_stops",  layer_options = "OVERWRITE=YES" )
st_write(frequent_stops, dsn=gpkg_out, layer="frequent_stops",  layer_options = "OVERWRITE=YES" )

