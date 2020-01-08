# Carl Higgs 20200106
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
library('tidyverse')
library('hablar')
library('sf')
# library('ggplot2')

gtfs_feeds <- c(
  'D:/ntnl_li_2018_template/data/destinations/gtfs_au_ntnl_20191008_20191205/custom_gtfs_vic_ptv/gtfs_au_vic_ptv_20191004.zip'
)

gpkg_out <- 'D:/ntnl_li_2018_template/data/destinations/gtfs_au_ntnl_20191008_20191205/2020-01-07_tidy_transit_headway.gpkg'


for (feed in gtfs_feeds) {
  gtfs <- read_gtfs(feed)
  gtfs <- gtfs %>% set_hms_times()  %>% set_date_service_table()
  # date_stub = paste0(gtfs$feed_info$feed_start_date,'_',gtfs$feed_info$feed_end_date)
  stub = strsplit(basename(feed),".",fixed=TRUE)[[1]][1]
  # note that default is for weekday, and we don't group by route;
  # however this also doesn't disaggregate by mode
  # stop_id to identify modes
  #  >> get stop_id from stop_times joined to trips (trip_id) joined to routes (route_id) to get route_mode
  #  >> disaggregate frequent stops by stop_id group lists using route_type, according to modes
  
  daytime_freq <- get_stop_frequency(gtfs, start_hour = 7, end_hour = 19,by_route = FALSE)
  # Note that date filtering is not currently working
  # services_in_range <- gtfs$.$date_service_table  %>% 
  #   filter(date >= "2019-10-08" & date <= "2019-12-05" )   %>% 
  #   select(service_id)
  # daytime_freq = daytime_freq %>% semi_join(services_in_range,by="service_id")
  print(summary(daytime_freq$headway))
  # create spatial features for all stops
  stops_sf <- stops_as_sf(gtfs$stops)
  # identify frequent stops
  daytime_frequent_stops_sf <- stops_sf %>% right_join(daytime_freq, by="stop_id") 
  # find minimum service headway for each stop
  unique_stop_headway = aggregate(headway ~ stop_id, data = daytime_frequent_stops_sf, min,na.rm=TRUE) %>% 
    left_join(stops_sf[,c('stop_id','geometry')], by="stop_id")
  frequent_stops <- unique_stop_headway %>% filter(headway <= 30)
  st_write(unique_stop_headway, dsn=gpkg_out, layer=paste0('all_',stub), layer_options = "OVERWRITE=YES" )
  st_write(frequent_stops, dsn=gpkg_out, layer=paste0('frequent_',stub), layer_options = "OVERWRITE=YES" )
}

# Then for interactive debugging, some example code bits to hone down on a particular tram stop on Sydney Rd
# Checking out Albion St / Sydney Rd stop (trams missing in melbourne wtf)
gtfs$stop_times%>%filter(stop_id==17320)%>%select(trip_id)
gtfs$trips%>%filter(trip_id=='2680.T0.3-19-mjp-1.1.H')
service_id ='3-19-mjp-1.1.H'

# this is where I was at when we stepped out for meeting