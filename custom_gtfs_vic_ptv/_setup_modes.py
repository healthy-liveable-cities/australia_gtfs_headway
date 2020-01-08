# define modes for GTFS feed(s) as per agency_id codes in agency.txt below
# Note that the Victorian PTV agency codes metro and regional trains as type 2 (intercity and long distance), not 1 (metro / subway)
modes = {
         'tram' :{'route_types':[0], 'custom_mode':"routes.agency_id IN ('3')",       'start_times':['07:00:00'],'end_times':['19:00:00'],'intervals':['00:15:00','00:30:00','00:45:00']},
         'train':{'route_types':[1,2], 'custom_mode':"routes.agency_id IN ('1','2')", 'start_times':['07:00:00'],'end_times':['19:00:00'],'intervals':['00:15:00','00:30:00','00:45:00']},
         'bus'  :{'route_types':[3], 'custom_mode':"routes.agency_id IN ('4','6')",   'start_times':['07:00:00'],'end_times':['19:00:00'],'intervals':['00:15:00','00:30:00','00:45:00']}
         }

# define month and day for "representative period" ie. not in school time; here example is July 15 to August 15
# Dates for 2019 analysis copied as per Jonathan Arundel's dates for 2018 analysis
# ie. semester 4
start_date_mmdd = '1008'
end_date_mmdd = '1205'