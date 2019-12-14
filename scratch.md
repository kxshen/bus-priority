11.S953: Bus Priority Measures
================

``` r
#Libraries...?
library(tidytransit)
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.2.1     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ----------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(leaflet)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(sf)
```

    ## Linking to GEOS 3.6.1, GDAL 2.2.3, PROJ 4.9.3

``` r
library(geojsonsf)
```

Import data
===========

First, start with one route, and compare average travel times before and after implementation

Use GTFS to get (lat,long) for stop id's, and other things for the real map.

``` r
#GTFS
mbta_gtfs = read_gtfs('data/MBTA_GTFS_Jan_2019.zip')#runtime is long on importing GTFS feed
```

    ## Warning in gtfs_validate(gtfs_obj, quiet = quiet): Invalid feed. Missing
    ## required field(s): is_bidirectional

Visualize the map
=================

``` r
dat = read.csv(file = 'data/Broadway/R101_in.csv')

#Find coordinates of important stops
stopnames = dat %>% select(stopid) %>% unique()
stops_cut = filter(mbta_gtfs$stops, as.numeric(stop_code) %in% stopnames$stopid) %>% arrange(stop_lon)

#recreate the methodology from class, but only display the better bus lanes and the corresponding stops
geo1 = st_multipoint(cbind(stops_cut$stop_lon, stops_cut$stop_lat)) %>% st_sfc %>% st_sf# stop points
selected_shape_ids = mbta_gtfs$trips %>% 
  filter(as.numeric(route_id) %in% 101) %>% 
  select(shape_id) %>% 
  unique()
```

    ## Warning in as.numeric(route_id) %in% 101: NAs introduced by coercion

``` r
selected_shapes = mbta_gtfs$shapes %>%
  filter(shape_id %in% selected_shape_ids$shape_id) %>% 
  arrange(shape_id, shape_pt_sequence)

ls = vector("list", nrow(selected_shape_ids))
for(i in 1:nrow(selected_shape_ids)){
  ls[[i]] = selected_shapes %>% 
    filter(shape_id == selected_shape_ids$shape_id[i]) %>% 
    select(shape_pt_lon, shape_pt_lat) %>% 
    as.matrix()
}
geo2 = st_multilinestring(ls) %>% st_sfc %>% st_sf # have to convert to geometry type, then feature collection for aggregation?

geojson = rbind(geo1, geo2) %>% sf_geojson #combine features from different feature collections, finally!

#abort mission, this part doesn't work as I intended
# leaflet() %>% 
#   addProviderTiles("CartoDB.Positron") %>% 
#   addGeoJSON(geojson)

m = leaflet(stops_cut) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolylines(lng = ~stop_lon, lat = ~stop_lat)  %>% #trip goes sotuhbound, remember that
  addCircleMarkers(lng = ~stop_lon, lat = ~stop_lat,
    radius = 6,
    color = "red",
    stroke = FALSE, fillOpacity = 0.5,
    popup = ~paste0("Stop: ", stop_id)
  )
m
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-dd31a7833a937a7de0c0">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["CartoDB.Positron",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addPolylines","args":[[[[{"lng":[-71.097724,-71.093553,-71.087677],"lat":[42.395076,42.392556,42.389316]}]]],null,null,{"interactive":true,"className":"","stroke":true,"color":"#03F","weight":5,"opacity":0.5,"fill":false,"fillColor":"#03F","fillOpacity":0.2,"smoothFactor":1,"noClip":false},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addCircleMarkers","args":[[42.395076,42.392556,42.389316],[-71.097724,-71.093553,-71.087677],6,null,null,{"interactive":true,"className":"","stroke":false,"color":"red","weight":5,"opacity":0.5,"fill":true,"fillColor":"red","fillOpacity":0.5},null,null,["Stop: 5303","Stop: 2705","Stop: 2709"],null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[42.389316,42.395076],"lng":[-71.097724,-71.087677]}},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
Broadway inwards
================

``` r
#Convert to datetime object
dat$actstoptime = dat$actstoptime %>% 
  as.POSIXct()

#Classify into AM peak (1), midday (2), and PM peak (3)
timeclasses = matrix(
  c("07:30:00", "09:30:00",
  "12:00:00", "14:00:00", #gtfs uses 24 hr clock
  "16:30:00", "18:30:00"),
  nrow = 3, ncol = 2,
  byrow = TRUE
) %>% 
  as.POSIXct(format = "%H:%M:%S") # creates POSIX object with today's date (just make sure it runs on the same day as the other)

dat = dat %>% 
  mutate(time = as.POSIXct(format(actstoptime,"%H%:%M:%S"), format = "%H:%M:%S")) %>% #first, extract time out of the datetime
  mutate(peak = with(.,case_when( #case_when() is better than nested ifelse statements
  (time > timeclasses[1] & time < timeclasses[4]) ~ 1,
  (time > timeclasses[2] & time < timeclasses[5]) ~ 2,
  (time > timeclasses[3] & time < timeclasses[6]) ~ 3,
  TRUE ~ 0))) #all other trips are not analyzed

triptimes = dat %>% 
  group_by(trip) %>% 
  filter(any(stopid %in% 2709) & any(stopid %in% 5303)) %>% # look at length of interest: the full corridor
  filter(any(peak > 0)) %>% 
  summarise(travel_time = max(actstoptime) - min(actstoptime),#calculate stoptime using the first and last stops
            seasonal_period = unique(seasonal_period), 
            implemented = unique(implemented),
            peak = max(peak) )

#results:
triptimes %>% 
  group_by(seasonal_period, peak) %>% 
  summarise(
            mean_imp = median(travel_time[implemented == 1]), 
            sd_imp = sd(travel_time[implemented == 1]),
            mean_noimp = median(travel_time[implemented == 0]),
            sd_noimp = sd(travel_time[implemented == 0])
            )
```

    ## # A tibble: 9 x 6
    ## # Groups:   seasonal_period [3]
    ##   seasonal_period  peak mean_imp      sd_imp mean_noimp    sd_noimp
    ##   <fct>           <dbl> <drtn>         <dbl> <drtn>           <dbl>
    ## 1 1                   1       NA mins  NA    6.500000 mins     1.57
    ## 2 1                   2       NA mins  NA    6.366667 mins     1.36
    ## 3 1                   3       NA mins  NA    5.716667 mins     1.12
    ## 4 2                   1 6.458333 mins   1.49 7.316667 mins     2.29
    ## 5 2                   2 5.075000 mins   1.81 5.991667 mins     1.35
    ## 6 2                   3 5.333333 mins   1.22 5.950000 mins     1.21
    ## 7 NULL                1       NA mins  NA    7.275000 mins     2.56
    ## 8 NULL                2       NA mins  NA    5.916667 mins     1.65
    ## 9 NULL                3       NA mins  NA    5.808333 mins     1.47

``` r
# I assume that peaks across days are comparable. Could be tested I guess. 
```

``` r
dat = read.csv(file = 'data/Broadway/R101_out.csv')
endstops = c(2722, 2729)

#Find coordinates of important stops
stopnames = dat %>% select(stopid) %>% unique()
stops_cut = filter(mbta_gtfs$stops, as.numeric(stop_code) %in% stopnames$stopid)

#map out the stops
m = leaflet(stops_cut) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolylines(lng = ~stop_lon, lat = ~stop_lat)  %>% #trip goes sotuhbound, remember that
  addCircleMarkers(lng = ~stop_lon, lat = ~stop_lat,
    radius = 6,
    color = "red",
    stroke = FALSE, fillOpacity = 0.5
  )
m
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-1e780089e594346e8234">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["CartoDB.Positron",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addPolylines","args":[[[[{"lng":[-71.08728,-71.092975,-71.097355],"lat":[42.389405,42.392521,42.394765]}]]],null,null,{"interactive":true,"className":"","stroke":true,"color":"#03F","weight":5,"opacity":0.5,"fill":false,"fillColor":"#03F","fillOpacity":0.2,"smoothFactor":1,"noClip":false},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addCircleMarkers","args":[[42.389405,42.392521,42.394765],[-71.08728,-71.092975,-71.097355],6,null,null,{"interactive":true,"className":"","stroke":false,"color":"red","weight":5,"opacity":0.5,"fill":true,"fillColor":"red","fillOpacity":0.5},null,null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[42.389405,42.394765],"lng":[-71.097355,-71.08728]}},"evals":[],"jsHooks":[]}</script>
<!--/html_preserve-->
``` r
#Convert to datetime object
dat$actstoptime = dat$actstoptime %>% 
  as.POSIXct()

dat = dat %>% 
  mutate(time = as.POSIXct(format(actstoptime,"%H%:%M:%S"), format = "%H:%M:%S")) %>% #first, extract time out of the datetime
  mutate(peak = with(.,case_when( #case_when() is better than nested ifelse statements
  (time > timeclasses[1] & time < timeclasses[4]) ~ 1,
  (time > timeclasses[2] & time < timeclasses[5]) ~ 2,
  (time > timeclasses[3] & time < timeclasses[6]) ~ 3,
  TRUE ~ 0))) #all other trips are not analyzed

triptimes = dat %>% 
  group_by(trip) %>% 
  filter(any(stopid %in% endstops[1]) & any(stopid %in% endstops[2])) %>% # look at length of interest: the full corridor
  filter(any(peak > 0)) %>%
  summarise(travel_time = max(actstoptime) - min(actstoptime),#calculate stoptime using the first and last stops
            seasonal_period = unique(seasonal_period), 
            implemented = unique(implemented),
            peak = max(peak) )

#results:
triptimes %>% 
  group_by(seasonal_period, peak) %>% 
  summarise(
            mean_imp = median(travel_time[implemented == 1]), 
            sd_imp = sd(travel_time[implemented == 1]),
            mean_noimp = median(travel_time[implemented == 0]),
            sd_noimp = sd(travel_time[implemented == 0])
            )
```

    ## # A tibble: 9 x 6
    ## # Groups:   seasonal_period [3]
    ##   seasonal_period  peak mean_imp      sd_imp mean_noimp    sd_noimp
    ##   <fct>           <dbl> <drtn>         <dbl> <drtn>           <dbl>
    ## 1 1                   1       NA mins NA     4.433333 mins    1.66 
    ## 2 1                   2       NA mins NA     4.600000 mins    0.934
    ## 3 1                   3       NA mins NA     5.533333 mins    1.12 
    ## 4 2                   1 3.858333 mins  2.37  4.533333 mins    1.86 
    ## 5 2                   2 4.216667 mins  1.12  4.883333 mins    1.10 
    ## 6 2                   3 4.733333 mins  0.907 5.816667 mins    1.77 
    ## 7 NULL                1       NA mins NA     4.375000 mins    2.76 
    ## 8 NULL                2       NA mins NA     4.783333 mins    1.10 
    ## 9 NULL                3       NA mins NA     5.583333 mins    2.08

``` r
#
```
