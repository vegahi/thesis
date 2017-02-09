require(etl)
require(citibike)
library(lubridate)
library(leaflet)
library(ggplot2)
library(rgdal)

bikes <- etl("citibike", dir = "~/Desktop/citibike_data")
system.time(bikes %>%
              etl_extract() %>%
              etl_transform() %>%
              etl_load())
trips<- tbl(bikes,"trips")
data <- collect(trips, n = Inf)

# format cleaning
num_columns <- c(1,4,6,7,8,10,11,12,14,15)
time_columns <- c(2,3)
string_columns <- c(5,9,13) # unused
for (i in num_columns) {
  data[,i] <-lapply(data[,i, drop=FALSE], function(x) 
    as.numeric(gsub('"', " ", x))) 
}
for (i in time_columns) {
  data[,i] <-lapply(data[,i, drop=FALSE], function(x) 
    ymd_hms(gsub('"', " ", x))) 
}

# take sample of 2013-07-01 at 9 am 
sample <- data %>%
  mutate(startDate = as_date(starttime)) %>%
  mutate(startHour = hour(starttime)) %>%
  filter(startDate == ("2013-07-01")) %>%
  filter(startHour == 9)


plot_trip <- function(sample){
  master_stations <- bind_rows(
    sample %>%
      select(start.station.id, start.station.name, start.station.longitude, start.station.latitude) %>%
      rename(id = start.station.id, name = start.station.name, 
             longitude = start.station.longitude, latitude = start.station.latitude),
    sample %>%
      select(end.station.id, end.station.name, end.station.longitude, end.station.latitude) %>%
      rename(id = end.station.id, name = end.station.name,
             longitude = end.station.longitude, latitude = end.station.latitude)) %>%
    distinct(id, name, longitude, latitude)
  
  trip_data <- sample %>%
    group_by(start.station.id, end.station.id) %>%
    summarise(trips = n()) 
  
  station_count <- bind_rows(
    trip_data %>%
      select(start.station.id, trips) %>%
      rename(id = start.station.id),
    trip_data %>%
      select(-start.station.id) %>%
      rename(id = end.station.id)) %>%
    select(-start.station.id) %>%
    group_by(id) %>%
    summarise(total_trips = sum(trips))
  
  stations_plot <- left_join(station_count, master_stations, by = "id")
  
  
  # create the lines data set with start and end stations info
  lines <- bind_rows(
    sample %>%
      select(start.station.id, end.station.id, start.station.longitude, start.station.latitude) %>%
      rename(lon = start.station.longitude, lat = start.station.latitude),
    sample %>%
      select(start.station.id, end.station.id, end.station.longitude, end.station.latitude) %>%
      rename(lon = end.station.longitude, lat = end.station.latitude)) %>%
    arrange(start.station.id, end.station.id) %>%
    na.omit() %>%
    group_by(start.station.id, end.station.id) %>%
    do(line = Line(as.data.frame(select(., lon, lat))))
  
  # create line
  make_line <- function(x){
    Lines(list(x$line), ID = paste0(x$start.station.id, "-", x$end.station.id))
  }
  
  # add trip info to the lines data
  lines <- sample %>%
    group_by(start.station.id, end.station.id) %>%
    summarise(total_trip = n()) %>%
    right_join(., lines, by = c("start.station.id" = "start.station.id", "end.station.id" = "end.station.id"))
  
  # convert into SpatialLines and specify the projection to use
  lines_list <- apply(lines, MARGIN = 1, make_line)
  segments_sp <- SpatialLines(lines_list, proj4string = CRS("+init=epsg:4326"))
  
  # rename the rownames of lines
  for (i in (1:length(segments_sp))) {
    lines$link[i] <- segments_sp@lines[[i]]@ID
  }
  lines <- select(lines, -line)
  rownames(lines) <- lines$link
  lines <- as.data.frame(lines)
  
  # create a spatial lines data frame
  splndf <- SpatialLinesDataFrame(sl= segments_sp, data = lines, match.ID = TRUE)
  mylines_ll <- sp::spTransform(segments_sp, CRS("+init=epsg:4326"))
  
  # create the palette
  pal <- colorNumeric( palette = "Blues", domain = splndf$total_trip)
  weight
  
  # draw segment graph with leaflet
  # get rid of one-trip route
  leaflet() %>%
    addTiles() %>%
    #addProviderTiles("Esri.WorldImagery") %>%
    addPolylines(data = splndf,color = ~ pal(total_trip), weight = ~((total_trip-1) *10), noClip = TRUE,  fillOpacity = 100, group = "Trips")%>%
    addCircles(data = stations_plot, lng = ~longitude, lat = ~latitude, weight = 1, radius = ~ total_trips * 5 , popup = ~name, color = "black", group = "Stations") %>%
    setView(-73.9,40.7, zoom = 11) %>%
    addLayersControl(overlayGroups = c("Stations", "Trips"))
}

plot_trip(sample)

