library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(leaflet)
library(rgdal)
library(shiny)
server <- function(input, output) {
  # database
  bikes <- src_mysql(host = "scidb.smith.edu",dbname = "citibike", user = "wzhang", password = "PedalP0wer")
  output$total_trip <- renderPrint({
    bikes %>%
      tbl("trips_sub") %>%
      mutate(year = YEAR(Start_Time), month = MONTH(Start_Time), 
             day = DAY(Start_Time), hour = HOUR(Start_Time)) %>%
      filter(year == year(input$dates) & month == month(input$dates) & 
               day == day(input$dates) & hour == (input$hours)) %>%
      summarise(total_trip = n())
    
  })
  
  output$trips_vol <- renderTable({
    bikes %>%
      tbl("trips_sub") %>%
      mutate(year = YEAR(Start_Time), month = MONTH(Start_Time), 
             day = DAY(Start_Time), hour = HOUR(Start_Time)) %>%
      filter(year == year(input$dates) & month == month(input$dates) & 
               day == day(input$dates) & hour == (input$hours)) %>%
      group_by(Start_Station_ID, End_Station_ID) %>%
      summarise(trip_volume = n()) %>%
      arrange(desc(trip_volume)) %>%
      head(n = 10)
  })
  
  output$trip_length <- renderPrint({
    bikes %>%
      tbl("trips") %>%
      mutate(year = YEAR(Start_Time), month = MONTH(Start_Time), 
             day = DAY(Start_Time), hour = HOUR(Start_Time)) %>%
      select(Trip_Duration, year, month, day, hour) %>%
      filter(year == year(input$dates) & month == month(input$dates) & 
               day == day(input$dates) & hour == (input$hours)) %>%
      select(Trip_Duration) %>%
      summarise(mean = mean(Trip_Duration), sd = sd(Trip_Duration), 
                max = max(Trip_Duration), min = min(Trip_Duration))
    
  })
  
  # create the interactive map
  # sample data 2013-07-01- 9am
  output$map <- renderLeaflet({
    # filter for the date the user entered
    data <- bikes %>%
      tbl("trips_sub") %>%
      mutate(year = YEAR(Start_Time), month = MONTH(Start_Time), 
             day = DAY(Start_Time), hour = HOUR(Start_Time)) %>%
      filter(year == year("2013-07-01") & month == month("2013-07-01") & 
               day == day("2013-07-01") & hour == 9) %>%
      collect(n = Inf)
    master_stations <- bikes %>%
      tbl("master_stations") %>%
      collect(n = Inf)
    plot_trip <- function(sample){
      # get the edges
      edges <- data %>%
        group_by(Start_Station_ID, End_Station_ID) %>%
        summarise(trips = n())
      nodes <- edges %>%
        gather(key = "type", value = "ID", -trips) %>%
        group_by(ID) %>%
        summarise(total_trips = sum(trips)/2) %>%
        left_join(., master_stations, by = "ID" )
      # create line
      make_line <- function(x){
        Lines(list(x$line), ID = paste0(x$Start_Station_ID, 
                                        "-", x$End_Station_ID))
      }
      master_stations_sub <- master_stations %>%
        select(-station_name, -all_names) %>%
        na.omit()
       test <- edges %>%
        right_join(., master_stations_sub, by = c("Start_Station_ID" = "ID")) %>%
        right_join(., master_stations_sub, by = c("End_Station_ID" = "ID")) %>%
        na.omit() 
       lines <- bind_rows(
        test %>%
          select(Start_Station_ID, End_Station_ID, 
                 longitude_avg.x, latitude_avg.x) %>%
          rename(lon = longitude_avg.x, lat = latitude_avg.x),
        test %>%
          select(Start_Station_ID, End_Station_ID, 
                 longitude_avg.y, latitude_avg.y) %>%
          rename(lon = longitude_avg.y, lat = latitude_avg.y)) %>%
          arrange(Start_Station_ID, End_Station_ID) %>%
          na.omit() %>%
          group_by(Start_Station_ID, End_Station_ID) %>%
          do(line = Line(as.data.frame(select(., lon, lat))))
    # add trip info to the lines data
    lines <- test %>%
      select(trips, Start_Station_ID, End_Station_ID) %>%
      right_join(., lines, by = c("Start_Station_ID" = "Start_Station_ID", 
                                  "End_Station_ID" = "End_Station_ID"))
    # convert into SpatialLines and specify the projection to use
      lines_list <- apply(lines, MARGIN = 1, make_line)
      segments_sp <- SpatialLines(lines_list,
                                  proj4string = CRS("+init=epsg:4326"))
    # rename the rownames of lines
      for (i in (1:length(segments_sp))) {
        lines$link[i] <- segments_sp@lines[[i]]@ID
      }
      lines <- select(lines, -line)
      rownames(lines) <- lines$link
      lines <- as.data.frame(lines)
      # create a spatial lines data frame
      splndf <- SpatialLinesDataFrame(sl= segments_sp, 
                                      data = lines, match.ID = TRUE)
      mylines_ll <- sp::spTransform(segments_sp, CRS("+init=epsg:4326"))
      # create the palette
      pal <- leaflet::colorNumeric( palette = "Blues", domain = splndf$trips)
      # draw segment graph with leaflet
      # get rid of one-trip route
      leaflet() %>%
        addTiles() %>%
        #addProviderTiles("Esri.WorldImagery") %>%
        addPolylines(data = splndf,color = ~ pal(trips), 
                     weight = ~((trips-1) *10), noClip = TRUE,  
                     fillOpacity = 100, group = "Trips")%>%
        addCircles(data = nodes, lng = ~longitude_avg,lat = ~latitude_avg, 
                   weight = 1, radius = ~ total_trips * 5, 
                   popup = ~station_name, color = "black", group = "Stations") %>%
        setView(-73.9,40.7, zoom = 11) %>%
        addLayersControl(overlayGroups = c("Stations", "Trips"))
  }
  plot_trip(data)
})
}
ui <- fluidPage(
  # Application title
  titlePanel("CitiBike Data Analysis"),
  
  navbarPage("My Application",
             tabPanel("Summary Statistics",
                      # date input
                      dateInput("dates","Date", value = "2013-07-01", 
                                min ="2013-07-01", max ="2016-12-31", 
                                startview = "year"),
                      # hour input
                      numericInput("hours", "Hour",
                                   value = 9, min = 0, max = 24, step = 1),
                      h4("Number of Trips (Total)"),
                      verbatimTextOutput("total_trip"),
                      
                      h4("Summary Trip Distribution"),
                      tableOutput("trips_vol"), 
                      
                      h4("Trip Length"),
                      verbatimTextOutput("trip_length")
             ),
             tabPanel("Trip Map",
                      # date input
                      dateInput("dates","Date", value = "2013-07-01", 
                                min ="2013-07-01", max ="2016-12-31", 
                                startview = "year"),
                      # hour input
                      numericInput("hours", "Hour",
                                  value = 9, min = 0, max = 24, step = 1),
                      leafletOutput("map"))

  ),
  mainPanel()
)

shinyApp(ui = ui, server = server)

