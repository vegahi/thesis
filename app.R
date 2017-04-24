library(RMySQL)
library(utils)
library(lubridate)
library(ggplot2)
library(leaflet)
library(rgdal)
library(shiny)
server <- function(input, output) {
  # connect to the database
  bikes <- src_mysql(host = "scidb.smith.edu",dbname = "citibike", user = "wzhang", password = "PedalP0wer")
  bikes <- etl("citibike", dir = "~/Desktop/citibike_data", db = src_mysql(host = "scidb.smith.edu",dbname = "citibike", user = "wzhang", password = "PedalP0wer"))
   output$total_trip <- renderPrint({
    bikes %>%
      tbl("trips_sub") %>%
      head()
      mutate(year = YEAR(Start_Time), month = MONTH(Start_Time), day = DAY(Start_Time), hour = HOUR(Start_Time)) %>%
      filter(year == year(input$dates) & month == month(input$dates) & day == day(input$dates) & hour == (input$hours)) %>%
      summarise(total_trip = n())
     
   })
   
   output$trips_vol <- renderTable({
     bikes %>%
       tbl("trips_sub") %>%
       mutate(year = YEAR(Start_Time), month = MONTH(Start_Time), day = DAY(Start_Time), hour = HOUR(Start_Time)) %>%
       filter(year == year(input$dates) & month == month(input$dates) & day == day(input$dates) & hour == (input$hours)) %>%
       group_by(Start_Station_ID, End_Station_ID) %>%
       summarise(trip_volume = n()) %>%
       arrange(desc(trip_volume)) %>%
       head(n = 10)
   })
   
   output$trip_length <- renderPrint({
     bikes %>%
       tbl("trips") %>%
       mutate(year = YEAR(Start_Time), month = MONTH(Start_Time), day = DAY(Start_Time), hour = HOUR(Start_Time)) %>%
       select(Trip_Duration, year, month, day, hour) %>%
       filter(year == year(input$dates) & month == month(input$dates) & day == day(input$dates) & hour == (input$hours)) %>%
       select(Trip_Duration) %>%
       summarise(mean = mean(Trip_Duration), sd = sd(Trip_Duration), max = max(Trip_Duration), min = min(Trip_Duration))
       
   })
   # output$yearPlot <- renderPlot({
   #   # by year
   #   trip_vol_year <- bikes %>%
   #     tbl("trips_starttime") %>%
   #     mutate(year = YEAR(Start_Time)) %>%
   #     group_by(year) %>%
   #     summarise(total_trip = n()) %>%
   #     collect()
   #   ggplot(data = trip_vol_year, aes(year,total_trip))+ geom_bar(stat = "identity", fill ="Light Blue")
   # })
  
  # # by month
  # output$monthPlot <- renderPlot({
  #   trip_vol_month <- bikes %>%
  #     tbl("trips_starttime") %>%
  #     mutate(year = YEAR(Start_Time), month = MONTH(Start_Time)) %>%
  #     group_by(year, month) %>%
  #     summarise(total_trip = n()) %>%
  #     collect()
  #   
  #   ggplot(data = trip_vol_month, aes(month,total_trip))+ geom_line(color ="Light Blue") + facet_wrap(~ year)
  # })
  # 
  # # by day
  # output$dayPlot <- renderPlot({
  #   trip_vol_day <- bikes %>%
  #     tbl("trips_starttime") %>%
  #     mutate(year = YEAR(Start_Time), month = MONTH(Start_Time), day = DAY(Start_Time)) %>%
  #     group_by(year, month, day) %>%
  #     summarise(total_trip = n()) %>%
  #     collect()
  #   ggplot(data = trip_vol_day, aes(day,total_trip))+ geom_line(color ="Light Blue") + facet_grid(month~ year)
  # })
  # 
    
  # #the map
  # #sample data 2013-07-01- 9am
  # output$map <- renderLeaflet({
  #   data <- bikes %>%
  #     tbl("trips_sub") %>%
  #     mutate(year = YEAR(Start_Time), month = MONTH(Start_Time), day = DAY(Start_Time), hour = HOUR(Start_Time)) %>%
  #     filter(year == year(input$dates) & month == month(input$dates) & day == day(input$dates) & hour == (input$hours)) %>%
  #     collect(n = Inf)
  # 
  #   plot_trip <- function(sample){
  #     master_stations <- bind_rows(
  #       sample %>%
  #         select(Start_Station_ID, Start_Station_Name, Start_Station_Longitude, Start_Station_Latitude) %>%
  #         rename(id = Start_Station_ID, name = Start_Station_Name,
  #                longitude = Start_Station_Longitude, latitude = Start_Station_Latitude),
  #       sample %>%
  #         select(End_Station_ID, End_Station_Name, End_Station_Longitude, End_Station_Latitude) %>%
  #         rename(id = End_Station_ID, name = End_Station_Name,
  #                longitude = End_Station_Longitude, latitude = End_Station_Latitude)) %>%
  #       distinct(id, name, longitude, latitude)
  # 
  #     trip_data <- sample %>%
  #       group_by(Start_Station_ID, End_Station_ID) %>%
  #       summarise(trips = n())
  # 
  #     station_count <- bind_rows(
  #       trip_data %>%
  #         select(Start_Station_ID, trips) %>%
  #         rename(id = Start_Station_ID),
  #       trip_data %>%
  #         select(-Start_Station_ID) %>%
  #         rename(id = End_Station_ID)) %>%
  #       select(-Start_Station_ID) %>%
  #       group_by(id) %>%
  #       summarise(total_trips = sum(trips))
  # 
  #     stations_plot <- left_join(station_count, master_stations, by = "id")
  # 
  # 
  #     # create the lines data set with start and end stations info
  #     lines <- bind_rows(
  #       sample %>%
  #         select(Start_Station_ID, End_Station_ID, Start_Station_Longitude, Start_Station_Latitude) %>%
  #         rename(lon = Start_Station_Longitude, lat = Start_Station_Latitude),
  #       sample %>%
  #         select(Start_Station_ID, End_Station_ID, End_Station_Longitude, End_Station_Latitude) %>%
  #         rename(lon = End_Station_Longitude, lat = End_Station_Latitude)) %>%
  #       arrange(Start_Station_ID, End_Station_ID) %>%
  #       na.omit() %>%
  #       group_by(Start_Station_ID, End_Station_ID) %>%
  #       do(line = Line(as.data.frame(select(., lon, lat))))
  # 
  #     # create line
  #     make_line <- function(x){
  #       Lines(list(x$line), ID = paste0(x$Start_Station_ID, "-", x$End_Station_ID))
  #     }
  # 
  #     # add trip info to the lines data
  #     lines <- sample %>%
  #       group_by(Start_Station_ID, End_Station_ID) %>%
  #       summarise(total_trip = n()) %>%
  #       right_join(., lines, by = c("Start_Station_ID" = "Start_Station_ID", "End_Station_ID" = "End_Station_ID"))
  # 
  #     # convert into SpatialLines and specify the projection to use
  #     lines_list <- apply(lines, MARGIN = 1, make_line)
  #     segments_sp <- SpatialLines(lines_list, proj4string = CRS("+init=epsg:4326"))
  # 
  #     # rename the rownames of lines
  #     for (i in (1:length(segments_sp))) {
  #       lines$link[i] <- segments_sp@lines[[i]]@ID
  #     }
  #     lines <- select(lines, -line)
  #     rownames(lines) <- lines$link
  #     lines <- as.data.frame(lines)
  # 
  #     # create a spatial lines data frame
  #     splndf <- SpatialLinesDataFrame(sl= segments_sp, data = lines, match.ID = TRUE)
  #     mylines_ll <- sp::spTransform(segments_sp, CRS("+init=epsg:4326"))
  # 
  #     # create the palette
  #     pal <- colorNumeric( palette = "Blues", domain = splndf$total_trip)
  # 
  #     # draw segment graph with leaflet
  #     # get rid of one-trip route
  #     leaflet() %>%
  #       addTiles() %>%
  #       #addProviderTiles("Esri.WorldImagery") %>%
  #       addPolylines(data = splndf,color = ~ pal(total_trip), weight = ~((total_trip-1) *10), noClip = TRUE,  fillOpacity = 100, group = "Trips")%>%
  #       addCircles(data = stations_plot, lng = ~longitude, lat = ~latitude, weight = 1, radius = ~ total_trips * 5 , popup = ~name, color = "black", group = "Stations") %>%
  #       setView(-73.9,40.7, zoom = 11) %>%
  #       addLayersControl(overlayGroups = c("Stations", "Trips"))
  #   }
  # 
  #   plot_trip(data)

  # })
}

ui <- fluidPage(
  # Application title
  titlePanel("CitiBike Data Analysis"),
  
  navbarPage("My Application",
             # tabPanel("Trip by Year",
             #          plotOutput("yearPlot")),
             # tabPanel("Trip by Month",
             #          plotOutput("monthPlot")),
             # tabPanel("Trip by Day",
             #          plotOutput("dayPlot")),
             tabPanel("Summary Statistics",
                      # date input
                      dateInput("dates","Date", value = "2013-07-01", min ="2013-07-01", max ="2016-12-31", startview = "year"),
                      # hour input
                      numericInput("hours", "Hour",
                                   value = 9, min = 0, max = 24, step = 1),
                      h4("Number of Trips (Total)"),
                      verbatimTextOutput("total_trip"),
                      
                      h4("Summary Trip Distribution"),
                      tableOutput("trips_vol"), 
                      
                      h4("Trip Length"),
                      verbatimTextOutput("trip_length")
                      )
             # tabPanel("Trip Map",
             #          # date input
             #          dateInput("dates","Date", value = "2013-07-01", min ="2013-07-01", max ="2016-12-31", startview = "year"),
             #          # hour input
             #          numericInput("hours", "Hour",
             #                      value = 9, min = 0, max = 24, step = 1),
             #          leafletOutput("map"))
             # navbarMenu("More",
             #            tabPanel("Sub-Component A"),
             #            tabPanel("Sub-Component B"))
  ),
    mainPanel( )
)

shinyApp(ui = ui, server = server)

