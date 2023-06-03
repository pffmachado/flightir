library(shiny)
library(leaflet)
library(dplyr)
library(openskyr)
library(ggplot2)
library(tidyr)
library(shinydashboard)
library(sp)
library(rnaturalearth)
library(DT)

tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"))

####### UI #####################################################################

ui <- dashboardPage(
  dashboardHeader(title = "LPPT - 2022"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("house")
      ),
      menuItem("Departures Volume", tabName = "departures", icon = icon("plane-departure"),
               menuSubItem("In Time", tabName = "departures_in_time", icon = icon("clock")),
               menuSubItem("Geographically", tabName = "departures_geographically", icon = icon("map"))
      ),
      menuItem("Arrivals Volume", tabName = "arrivals", icon = icon("plane-arrival"),
               menuSubItem("In Time", tabName = "arrivals_in_time", icon = icon("clock")),
               menuSubItem("Geographically", tabName = "arrivals_geographically", icon = icon("map"))
      ),
      menuItem("Arrival's Routes", tabName = "routes", icon = icon("route"))
    )
  ),
  dashboardBody(
    
###### Departures Tabs #########################################################
    
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(
          column(
            width = 12,
            height = "600px",
            h1 ("Welcome to this Dashboard!"),
            h1 (""),
            h1 (""),
            h1 (""),
            h2 ("This Dashboard is a user-friendly environment that present flight data for 
                Lisbon 'Humberto Delgado' Airport (LPPT) for the year of 2022."),
            h2(""),
            h2(""),
            h2("It represents the Final Project for the Aeronautical Sciences 
               Degree in ISEC Lisboa."),
            h2(""),
            h2(""),
            h3("Please navigate in this Dashboard using the sidepanel on the left, which 
            displays all available analysis."),
            h3(""),
            p("The data was collected from the OpenSky Network API, available in https://opensky-network.org/."),
            p(""),
            p(""),
            p("This Dashboard was programmed using the R Language. All the codes are available in
            https://github.com/miguelblsalazar/DASH_LPPT2022_OSN.")
          )
        )
      ),
      # Departures Volume tabs
      tabItem(tabName = "departures_in_time",
              fluidRow(
                column(
                  h2 ("Departures in Time"),
                  p("Departures Volumes over time. Time parameter can be changed in 'Time Reference' button."),
                  width = 12,
                  selectInput(
                    inputId = "time_reference_d",
                    label = "Time Reference",
                    choices = c("Month", "Weekday", "Period of Day", "Hour"),
                    selected = "Month"
                  ),
                  plotOutput(
                    outputId = "graph_dep_time",
                    height = "600px"
                  )
                )
              )
      ),
      tabItem(tabName = "departures_geographically",
              fluidRow(
                column(width = 12,
                       h2("Departures Geographically"),
                       p("Destination Countries and Airports from flights 
                         departing from Lisbon. The table provides views from 
                         a Country, Airport, Aircraft, or Airline perspective"),
                       tags$hr(),
                       column(width = 6,
                              selectInput(inputId = "map_type_d",
                                          label = "Map Type",
                                          choices = c("By Country (Europe and Northern Africa)", "By Country (Worldwide)", "By Airport"),
                                          selected = "By Country"),
                              plotOutput(outputId = "map_d", 
                                         width = "100%", 
                                         height = "700px")
                       ),
                       column(width = 6,
                              selectInput("table_type_d",
                                          label = "Table Type",
                                          choices = c("By Airport", "By Aircraft", "By Country", "By Airline"),
                                          selected = "Airport"),
                              div(style = "height: 700px; overflow-y: scroll;",
                                  DT::dataTableOutput("flight_counts_d")
                              )
                       )
                )
              )
      ),
    
###### Arrivals Tabs #########################################################
    
    tabItem(tabName = "arrivals_in_time",
            fluidRow(
              column(
                h2 ("Departures in Time"),
                p("Departures Volumes over time. Time parameter can be changed in 'Time Reference' button."),
                width = 12,
                selectInput(
                  inputId = "time_reference_a",
                  label = "Time Reference",
                  choices = c("Month", "Weekday", "Period of Day", "Hour"),
                  selected = "Month"
                ),
                plotOutput(
                  outputId = "graph_arr_time",
                  height = "600px"
                )
              )
            )
    ),
    tabItem(tabName = "arrivals_geographically",
            fluidRow(
              column(width = 12,
                     h2("Arrivals Geographically"),
                     p("Origin Countries and Airports from flights arriving in 
                       Lisbon. The table provides views from a Country, 
                       Airport, Aircraft, or Airline perspective"),
                     tags$hr(),
                     column(width = 6,
                            selectInput(inputId = "map_type_a",
                                        label = "Map Type",
                                        choices = c("By Country (Europe and Northern Africa)", "By Country (Worldwide)", "By Airport"),
                                        selected = "By Country (Europe and Northern Africa)"),
                            plotOutput(outputId = "map_a",
                                       width = "100%", 
                                       height = "700px")
                     ),
                     column(width = 6,
                            selectInput("table_type_a",
                                        label = "Table Type",
                                        choices = c("By Airport", "By Aircraft", "By Country", "By Airline"),
                                        selected = "Airport"),
                            div(style = "height: 700px; overflow-y: scroll;",
                                DT::dataTableOutput("flight_counts_a")
                            )
                     )
              )
            )
    ),
    
###### Routes Map Tab #######################################################
    
    tabItem(tabName = "routes",
            fluidRow(
              column(width=12,
                     h2 ("Arrival Routes"),
                     p("These maps provide routes for all arriving flights in Lisbon on
                       the Afternoon of September 15th. The objective of this visualization 
                       is to identify holding patterns, short and long approaches, for example."),
                     )
            ),
            column(width=6,
                   h2("Lateral Profile"),
                   leafletOutput("map_routes",
                          height = "600px")
            ),
            column(width = 6,
                   h2("Vertical Profile"),
                   plotOutput(outputId = "map_vertical_prof", 
                              height = "600px"))
    )
  )
  )
)

###### SERVER #################################################################

server <- function(input, output)  {

###### Arrivals in Time Section ###############################################
  
    arr_in_time <- reactive({
    switch(input$time_reference_a,
           "Month" = {
             arr_Month <- LPPT_A %>%
               mutate(`Wake Turbulence Category` = if_else(is.na(`Wake Turbulence Category`), 
                                                           "Undefined", `Wake Turbulence Category`)) %>%
               group_by(Month, `Wake Turbulence Category`) %>%
               summarize(num_flights = n()) %>%
               mutate(`Wake Turbulence Category` = factor(`Wake Turbulence Category`, levels = c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")))
             
             wtc_order <- c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")
             
             arr_Month <- as.data.frame(arr_Month)
             arr_Month$Month <- factor(arr_Month$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
             
             list(arr_Month = arr_Month, wtc_order = wtc_order)
           },
           "Weekday" = {
             arr_weekday <- LPPT_A %>%
               mutate(`Wake Turbulence Category` = if_else(is.na(`Wake Turbulence Category`), 
                                                           "Undefined", `Wake Turbulence Category`)) %>%
               group_by(Weekday, `Wake Turbulence Category`) %>%
               summarize(num_flights = n()) %>%
               mutate(`Wake Turbulence Category` = factor(`Wake Turbulence Category`, levels = c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")))
             
             wtc_order <- c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")
             
             arr_weekday <- as.data.frame(arr_weekday)
             arr_weekday$Weekday <- factor(arr_weekday$Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
             
             list(arr_weekday = arr_weekday, wtc_order = wtc_order)
           },
           "Period of Day" = {
             arr_period <- LPPT_A %>%
               mutate(`Wake Turbulence Category` = if_else(is.na(`Wake Turbulence Category`), 
                                                           "Undefined", `Wake Turbulence Category`)) %>%
               group_by(`Period of the Day`, `Wake Turbulence Category`) %>%
               summarize(num_flights = n()) %>%
               mutate(`Wake Turbulence Category` = factor(`Wake Turbulence Category`, 
                                                          levels = c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")))
             
             wtc_order <- c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")
             
             arr_period <- as.data.frame(arr_period)
             arr_period$`Period of the Day` <- factor(arr_period$`Period of the Day`, levels = c("Dawn", "Morning", "Afternoon", "Night"))
             
             list(arr_period = arr_period, wtc_order = wtc_order)
           },
           "Hour" = {
             arr_hour <- LPPT_A %>%
               mutate(`Wake Turbulence Category` = if_else(is.na(`Wake Turbulence Category`), 
                                                           "Undefined", `Wake Turbulence Category`)) %>%
               group_by(Hour, `Wake Turbulence Category`) %>%
               summarize(num_flights = n()) %>%
               mutate(`Wake Turbulence Category` = factor(`Wake Turbulence Category`, 
                                                          levels = c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")))
             
             wtc_order <- c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")
             
             arr_hour <- as.data.frame(arr_hour)
             
             list(arr_hour = arr_hour, wtc_order = wtc_order)
           })
  })
  

  output$graph_arr_time <- renderPlot({

    arr_time_plot <- ggplot() +
      theme_minimal()

    arr_in_time <- arr_in_time()
    

    if (input$time_reference_a %in% c("Month", "Weekday", "Period of Day", "Hour")) {

      arr_time_plot <- switch(input$time_reference_a,
                              "Month" = {
                                arr_Month <- arr_in_time$arr_Month
                                wtc_order <- arr_in_time$wtc_order
                                
                                ggplot(arr_Month, 
                                       aes(x = Month, 
                                           y = num_flights, 
                                           fill = `Wake Turbulence Category`)) +
                                  geom_bar(stat = "identity", position = "stack") +
                                  scale_fill_manual(breaks = wtc_order,
                                                    values = c("#00008B", "#FFA500", "#00FF00", "#FFFF00", "#999999")) +
                                  labs(title = "Flights per Month in LPPT - 2022", 
                                       x = "Month", 
                                       y = "Number of flights", 
                                       fill = "Wake Turbulence Category:") +
                                  theme_minimal() +
                                  theme(plot.title = element_text(hjust = 0.5))
                              },
                              "Weekday" = {
                                arr_weekday <- arr_in_time$arr_weekday
                                wtc_order <- arr_in_time$wtc_order
                                
                                ggplot(arr_weekday, 
                                       aes(x = Weekday, 
                                           y = num_flights, 
                                           fill = `Wake Turbulence Category`)) +
                                  geom_bar(stat = "identity", position = "stack") +
                                  scale_fill_manual(breaks = wtc_order,
                                                    values = c("#00008B", "#FFA500", "#00FF00", "#FFFF00", "#999999")) +
                                  labs(title = "Arrivals per Weekday in LPPT - 2022", 
                                       x = "Weekday", 
                                       y = "Number of flights", 
                                       fill = "Wake Turbulence Category:") +
                                  theme_minimal() +
                                  theme(plot.title = element_text(hjust = 0.5))
                              },
                              "Period of Day" = {
                                arr_period <- arr_in_time$arr_period
                                wtc_order <- arr_in_time$wtc_order
                                
                                ggplot(arr_period, 
                                       aes(x = `Period of the Day`, 
                                           y = num_flights, 
                                           fill = `Wake Turbulence Category`)) +
                                  geom_bar(stat = "identity", position = "stack") +
                                  scale_fill_manual(breaks = wtc_order,
                                                    values = c("#00008B", "#FFA500", "#00FF00", "#FFFF00", "#999999")) +
                                  labs(title = "Arrivals per Period of the Day in LPPT - 2022", 
                                       x = "Period of the Day", 
                                       y = "Number of flights", 
                                       fill = "Wake Turbulence Category:") +
                                  theme_minimal() +
                                  theme(plot.title = element_text(hjust = 0.5))
                              },
                              "Hour" = {
                                arr_hour <- arr_in_time$arr_hour
                                wtc_order <- arr_in_time$wtc_order
                                
                                ggplot(arr_hour, 
                                       aes(x = Hour, 
                                           y = num_flights, 
                                           fill = `Wake Turbulence Category`)) +
                                  geom_bar(stat = "identity", position = "stack") +
                                  scale_fill_manual(breaks = wtc_order,
                                                    values = c("#00008B", "#FFA500", "#00FF00", "#FFFF00", "#999999")) +
                                  labs(title = "Arrivals per Hour in LPPT - 2022", 
                                       x = "Hours of the Day", 
                                       y = "Number of flights", 
                                       fill = "Wake Turbulence Category:") +
                                  theme_minimal() +
                                  theme(plot.title = element_text(hjust = 0.5))
                              })
    }
    
    print(arr_time_plot)
  })

###### Departures in Time Section #############################################
  
  dep_in_time <- reactive({
    switch(input$time_reference_d,
           "Month" = {
             dep_Month <- LPPT_D %>%
               mutate(`Wake Turbulence Category` = if_else(is.na(`Wake Turbulence Category`), 
                                                           "Undefined", `Wake Turbulence Category`)) %>%
               group_by(Month, `Wake Turbulence Category`) %>%
               summarize(num_flights = n()) %>%
               mutate(`Wake Turbulence Category` = factor(`Wake Turbulence Category`, levels = c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")))
             
             wtc_order <- c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")
             
             dep_Month <- as.data.frame(dep_Month)
             dep_Month$Month <- factor(dep_Month$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
             
             list(dep_Month = dep_Month, wtc_order = wtc_order)
           },
           "Weekday" = {
             dep_weekday <- LPPT_D %>%
               mutate(`Wake Turbulence Category` = if_else(is.na(`Wake Turbulence Category`), 
                                                           "Undefined", `Wake Turbulence Category`)) %>%
               group_by(Weekday, `Wake Turbulence Category`) %>%
               summarize(num_flights = n()) %>%
               mutate(`Wake Turbulence Category` = factor(`Wake Turbulence Category`, levels = c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")))
             
             wtc_order <- c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")
             
             dep_weekday <- as.data.frame(dep_weekday)
             dep_weekday$Weekday <- factor(dep_weekday$Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
             
             list(dep_weekday = dep_weekday, wtc_order = wtc_order)
           },
           "Period of Day" = {
             dep_period <- LPPT_D %>%
               mutate(`Wake Turbulence Category` = if_else(is.na(`Wake Turbulence Category`), 
                                                           "Undefined", `Wake Turbulence Category`)) %>%
               group_by(`Period of the Day`, `Wake Turbulence Category`) %>%
               summarize(num_flights = n()) %>%
               mutate(`Wake Turbulence Category` = factor(`Wake Turbulence Category`, 
                                                          levels = c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")))
             
             wtc_order <- c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")
             
             dep_period <- as.data.frame(dep_period)
             dep_period$`Period of the Day` <- factor(dep_period$`Period of the Day`, levels = c("Dawn", "Morning", "Afternoon", "Night"))
             
             list(dep_period = dep_period, wtc_order = wtc_order)
           },
           "Hour" = {
             dep_hour <- LPPT_D %>%
               mutate(`Wake Turbulence Category` = if_else(is.na(`Wake Turbulence Category`), 
                                                           "Undefined", `Wake Turbulence Category`)) %>%
               group_by(Hour, `Wake Turbulence Category`) %>%
               summarize(num_flights = n()) %>%
               mutate(`Wake Turbulence Category` = factor(`Wake Turbulence Category`, 
                                                          levels = c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")))
             
             wtc_order <- c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")
             
             dep_hour <- as.data.frame(dep_hour)
             
             list(dep_hour = dep_hour, wtc_order = wtc_order)
           })
  })
  
  output$graph_dep_time <- renderPlot({

    dep_time_plot <- ggplot() +
      theme_minimal()
    
    dep_in_time <- dep_in_time()
    
    if (input$time_reference_d %in% c("Month", "Weekday", "Period of Day", "Hour")) {

      dep_time_plot <- switch(input$time_reference_d,
                              "Month" = {
                                dep_Month <- dep_in_time$dep_Month
                                wtc_order <- dep_in_time$wtc_order
                                
                                ggplot(dep_Month, 
                                       aes(x = Month, 
                                           y = num_flights, 
                                           fill = `Wake Turbulence Category`)) +
                                  geom_bar(stat = "identity", position = "stack") +
                                  scale_fill_manual(breaks = wtc_order,
                                                    values = c("#00008B", "#FFA500", "#00FF00", "#FFFF00", "#999999")) +
                                  labs(title = "Departures per Month in LPPT - 2022", 
                                       x = "Month", 
                                       y = "Number of flights", 
                                       fill = "Wake Turbulence Category:") +
                                  theme_minimal() +
                                  theme(plot.title = element_text(hjust = 0.5))
                              },
                              "Weekday" = {
                                dep_weekday <- dep_in_time$dep_weekday
                                wtc_order <- dep_in_time$wtc_order
                                
                                ggplot(dep_weekday, 
                                       aes(x = Weekday, 
                                           y = num_flights, 
                                           fill = `Wake Turbulence Category`)) +
                                  geom_bar(stat = "identity", position = "stack") +
                                  scale_fill_manual(breaks = wtc_order,
                                                    values = c("#00008B", "#FFA500", "#00FF00", "#FFFF00", "#999999")) +
                                  labs(title = "Departures per Weekday in LPPT - 2022", 
                                       x = "Weekday", 
                                       y = "Number of flights", 
                                       fill = "Wake Turbulence Category:") +
                                  theme_minimal() +
                                  theme(plot.title = element_text(hjust = 0.5))
                              },
                              "Period of Day" = {
                                dep_period <- dep_in_time$dep_period
                                wtc_order <- dep_in_time$wtc_order
                                
                                ggplot(dep_period, 
                                       aes(x = `Period of the Day`, 
                                           y = num_flights, 
                                           fill = `Wake Turbulence Category`)) +
                                  geom_bar(stat = "identity", position = "stack") +
                                  scale_fill_manual(breaks = wtc_order,
                                                    values = c("#00008B", "#FFA500", "#00FF00", "#FFFF00", "#999999")) +
                                  labs(title = "Departures per Period of the Day in LPPT - 2022", 
                                       x = "Period of the Day", 
                                       y = "Number of flights", 
                                       fill = "Wake Turbulence Category:") +
                                  theme_minimal() +
                                  theme(plot.title = element_text(hjust = 0.5))
                              },
                              "Hour" = {
                                dep_hour <- dep_in_time$dep_hour
                                wtc_order <- dep_in_time$wtc_order
                                
                                ggplot(dep_hour, 
                                       aes(x = Hour, 
                                           y = num_flights, 
                                           fill = `Wake Turbulence Category`)) +
                                  geom_bar(stat = "identity", position = "stack") +
                                  scale_fill_manual(breaks = wtc_order,
                                                    values = c("#00008B", "#FFA500", "#00FF00", "#FFFF00", "#999999")) +
                                  labs(title = "Arrivals per Hour in LPPT - 2022", 
                                       x = "Hours of the Day", 
                                       y = "Number of flights", 
                                       fill = "Wake Turbulence Category:") +
                                  theme_minimal() +
                                  theme(plot.title = element_text(hjust = 0.5))
                              })
    }
    
    print(dep_time_plot)
  })
  
##### Arrivals Geographically (Maps) ###########################################

  observe({
    if (input$map_type_a == "By Country (Europe and Northern Africa)") {
      
      world <- ne_countries(scale = "medium", returnclass = "sf")
      world <- world %>% rename("Origin Country" = sovereignt)
      eu_a_map <- merge(world, DF_A_Country, by.x = "Origin Country", all.x = TRUE)

      MAP_A_EU_Country <- ggplot() +
        geom_sf(data = eu_a_map, aes(fill = `Number of Flights`)) +
        coord_sf(xlim = c(-30, 40), ylim = c(20, 75), expand = FALSE) +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        theme_void() +
        labs(size = NULL, title = "Number of Flights per Origin Country in Europe and Northern Africa - 2022") +
        theme(plot.title = element_text(hjust = 0.5))

      output$map_a <- renderPlot({
        MAP_A_EU_Country
      })
    }
    else if (input$map_type_a == "By Airport") {
      world <- ne_countries(scale = "medium", returnclass = "sf")
      
      Europe <- world[which(world$continent %in% c("Europe", "Africa", "Asia")),]
      
      MAP_A_EU_Airport <- ggplot(Europe) +
        geom_sf() +
        coord_sf(xlim = c(-30,40), ylim = c(20,75), expand = FALSE) + # Argument that defines the European Continent
        geom_point(data = DF_A_Airport, aes(x=lon, y=lat, size = `Number of Flights`, color = `Number of Flights`)) +
        scale_size(range = c(1, 10), guide = guide_legend(title = "")) +
        scale_color_gradient(low = "lightblue", high = "darkblue", guide = guide_colorbar(title = "Number of Flights")) +
        theme_void()
        labs(size = NULL, title = "Number of Flights per Origin Airport in Europe and Northern Africa - 2022") + 
        theme(plot.title = element_text(hjust = 0.5))
      
      output$map_a <- renderPlot({
        MAP_A_EU_Airport
      })
    }
    else if (input$map_type_a == "By Country (Worldwide)") {
      world <- ne_countries(scale = "medium", returnclass = "sf")
      world <- world %>% rename("Origin Country" = sovereignt)
      world_a_map <- merge(world, DF_A_Country, by.x = "Origin Country", all.x = TRUE)
      
      MAP_A_World_Country <- ggplot() +
        geom_sf(data = world_a_map, aes(fill = `Number of Flights`)) +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        theme_void() +
        labs(size = NULL, title = "Number of Flights per Origin Country Worldwide - 2022") + 
        theme(plot.title = element_text(hjust = 0.5))
      
      output$map_a <- renderPlot({
        MAP_A_World_Country
      })
    }
  })
  
##### Arrivals Geographically (Tables) ###########################################

  output$flight_counts_a <- renderDataTable({
    if (input$table_type_a == "By Country") {
      top_countries_a <- LPPT_A %>%
        group_by(`Origin Country`) %>%
        summarize(`Number of Flights` = n(),
                  `Most common Airline` = `Airline Name`[which.max(tabulate(match(`Airline Name`, `Airline Name`)))],
                  `Most commom operated Aircraft` = `Aircraft Type`[which.max(tabulate(match(`Aircraft Type`, `Aircraft Type`)))]) %>%
        filter(!is.na(`Origin Country`)) %>%
        arrange(desc(`Number of Flights`))
      
      rm(top_countries_a)
      counts <- DF_A_Country %>%
        filter(!is.na(`Origin Country`))
      
    } else if (input$table_type_a == "By Airport") {
      top_airports_a <- LPPT_A %>%
        group_by(`ICAO Departure`) %>%
        summarize(`Number of Flights` = n(),                        
                  lat = mean(Latitude, na.rm = FALSE),
                  lon = mean(Longitude, na.rm = FALSE),
                  `Most common Airline` = `Airline Name`[which.max(tabulate(match(`Airline Name`, `Airline Name`)))],
                  `Most common operated Aircraft` = `Aircraft Type`[which.max(tabulate(match(`Aircraft Type`, `Aircraft Type`)))]) %>%
        filter(!is.na(`ICAO Departure`)) %>%
        filter(!is.na(lat)) %>%
        filter(!is.na(lon)) %>%
        arrange(desc(`Number of Flights`))
      
      top_airports_a$lat <- as.numeric(top_airports_a$lat)
      top_airports_a$lon <- as.numeric(top_airports_a$lon)
      
      DF_D_Airport <- top_airports_a
      counts <- DF_A_Airport %>%
        select(`ICAO Departure`, `Number of Flights`, `Most common Airline`, `Most common operated Aircraft`)
      
    } else if (input$table_type_a == "By Airline") {
      top_airlines_a <- LPPT_A %>%
        group_by(`Airline Name`) %>%
        summarize(`Number of Flights` = n(),
                  `Most common Aircraft Type` = `Aircraft Type`[which.max(tabulate(match(`Aircraft Type`, `Aircraft Type`)))],
                  `Most commom Origin Country` = `Origin Country` [which.max(tabulate(match(`Origin Country`, `Origin Country`)))]) %>%
        filter(!is.na(`Airline Name`)) %>%
        arrange(desc(`Number of Flights`))
      
      DF_A_Airline <- top_airlines_a
      counts <- DF_A_Airline
      
    } else if (input$table_type_a == "By Aircraft") {
      top_aircraft_a <- LPPT_A %>%
        group_by(`Aircraft Type`) %>%
        summarize(`Number of Flights` = n(),
                  `Most common Airline` = `Airline Name`[which.max(tabulate(match(`Airline Name`, `Airline Name`)))],
                  `Most commom Origin Country` = `Origin Country` [which.max(tabulate(match(`Origin Country`, `Origin Country`)))]) %>%
        filter(!is.na(`Aircraft Type`)) %>%
        
        arrange(desc(`Number of Flights`))
      
      DF_A_Aircraft <- top_aircraft_a
      counts <- DF_A_Aircraft
      
    } else {
      counts <- NULL
    }
    
    counts
  }, options = list(pageLength = -1))
  

##### Departures Geographically (Maps) #######################################
  
  observe({
    if (input$map_type_d == "By Country (Europe and Northern Africa)") {
      
      world <- ne_countries(scale = "medium", returnclass = "sf")
      world <- world %>% rename("Destination Country" = sovereignt)
      eu_d_map <- merge(world, DF_D_Country, by.x = "Destination Country", all.x = TRUE)
      
      MAP_A_EU_Country <- ggplot() +
        geom_sf(data = eu_d_map, aes(fill = `Number of Flights`)) +
        coord_sf(xlim = c(-30, 40), ylim = c(20, 75), expand = FALSE) +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        theme_void() +
        labs(size = NULL, title = "Number of Flights per Origin Country in Europe and Northern Africa - 2022") +
        theme(plot.title = element_text(hjust = 0.5))
      
      output$map_d <- renderPlot({
        MAP_D_EU_Country
      })
    }
    else if (input$map_type_d == "By Airport") {
      world <- ne_countries(scale = "medium", returnclass = "sf")
      Europe <- world[which(world$continent %in% c("Europe", "Africa", "Asia")),]
      
      MAP_D_EU_Airport <- ggplot(Europe) +
        geom_sf() +
        coord_sf(xlim = c(-30,40), ylim = c(20,75), expand = FALSE) + # Argument that defines the European Continent
        geom_point(data = DF_D_Airport, aes(x=lon, y=lat, size = `Number of Flights`, color = `Number of Flights`)) +
        scale_size(range = c(1, 10), guide = guide_legend(title = "")) +
        scale_color_gradient(low = "lightblue", high = "darkblue", guide = guide_colorbar(title = "Number of Flights")) +
        theme_void()
      labs(size = NULL, title = "Number of Flights per Destination Airport in Europe and Northern Africa - 2022") + 
        theme(plot.title = element_text(hjust = 0.5))
      
      output$map_d <- renderPlot({
        MAP_D_EU_Airport
      })
    }
    else if (input$map_type_d == "By Country (Worldwide)") {
      world <- ne_countries(scale = "medium", returnclass = "sf")
      world <- world %>% rename("Destination Country" = sovereignt)
      world_d_map <- merge(world, DF_D_Country, by.x = "Destination Country", all.x = TRUE)
      
      MAP_D_World_Country <- ggplot() +
        geom_sf(data = world_d_map, aes(fill = `Number of Flights`)) +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        theme_void() +
        labs(size = NULL, title = "Number of Flights per Destination Country Worldwide - 2022") + 
        theme(plot.title = element_text(hjust = 0.5))
      
      output$map_d <- renderPlot({
        MAP_D_World_Country
      })
    }
  })

  ##### Departure Geographically (Tables) ###########################################
  
  output$flight_counts_d <- renderDataTable({
    if (input$table_type_d == "By Country") {
      top_countries_d <- LPPT_D %>%
        group_by(`Destination Country`) %>%
        summarize(`Number of Flights` = n(),
                  `Most common Airline` = `Airline Name`[which.max(tabulate(match(`Airline Name`, `Airline Name`)))],
                  `Most commom operated Aircraft` = `Aircraft Type`[which.max(tabulate(match(`Aircraft Type`, `Aircraft Type`)))]) %>%
        filter(!is.na(`Destination Country`)) %>%
        arrange(desc(`Number of Flights`))
      
      rm(top_countries_d)
      counts <- DF_D_Country %>%
        filter(!is.na(`Destination Country`))
      
    } else if (input$table_type_d == "By Airport") {
      top_airports_d <- LPPT_D %>%
        group_by(`ICAO Arrival`) %>%
        summarize(`Number of Flights` = n(),                        
                  lat = mean(Latitude, na.rm = FALSE),
                  lon = mean(Longitude, na.rm = FALSE),
                  `Most common Airline` = `Airline Name`[which.max(tabulate(match(`Airline Name`, `Airline Name`)))],
                  `Most common operated Aircraft` = `Aircraft Type`[which.max(tabulate(match(`Aircraft Type`, `Aircraft Type`)))]) %>%
        filter(!is.na(`ICAO Arrival`)) %>%
        filter(!is.na(lat)) %>%
        filter(!is.na(lon)) %>%
        arrange(desc(`Number of Flights`))
      
      top_airports_d$lat <- as.numeric(top_airports_d$lat)
      top_airports_d$lon <- as.numeric(top_airports_d$lon)
      
      DF_D_Airport <- top_airports_d
      counts <- DF_D_Airport %>%
        select(`ICAO Arrival`, `Number of Flights`, `Most common Airline`, `Most common operated Aircraft`)
      
    } else if (input$table_type_d == "By Airline") {
      top_airlines_d <- LPPT_D %>%
        group_by(`Airline Name`) %>%
        summarize(`Number of Flights` = n(),
                  `Most common Aircraft Type` = `Aircraft Type`[which.max(tabulate(match(`Aircraft Type`, `Aircraft Type`)))],
                  `Most commom Destination Country` = `Destination Country` [which.max(tabulate(match(`Destination Country`, `Destination Country`)))]) %>%
        filter(!is.na(`Airline Name`)) %>%
        arrange(desc(`Number of Flights`))
      
      DF_D_Airline <- top_airlines_d
      counts <- DF_D_Airline
      
    } else if (input$table_type_d == "By Aircraft") {
      top_aircraft_d <- LPPT_D %>%
        group_by(`Aircraft Type`) %>%
        summarize(`Number of Flights` = n(),
                  `Most common Airline` = `Airline Name`[which.max(tabulate(match(`Airline Name`, `Airline Name`)))],
                  `Most commom Destination Country` = `Destination Country` [which.max(tabulate(match(`Destination Country`, `Destination Country`)))]) %>%
        filter(!is.na(`Aircraft Type`)) %>%
        
        arrange(desc(`Number of Flights`))
      
      DF_D_Aircraft <- top_aircraft_d
      counts <- DF_D_Aircraft
      
    } else {
      counts <- NULL
    }
    
    counts
  }, options = list(pageLength = -1))

###### Arrival Routes #########################################################
  output$map_routes <- renderLeaflet({
    
    track_longitude <- as.numeric(pull(LPPT_TRK, longitude))
    track_latitude <- as.numeric(pull(LPPT_TRK, latitude))
    
    LPPT_TRK_LAT <- data.frame(track_latitude, track_longitude)
    
    TRK_A_LAT <- leaflet() %>%
      addTiles() %>%
      setView(lng = -9.1393, lat = 38.7223, zoom = 10)  # Set the initial map view
    
    TRK_A_LAT <- addPolylines(TRK_A_LAT, data = LPPT_TRK_LAT, lng = ~track_longitude, lat = ~track_latitude, color = "red")
    
    TRK_A_LAT
  })

  output$map_vertical_prof <- renderPlot({
    
    track_altitude <- as.numeric(pull(LPPT_TRK, baro_altitude))
    LPPT_TRK_VER <- data.frame (callsign, track_altitude)
    
    LPPT_TRK_VER <- subset(LPPT_TRK_VER, baro_altitude < 3000)
    
    LPPT_TRK_VER <- LPPT_TRK_VER[order(LPPT_TRK_VER$callsign, -LPPT_TRK_VER$baro_altitude), ]
    
    LPPT_TRK_VER$Index <- ave(LPPT_TRK_VER$baro_altitude, LPPT_TRK_VER$callsign, FUN = function(x) rev(seq_along(x)))
    
    LPPT_TRK_VER <- LPPT_TRK_VER[LPPT_TRK_VER$Index >= 1 & LPPT_TRK_VER$Index <= 300, ]
    
    TRK_A_VER <- ggplot(LPPT_TRK_VER, aes(x = -Index, y = baro_altitude, group = callsign, color = callsign)) +
      geom_smooth(se = FALSE) +
      scale_x_continuous(labels = NULL, breaks = NULL) +
      labs(x = "", y = "Altitude (m)", color = "Callsigns") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    print(TRK_A_VER)
  })
  
  } # FINAL DO SERVER

##### LAUCH APP ###############################################################

shinyApp(ui, server)