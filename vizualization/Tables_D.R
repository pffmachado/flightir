library(dplyr)

###### Most common Aircraft Types departing from Lisbon #######################

DF_D_Country <- LPPT_D %>%
  group_by(`Aircraft Type`) %>%
  summarize(`Number of Flights` = n(),
            `Most common Airline` = `Airline Name`[which.max(tabulate(match(`Airline Name`, `Airline Name`)))],
            `Most commom Destination Country` = `Destination Country` [which.max(tabulate(match(`Destination Country`, `Destination Country`)))]) %>%
  filter(!is.na(`Aircraft Type`)) %>%
  
  arrange(desc(`Number of Flights`))

###### Most common Airlines departing from Lisbon  ############################

DF_D_Airline <- LPPT_D %>%
  group_by(`Airline Name`) %>%
  summarize(`Number of Flights` = n(),
            `Most common Aircraft Type` = `Aircraft Type`[which.max(tabulate(match(`Aircraft Type`, `Aircraft Type`)))],
            `Most commom Destination Country` = `Destination Country` [which.max(tabulate(match(`Destination Country`, `Destination Country`)))]) %>%
  filter(!is.na(`Airline Name`)) %>%
  
  arrange(desc(`Number of Flights`))

###### Airports more frequently visited by departing planes from LPPT #########

DF_D_Airport <- LPPT_D %>%
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

DF_D_Airport$lat <- as.numeric(DF_D_Airport$lat)
DF_D_Airport$lon <- as.numeric(DF_D_Airport$lon)

###### Countries more frequently visited by departing planes from LPPT ########

DF_D_Country <- LPPT_D %>%
  group_by(`Destination Country`) %>%
  summarize(`Number of Flights` = n(),
            `Most common Airline` = `Airline Name`[which.max(tabulate(match(`Airline Name`, `Airline Name`)))],
            `Most commom operated Aircraft` = `Aircraft Type`[which.max(tabulate(match(`Aircraft Type`, `Aircraft Type`)))]) %>%
  filter(!is.na(`Destination Country`)) %>% 
  
  arrange(desc(`Number of Flights`))