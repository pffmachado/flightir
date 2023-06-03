library(dplyr)

###### Most common Aircraft Types arriving to Lisbon ##########################

DF_A_Aircraft <- LPPT_A %>%
  group_by(`Aircraft Type`) %>%
  summarize(`Number of Flights` = n(),
            `Most common Airline` = `Airline Name`[which.max(tabulate(match(`Airline Name`, `Airline Name`)))],
            `Most commom Origin Country` = `Origin Country` [which.max(tabulate(match(`Origin Country`, `Origin Country`)))]) %>%
  filter(!is.na(`Aircraft Type`)) %>%
  
  arrange(desc(`Number of Flights`))

###### Most common Airlines arriving to Lisbon ################################

DF_A_Airline <- LPPT_A %>%
  group_by(`Airline Name`) %>%
  summarize(`Number of Flights` = n(),
            `Most common Aircraft Type` = `Aircraft Type`[which.max(tabulate(match(`Aircraft Type`, `Aircraft Type`)))],
            `Most commom Origin Country` = `Origin Country` [which.max(tabulate(match(`Origin Country`, `Origin Country`)))]) %>%
  filter(!is.na(`Airline Name`)) %>%
  
  arrange(desc(`Number of Flights`))

###### Airports more frequently sending planes to LPPT ########################

DF_A_Airport <- LPPT_A %>%
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

DF_A_Airport$lat <- as.numeric(DF_A_Airport$lat)
DF_A_Airport$lon <- as.numeric(DF_A_Airport$lon)

####### Countries more frequently sending planes to LPPT ######################

top_countries_a <- LPPT_A %>%
  group_by(`Origin Country`) %>%
  summarize(`Number of Flights` = n(),
            `Most common Airline` = `Airline Name`[which.max(tabulate(match(`Airline Name`, `Airline Name`)))],
            `Most commom operated Aircraft` = `Aircraft Type`[which.max(tabulate(match(`Aircraft Type`, `Aircraft Type`)))]) %>%
  filter(!is.na(`Origin Country`)) %>%
  
  arrange(desc(`Number of Flights`))