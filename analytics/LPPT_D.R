library(tidyverse)
library(openskyr)
library(lubridate)

#########  Options  #######################################################

airport="LPPT"
option="departures" # Arrivals or Departures

Day1_MN=1640995200 # 00:00:00 01/01/22 (DAY 1 MIDNIGHT)
DayN_MN=1672444800 # 00:00:00 31/12/22 (DAY 365 MIDNIGHT)
Step=86400

#########  "For" loop  #######################################################

Date_Loop <- seq(Day1_MN, DayN_MN, by = Step)

LPPT_D <- data.frame()

for (i in Date_Loop) {
  LPPT_D_DF <- get_airport_data(Sys.getenv("OPENSKY_USERNAME"),Sys.getenv("OPENSKY_PASSWORD"),
                              airport,option,
                              begin=i, end=i+86399)
  LPPT_D <- rbind(LPPT_D, LPPT_D_DF)
}

rm(LPPT_D_DF)

#########  Data Manipulation  #################################################

LPPT_D$firstSeen <- as.numeric(LPPT_D$firstSeen)

LPPT_D$firstSeen <- as_datetime(LPPT_D$firstSeen)

LPPT_D$icao24 <- as.character(LPPT_D$icao24)

LPPT_D <- LPPT_D %>% 
  mutate(Year = substr(firstSeen, 1, 4),
         Month = substr(firstSeen, 6, 7),
         Day = substr(firstSeen, 9, 10),
         Hour = substr(firstSeen, 12, 13),
         Date = substr(firstSeen, 1, 10),
         Time = substr(firstSeen, 12, 19)) %>%
  
  mutate(Month = case_when(
    Month == "01" ~ "January",
    Month == "02" ~ "February",
    Month == "03" ~ "March",
    Month == "04" ~ "April",
    Month == "05" ~ "May",
    Month == "06" ~ "June",
    Month == "07" ~ "July",
    Month == "08" ~ "August",
    Month == "09" ~ "September",
    Month == "10" ~ "October",
    Month == "11" ~ "November",
    Month == "12" ~ "December"
  ))

LPPT_D$Hour <- as.numeric(LPPT_D$Hour)

LPPT_D$`Period of the Day` <- case_when(
  LPPT_D$Hour %in% c(1,2,3,4,5) ~ "Dawn",
  LPPT_D$Hour %in% c(6,7,8,9,10,11) ~ "Morning",
  LPPT_D$Hour %in% c(12,13,14,15,16,17,18,19) ~ "Afternoon",
  LPPT_D$Hour %in% c(20,21,22,23,0) ~ "Night",
)

LPPT_D$Date <- as_datetime(LPPT_D$Date)
LPPT_D$Date <- as_date(LPPT_D$Date)

LPPT_D$Weekday <- weekdays(LPPT_D$Date)

LPPT_D <- subset(LPPT_D, select = -c(estDepartureAirportHorizDistance, 
                                         estDepartureAirportVertDistance, 
                                         estArrivalAirportHorizDistance, 
                                         estArrivalAirportVertDistance, 
                                         departureAirportCandidatesCount, 
                                         arrivalAirportCandidatesCount,lastSeen))

LPPT_D <- rename(LPPT_D, ICAO_APT = `estArrivalAirport`)
LPPT_D$ICAO_APT <- as.character(LPPT_D$ICAO_APT)

LPPT_D$Airline <- substr(LPPT_D$callsign, 1, 3)

#########  Uploading External Files  ##########################################

# "Airline Codes"

Airline_Codes <- readxl::read_xlsx("files/Airline_Codes.xlsx")

Airline_Codes <- Airline_Codes %>% 
  select(`Airline Name`, `ICAO Designator`, Country) %>% 
  rename(Airline = `ICAO Designator`)

LPPT_D <- merge(LPPT_D, Airline_Codes, by = "Airline", all.x = TRUE)

# "Aircraft Database"

Aircraft_Database <- readxl::read_xlsx("files/Aircraft_Database.xlsx")

Aircraft_Database <- Aircraft_Database %>% 
  select(icao24, `Aircraft Type`, `Wake Turbulence Category`)

Aircraft_Database$icao24 <- as.character(Aircraft_Database$icao24)

LPPT_D <- merge(LPPT_D, Aircraft_Database, by = "icao24", all.x = TRUE)

# "Airport Codes"

Airport_Codes <- readxl::read_xlsx("files/Airport_Codes.xlsx")

Airport_Codes$gps_code <- as.character(Airport_Codes$gps_code)
Airport_Codes <- rename(Airport_Codes, ICAO_APT = `gps_code`) %>%
  rename(`Destination Country` = `Country`)

LPPT_D <- merge(LPPT_D, Airport_Codes, by = "ICAO_APT", all.x = TRUE)

######### Dataset Organization ################################################

LPPT_D <- rename(LPPT_D, `ICAO Arrival` = `ICAO_APT`) %>%
  rename(`ICAO Departure` = `estDepartureAirport`) %>%
  rename(`Date Hour` = firstSeen)

LPPT_D <- LPPT_D[, c("ICAO Departure", "ICAO Arrival", "Destination Country", 
                     "Longitude", "Latitude", "callsign", "Airline", 
                     "Airline Name", "Country", "icao24", "Aircraft Type", 
                     "Wake Turbulence Category", "Date Hour", "Year", "Month", 
                     "Day", "Hour", "Date", "Time", "Period of the Day", 
                     "Weekday")]