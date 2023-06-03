library(tidyverse)
library(openskyr)
library(lubridate)

#########  Options  #######################################################


airport="LPPT"
option="arrivals" # Arrivals or departures

Day1_MN=1640995200 # 00:00:00 01/01/22 (DAY 1 MIDNIGHT)
DayN_MN=1672444800 # 00:00:00 31/12/22 (DAY 365 MIDNIGHT)

Step=86400

#########  "For" loop  ########################################################

Date_Loop <- seq(Day1_MN, DayN_MN, by = Step)

LPPT_A <- data.frame()

for (i in Date_Loop) {
  LPPT_A_DF <- get_airport_data(Sys.getenv("OPENSKY_USERNAME"),Sys.getenv("OPENSKY_PASSWORD"),
                                 airport,option,
                                 begin=i, end=i+86399)
  LPPT_A <- rbind(LPPT_A, LPPT_A_DF)
}

rm(LPPT_A_DF)

#########  Data Manipulation  #################################################

LPPT_A$lastSeen <- as.numeric(LPPT_A$lastSeen)

LPPT_A$lastSeen <- as_datetime(LPPT_A$lastSeen)

LPPT_A$icao24 <- as.character(LPPT_A$icao24)

LPPT_A <- LPPT_A %>% 
  mutate(Year = substr(lastSeen, 1, 4),
         Month = substr(lastSeen, 6, 7),
         Day = substr(lastSeen, 9, 10),
         Hour = substr(lastSeen, 12, 13),
         Date = substr(lastSeen, 1, 10),
         Time = substr(lastSeen, 12, 19)) %>% 
  
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

LPPT_A$Hour <- as.numeric(LPPT_A$Hour)

LPPT_A$`Period of the Day` <- case_when(
  LPPT_A$Hour %in% c(1,2,3,4,5) ~ "Dawn",
  LPPT_A$Hour %in% c(6,7,8,9,10,11) ~ "Morning",
  LPPT_A$Hour %in% c(12,13,14,15,16,17,18,19) ~ "Afternoon",
  LPPT_A$Hour %in% c(20,21,22,23,0) ~ "Night",
)

LPPT_A$Date <- as_datetime(LPPT_A$Date)
LPPT_A$Date <- as_date(LPPT_A$Date)

LPPT_A$Weekday <- weekdays(LPPT_A$Date)

LPPT_A <- subset(LPPT_A, select = -c(estDepartureAirportHorizDistance, 
                                     estDepartureAirportVertDistance, 
                                     estArrivalAirportHorizDistance, 
                                     estArrivalAirportVertDistance, 
                                     departureAirportCandidatesCount, 
                                     arrivalAirportCandidatesCount,firstSeen))

LPPT_A <- rename(LPPT_A, ICAO_APT = `estDepartureAirport`)
LPPT_A$ICAO_APT <- as.character(LPPT_A$ICAO_APT)

LPPT_A$Airline <- substr(LPPT_A$callsign, 1, 3)

#########  Uploading External Files ###########################################

# "Airline Codes"

Airline_Codes <- readxl::read_xlsx("files/Airline_Codes.xlsx")

Airline_Codes <- Airline_Codes %>% 
  select(`Airline Name`, `ICAO Designator`, Country) %>% 
  rename(Airline = `ICAO Designator`)

LPPT_A <- merge(LPPT_A, Airline_Codes, by = "Airline", all.x = TRUE)

# "Aircraft Database"

Aircraft_Database <- readxl::read_xlsx("files/Aircraft_Database.xlsx")

Aircraft_Database <- Aircraft_Database %>% 
  select(icao24, `Aircraft Type`, `Wake Turbulence Category`)

Aircraft_Database$icao24 <- as.character(Aircraft_Database$icao24)

LPPT_A <- merge(LPPT_A, Aircraft_Database, by = "icao24", all.x = TRUE)

# "Airport Codes"

Airport_Codes <- readxl::read_xlsx("files/Airport_Codes.xlsx")

Airport_Codes$gps_code <- as.character(Airport_Codes$gps_code)
Airport_Codes <- rename(Airport_Codes, ICAO_APT = `gps_code`) %>%
  rename(`Origin Country` = `Country`)

LPPT_A <- merge(LPPT_A, Airport_Codes, by = "ICAO_APT", all.x = TRUE)

#########  Dataset Organization  #############################################

LPPT_A <- rename(LPPT_A, `ICAO Departure` = `ICAO_APT`)%>%
  rename(`ICAO Arrival` = `estArrivalAirport`) %>%
  rename(`Date Hour` = lastSeen)

LPPT_A <- LPPT_A[, c("ICAO Departure", "ICAO Arrival", "Origin Country", 
                     "Longitude", "Latitude", "callsign", "Airline", 
                     "Airline Name", "Country", "icao24", "Aircraft Type", 
                     "Wake Turbulence Category", "Date Hour", "Year", "Month", 
                     "Day", "Hour", "Date", "Time", "Period of the Day", 
                     "Weekday")]