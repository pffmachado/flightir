library(leaflet)
library(dplyr)
library(openskyr)


selected_columns <- c("icao24", "Date Hour", "Month", "Day", "Period of the Day", 
                      "Hour", "Origin Country", "Airline", "Airline Name")
LPPT_TRK_FLIGHTS <- LPPT_A[selected_columns]
LPPT_TRK_FLIGHTS$epoch_time <- as.numeric(LPPT_TRK_FLIGHTS$`Date Hour`)


LPPT_TRK_FLIGHTS <- subset(LPPT_TRK_FLIGHTS, Month == "September")
LPPT_TRK_FLIGHTS <- subset(LPPT_TRK_FLIGHTS, Day == "15")
LPPT_TRK_FLIGHTS <- subset(LPPT_TRK_FLIGHTS, `Period of the Day` == "Afternoon")

LPPT_TRK <- data.frame()

for (i in 1:nrow(LPPT_TRK_FLIGHTS)) {
  icao24 <- LPPT_TRK_FLIGHTS[i, "icao24"]
  time <- LPPT_TRK_FLIGHTS[i, "epoch_time"]
  
  LPPT_TRK_FOR <- get_track_data(Sys.getenv("OPENSKY_USERNAME"), Sys.getenv("OPENSKY_PASSWORD"),
                               icao24 = icao24, time = time)
  
  LPPT_TRK <- rbind(LPPT_TRK, LPPT_TRK_FOR)
  
  } 

LPPT_TRK <- select(LPPT_TRK, icao24, startTime, time, latitude, longitude, baro_altitude)
LPPT_TRK <- rename(LPPT_TRK, callsign = startTime)
LPPT_TRK <- data.frame(callsign, track_longitude, track_latitude, track_altitude)

rm(LPPT_TRK_FOR)