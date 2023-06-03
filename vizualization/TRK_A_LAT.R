library(leaflet)
library(dplyr)

track_longitude <- as.numeric(pull(LPPT_TRK, longitude))
track_latitude <- as.numeric(pull(LPPT_TRK, latitude))

LPPT_TRK_LAT <- data.frame(track_latitude, track_longitude)

  TRK_A_LAT <- leaflet() %>%
    addTiles() %>%
    setView(lng = -9.1393, lat = 38.7223, zoom = 10)  # Set the initial map view

  TRK_A_LAT <- addPolylines(TRK_A_LAT, data = LPPT_TRK_LAT, lng = ~track_longitude, lat = ~track_latitude, color = "red")

TRK_A_LAT