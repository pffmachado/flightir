library(tidyverse)
library(ggplot2)

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