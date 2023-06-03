library(ggplot2)
library(sf)
library(rnaturalearth)
library(tidyverse)
library(dplyr)

###### Departing from LPPT by Dest. Airport for Europe and Northern Africa ####

world <- ne_countries(scale = "medium", returnclass = "sf")

Europe <- world[which(world$continent %in% c("Europe", "Africa", "Asia")),]

MAP_D_EU_Airport <- ggplot(Europe) +
  geom_sf() +
  coord_sf(xlim = c(-30,40), ylim = c(20,75), expand = FALSE) + # Argument that defines the European Continent
  geom_point(data = DF_D_Airport, aes(x=lon, y=lat, size = `Number of Flights`, color = `Number of Flights`)) +
  scale_size(range = c(1, 10), guide = guide_legend(title = "")) +
  scale_color_gradient(low = "lightblue", high = "darkblue", guide = guide_colorbar(title = "Number of Flights")) +
  labs(size = NULL, title = "Number of Flights per Destination Airport in Europe and Northern Africa - 2022") + 
  theme(plot.title = element_text(hjust = 0.5))

plot(MAP_D_EU_Airport)

rm(world, Europe)

###### Departures from LPPT by Dest. Country for Europe and Northern Africa ###

world <- ne_countries(scale = "medium", returnclass = "sf")

world <- world %>% rename("Destination Country" = sovereignt)

eu_d_map <- merge(world, DF_D_Country, by.x = "Destination Country", all.x = TRUE)

MAP_D_EU_Country <- ggplot() +
  geom_sf(data = eu_d_map, aes(fill = `Number of Flights`)) +
  coord_sf(xlim = c(-30,40), ylim = c(20,75), expand = FALSE) + # Argument that defines the European Continent
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_void() +
  labs(size = NULL, title = "Number of Flights per Destination Country in Europe - 2022") + 
  theme(plot.title = element_text(hjust = 0.5))

plot(MAP_D_EU_Country)

rm(world, eu_d_map)

###### Departures from LPPT by Dest. Country Worldwide ########################