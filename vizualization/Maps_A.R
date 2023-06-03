library(ggplot2)
library(sf)
library(rnaturalearth)
library(tidyverse)
library(dplyr)

###### Arrivals in LPPT by Origin Airport for Europe and Northern Africa ####

world <- ne_countries(scale = "medium", returnclass = "sf")

Europe <- world[which(world$continent %in% c("Europe", "Africa", "Asia")),]

MAP_A_EU_Airport <- ggplot(Europe) +
  geom_sf() +
  coord_sf(xlim = c(-30,40), ylim = c(20,75), expand = FALSE) + # Argument that defines the European Continent
  geom_point(data = DF_A_Airport, aes(x=lon, y=lat, size = `Number of Flights`, color = `Number of Flights`)) +
  scale_size(range = c(1, 10), guide = guide_legend(title = "")) +
  scale_color_gradient(low = "lightblue", high = "darkblue", guide = guide_colorbar(title = "Number of Flights")) +
  labs(size = NULL, title = "Number of Flights per Origin Airport in Europe and Northern Africa - 2022") + 
  theme(plot.title = element_text(hjust = 0.5))

plot(MAP_A_EU_Airport)

rm(world, Europe)

###### Arrivals in LPPT by Origin Country for Europe and Northern Africa ####

world <- ne_countries(scale = "medium", returnclass = "sf")

world <- world %>% rename("Origin Country" = sovereignt)

eu_a_map <- merge(world, DF_A_Country, by.x = "Origin Country", all.x = TRUE)

MAP_A_EU_Country <- ggplot() +
  geom_sf(data = eu_a_map, aes(fill = `Number of Flights`)) +
  coord_sf(xlim = c(-30,40), ylim = c(20,75), expand = FALSE) + # Argument that defines the European Continent
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_void() +
  labs(size = NULL, title = "Number of Flights per Origin Country in Europe and Northern Africa - 2022") + 
  theme(plot.title = element_text(hjust = 0.5))

plot(MAP_A_EU_Country)

rm(world, eu_a_map)

###### Arrivals in LPPT by Origin Country Worldwide #########################

world <- ne_countries(scale = "medium", returnclass = "sf")

world <- world %>% rename("Origin Country" = sovereignt)

world_a_map <- merge(world, DF_A_Country, by.x = "Origin Country", all.x = TRUE)

MAP_A_World_Country <- ggplot() +
  geom_sf(data = world_a_map, aes(fill = `Number of Flights`)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_void() +
  labs(size = NULL, title = "Number of Flights per Origin Country Worldwide - 2022") + 
  theme(plot.title = element_text(hjust = 0.5))

plot(MAP_A_World_Country)

rm(world, world_a_map)