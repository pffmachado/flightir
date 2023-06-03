library(tidyverse)
library(dplyr)
library(ggplot2)

###### Departures by Month Graph ##############################################

dep_Month <- LPPT_D %>%
  mutate(`Wake Turbulence Category` = if_else(is.na(`Wake Turbulence Category`), 
                                              "Undefined", `Wake Turbulence Category`)) %>%
  group_by(Month, `Wake Turbulence Category`) %>%
  summarize(num_flights = n())%>%
  mutate(`Wake Turbulence Category` = factor(`Wake Turbulence Category`, levels = c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")))

wtc_order <- c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")

GRAPH_D_Month <- ggplot(dep_Month, 
                        aes(x = factor(Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")), 
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

plot(GRAPH_D_Month)

rm(dep_Month)

###### Departures by Weekday Graph ############################################

dep_weekday <- LPPT_D %>%
  mutate(`Wake Turbulence Category` = if_else(is.na(`Wake Turbulence Category`), 
                                              "Undefined", `Wake Turbulence Category`)) %>%
  group_by(Weekday, `Wake Turbulence Category`) %>%
  summarize(num_flights = n())%>%
  mutate(`Wake Turbulence Category` = factor(`Wake Turbulence Category`, levels = c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")))

wtc_order <- c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")

GRAPH_D_Weekday <- ggplot(dep_weekday, 
                          aes(x = factor(Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), 
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

plot(GRAPH_D_Weekday)

rm(dep_weekday)

###### Departures by Period of the Day Graph ##################################

dep_period <- LPPT_D %>%
  mutate(`Wake Turbulence Category` = if_else(is.na(`Wake Turbulence Category`), 
                                              "Undefined", `Wake Turbulence Category`)) %>%
  group_by(`Period of the Day`, `Wake Turbulence Category`) %>%
  summarize(num_flights = n()) %>%
  mutate(`Wake Turbulence Category` = factor(`Wake Turbulence Category`, 
                                             levels = c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")))

wtc_order <- c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")

GRAPH_D_PeriodDay <- ggplot(dep_period, 
                            aes(x = factor(`Period of the Day`, levels = c("Dawn", "Morning", "Afternoon", "Night")), 
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

plot(GRAPH_D_PeriodDay)

rm(dep_period)

###### Departures by Hour of the Day Graph ##################################

dep_hour <- LPPT_D %>%
  mutate(`Wake Turbulence Category` = if_else(is.na(`Wake Turbulence Category`), 
                                              "Undefined", `Wake Turbulence Category`)) %>%
  group_by(Hour, `Wake Turbulence Category`) %>%
  summarize(num_flights = n())%>%
  mutate(`Wake Turbulence Category` = factor(`Wake Turbulence Category`, 
                                             levels = c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")))

wtc_order <- c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")

GRAPH_D_Hour <- ggplot(dep_hour, 
                       aes(x = Hour, 
                           y = num_flights, 
                           fill = `Wake Turbulence Category`)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(breaks = wtc_order,
                    values = c("#00008B", "#FFA500", "#00FF00", "#FFFF00", "#999999")) +
  labs(title = "Departures per Hour in LPPT - 2022", 
       x = "Hours of the Day", 
       y = "Number of flights", 
       fill = "Wake Turbulence Category:") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

plot(GRAPH_D_Hour)

rm(dep_hour)