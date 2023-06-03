library(tidyverse)
library(dplyr)
library(ggplot2)

###### Arrivals by Month Graph ################################################

arr_Month <- LPPT_A %>%
  mutate(`Wake Turbulence Category` = if_else(is.na(`Wake Turbulence Category`), 
                                              "Undefined", `Wake Turbulence Category`)) %>%
  group_by(Month, `Wake Turbulence Category`) %>%
  summarize(num_flights = n())%>%
  mutate(`Wake Turbulence Category` = factor(`Wake Turbulence Category`, levels = c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")))

wtc_order <- c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")

GRAPH_A_Month <- ggplot(arr_Month, 
                        aes(x = factor(Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")), 
                            y = num_flights, 
                            fill = `Wake Turbulence Category`)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(breaks = wtc_order,
                    values = c("#00008B", "#FFA500", "#00FF00", "#FFFF00", "#999999")) +
  labs(title = "Flights per Month in LPPT - 2022", 
       x = "Month", 
       y = "Number of flights", 
       fill = "Wake Turbulence Category:") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

plot(GRAPH_A_Month)

rm(arr_Month)

###### Arrivals by Weekday Graph ################################################

arr_weekday <- LPPT_A %>%
  mutate(`Wake Turbulence Category` = if_else(is.na(`Wake Turbulence Category`), 
                                              "Undefined", `Wake Turbulence Category`)) %>%
  group_by(Weekday, `Wake Turbulence Category`) %>%
  summarize(num_flights = n())%>%
  mutate(`Wake Turbulence Category` = factor(`Wake Turbulence Category`, levels = c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")))

wtc_order <- c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")

GRAPH_A_Weekday <- ggplot(arr_weekday, 
                          aes(x = factor(Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), 
                              y = num_flights, 
                              fill = `Wake Turbulence Category`)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(breaks = wtc_order,
                    values = c("#00008B", "#FFA500", "#00FF00", "#FFFF00", "#999999")) +
  labs(title = "Arrivals per Weekday in LPPT - 2022", 
       x = "Weekday", 
       y = "Number of flights", 
       fill = "Wake Turbulence Category:") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

plot(GRAPH_A_Weekday)

rm(arr_weekday)

###### Arrivals by Period of the day Graph ####################################

arr_period <- LPPT_A %>%
  mutate(`Wake Turbulence Category` = if_else(is.na(`Wake Turbulence Category`), 
                                              "Undefined", `Wake Turbulence Category`)) %>%
  group_by(`Period of the Day`, `Wake Turbulence Category`) %>%
  summarize(num_flights = n()) %>%
  mutate(`Wake Turbulence Category` = factor(`Wake Turbulence Category`, 
                                             levels = c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")))

wtc_order <- c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")

GRAPH_A_PeriodDay <- ggplot(arr_period, 
                            aes(x = factor(`Period of the Day`, levels = c("Dawn", "Morning", "Afternoon", "Night")), 
                                y = num_flights, 
                                fill = `Wake Turbulence Category`)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(breaks = wtc_order,
                    values = c("#00008B", "#FFA500", "#00FF00", "#FFFF00", "#999999")) +
  labs(title = "Arrivals per Period of the Day in LPPT - 2022", 
       x = "Period of the Day", 
       y = "Number of flights", 
       fill = "Wake Turbulence Category:") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

plot(GRAPH_A_PeriodDay)

rm(arr_period)

###### Arrivals by Hour of the day Graph ######################################

arr_hour <- LPPT_A %>%
  mutate(`Wake Turbulence Category` = if_else(is.na(`Wake Turbulence Category`), 
                                              "Undefined", `Wake Turbulence Category`)) %>%
  group_by(Hour, `Wake Turbulence Category`) %>%
  summarize(num_flights = n())%>%
  mutate(`Wake Turbulence Category` = factor(`Wake Turbulence Category`, 
                                             levels = c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")))

wtc_order <- c("Heavy", "Medium", "Light/Medium", "Light", "Undefined")

GRAPH_A_Hour <- ggplot(arr_hour, 
                       aes(x = Hour, 
                           y = num_flights, 
                           fill = `Wake Turbulence Category`)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(breaks = wtc_order,
                    values = c("#00008B", "#FFA500", "#00FF00", "#FFFF00", "#999999")) +
  labs(title = "Arrivals per Hour in LPPT - 2022", 
       x = "Hours of the Day", 
       y = "Number of flights", 
       fill = "Wake Turbulence Category:") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

plot(GRAPH_A_Hour)

rm(arr_hour)