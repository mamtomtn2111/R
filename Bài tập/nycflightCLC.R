library(dplyr)
library(nycflights13)
library(ggplot2)

data(flights)
#Câu 1
flights %>% 
  filter(day == 1 & month == 1 & year == 2013) -> flightsFirstDayOf2013
nrow(flightsFirstDayOf2013)


#
flights %>%
  group_by(month, day) %>% 
  summarise(Total_flight = n()) %>% 
  arrange(desc(Total_flight)) %>% 
  head(1)

#max(GroupByDayMonth$n)



