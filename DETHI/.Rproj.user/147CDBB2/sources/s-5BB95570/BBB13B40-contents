library(ggplot2)
library(dplyr)
library(scales)
data("economics")

#Visualize with line graphs
ggplot(data = economics, mapping = aes(x = date, y = psavert)) +
  geom_line() +
  labs(title = " Personal Savings rate",
       x = "Data",
       y = "Personal Savings rate")

#Decorate
ggplot(data = economics, mapping = aes(x = date, y = psavert)) +
  geom_line(color = "indianred3", size = 1) +
  geom_smooth()+
  scale_x_date(date_breaks = "5 years", labels = date_format("%b-%y"))+
  labs(title = " Personal Savings rate",
       x = "Data",
       y = "Personal Savings rate") +
  theme_minimal()