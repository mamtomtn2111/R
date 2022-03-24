library(leaflet)
library(dplyr)
leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = 105.836262, lat = 21.593674,
             popup = "welcome to my house, contact me")

#using the plotly to make the interactive graph
library(ggplot2)
library(plotly)
data(mpg, package = "ggplot2")
p <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = class)) +
  geom_point(size = 3, alpha = 0.6) +
  labs(title = "Engine Display by class",
       x = "Engine Displacement",
       y = "Highway Mileage per gallon",
       caption = "www.mpg-org.us") +
  theme_bw()

ggplotly(p)

library(highcharter)
library(gapminder)

data("gapminder")
asia <- gapminder %>% 
  filter(continent == "Asia") %>% 
  select(year, country, lifeExp)

library(tidyr)
plotdata <- spread(asia, country, lifeExp)

highchart() %>% 
  hc_xAxis(categories = plotdata$year) %>% 
  hc_add_series(name = "Vietnam", data = plotdata$Vietnam) %>% 
  hc_add_series(name = "China", data = plotdata$China) %>% 
  hc_add_series(name = "Iran", data = plotdata$Iran) %>% 
  hc_add_series(name = "Cambodia", data = plotdata$Cambodia) %>% 
  hc_add_series(name = "Japan", data = plotdata$Japan) %>% 
  hc_add_series(name = "Myanmar", data = plotdata$Myanmar) -> plot1

plot1

plot1 <- plot1 %>% 
  hc_title(text = "Life expectancy by country in asia",
           margin = 30, align = "center",
           style = list(color = "stellblue", fontWeight = "bold")) %>% 
  hc_subtitle(text = "From 1952 to 2007", margin = 20, align = "left",
              style = list(color = "red", fontWeight = "bold")) %>% 
  hc_credits(enabled = TRUE, text = "Gapminder data",
             href = "http://gapminder.com") %>% 
  hc_legend(align = "left", layout = "vertical",
            x = 0, y = 100, verticalAlign = "top") %>% 
  hc_tooltip(crosshairs = TRUE, backgroundColors = "#FCFFC5",
             shared = TRUE, borderWidth = 4) %>% 
  hc_exporting(enabled = TRUE)

plot1

#Sử dụng highchart để tạo biểu đồ so sánh gdp của châu âu
#Germany, france, Russia, Spain Russia, Poland, Belgium, England

europe <- gapminder %>% 
  filter(continent == "Europe") %>% 
  select(year, country, gdpPercap)

plotdata <- spread(europe, country, gdpPercap)


highchart() %>% 
  hc_xAxis(categories = plotdata$year) %>% 
  hc_add_series(name = "Germany", data = plotdata$Germany) %>% 
  hc_add_series(name = "France", data = plotdata$France) %>% 
  hc_add_series(name = "England", data = plotdata$`United Kingdom`) %>% 
  hc_add_series(name = "Spain", data = plotdata$Spain) %>% 
  hc_add_series(name = "Denmark", data = plotdata$Denmark) %>% 
  hc_add_series(name = "Begium", data = plotdata$Belgium) -> plot1

plot1 <- plot1 %>% 
  hc_title(text = "GDP per cap by country in asia",
           margin = 30, align = "center",
           style = list(color = "stellblue", fontWeight = "bold")) %>% 
  hc_subtitle(text = "From 1952 to 2007", margin = 20, align = "left",
              style = list(color = "red", fontWeight = "bold")) %>% 
  hc_credits(enabled = TRUE, text = "Gapminder data",
             href = "http://gapminder.com") %>% 
  hc_legend(align = "left", layout = "vertical",
            x = 0, y = 100, verticalAlign = "top") %>% 
  hc_tooltip(crosshairs = TRUE, backgroundColors = "#FCFFC5",
             shared = TRUE, borderWidth = 4) %>% 
  hc_exporting(enabled = TRUE)

plot1


