library(ggalt)
library(ggplot2)
library(gapminder)
library(dplyr)
library(tidyr)

data(gapminder)

plotdata_long <- filter(gapminder, 
                   continent == "Asia" & year %in% c(1952,2007)) %>% 
  select(country, year, lifeExp)

# Convert from longdata to wide data
plotdata_wide <- spread(plotdata_long, year, lifeExp)
names(plotdata_wide) <- c("Country", "Year1952", "Year2007")

#Make the dumbell chart
ggplot(data = plotdata_wide, mapping = aes(y = reorder(Country, Year1952), x = Year1952,
                                           xend = Year2007)) +
  geom_dumbbell(size = 1.3, size_x = 3, size_xend = 3,
                colour_x = "blue", colour_xend = "red",
                colour = "grey") +
  labs(title = "Change in life Expectancy",
       subtitle = "From 1952 to 2007",
       caption = "source: www.gapminder-org.us",
       y = "",
       x = "Life expectancy (years)") -> p1
  #theme_economist()


#GDP PERCAP
plotdata_long <- filter(gapminder, 
                        continent == "Europe" & year %in% c(1952,2007)) %>% 
  select(country, year, gdpPercap)

# Convert from longdata to wide data
plotdata_wide <- spread(plotdata_long, year, gdpPercap)
names(plotdata_wide) <- c("Country", "Year1952", "Year2007")

#Make the dumbell chart
ggplot(data = plotdata_wide, mapping = aes(y = reorder(Country, Year1952), x = Year1952,
                                           xend = Year2007)) +
  geom_dumbbell(size = 1.3, size_x = 3, size_xend = 3,
                colour_x = "green", colour_xend = "cornflowerblue",
                colour = "grey") +
  labs(title = "Change in GDP/person",
       subtitle = "From 1952 to 2007",
       caption = "source: www.gapminder-org.us",
       y = "",
       x = "GDP (USD)") -> p2
#theme_economist()

library(gridExtra)
grid.arrange(p1, p2, nrow = 1)
