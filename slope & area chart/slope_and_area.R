library(CGPfunctions)
library(gapminder)
library(dplyr)
library(ggplot2)
library(gcookbook)
mydata <- gapminder %>%
  filter(year %in% c(1992, 1997, 2002, 2007) &
           country %in% c("Cambodia", "Vietnam", "China",
                          "Japan", "Malaysia", "Oman")) %>%
  mutate(year = factor(year), lifeExp = round(lifeExp))

# Create the slope graph
newggslopegraph(mydata, year, lifeExp, country) +
  labs(title = "Life Expectancy by Country",
       subtitle = "Asia",
       caption = "source:www.gapminder-orge")

# Create the area chart
ggplot(data = economics, mapping = aes(x = date, y = psavert)) +
  geom_area(fill = "lightblue", color = "black") +
  labs(title = "Personal Savings Rate",
       x = "Date",
       y = "Personal Savings Rate")

data("uspopage")
ggplot(data = uspopage, mapping = aes(x = Year, y = Thousands,
                                      fill = AgeGroup)) +
  geom_area() +
  labs(title = "US Population by Age",
       x = "Year",
       y = "Population in Thousands")

ggplot(data = uspopage, mapping = aes(x = Year, y = Thousands/1000,
                                      fill = forcats::fct_rev(AgeGroup))) +
  geom_area(color = "black") +
  labs(title = "US Population by Age",
       subtitle = "From 1900 to 2002",
       caption = "source: US. Census Bureau, 2003, HS-3",
       x = "Year",
       y = "Population in Thousands",
       fill = "Age Group") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()