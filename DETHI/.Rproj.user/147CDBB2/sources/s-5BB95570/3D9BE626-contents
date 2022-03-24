library(ggplot2)
library(dplyr)
library(carData)
library(scales)

data("Salaries")
#names(Salaries)
ggplot(data = Salaries, mapping = aes(x = yrs.since.phd, y = salary, color = rank)) +
  geom_point() +
  labs(title = "Academic salaries by rank")

#Show the visualization by gender
ggplot(data = Salaries, mapping = aes(x = yrs.since.phd, y = salary, color = rank,
                                      shape = sex)) +
  geom_point(size = 3, alpha = 0.6) +
  labs(title = "Academic salaries by rank")

#Show bubble by yrs service
ggplot(data = Salaries, mapping = aes(x = yrs.since.phd, y = salary, color = rank,
                                      size = yrs.service)) +
  geom_point(size = 3, alpha = 0.6) +
  labs(title = "Academic salaries by rank")

#creating the linear regression or plynomial regression
ggplot(data = Salaries, mapping = aes(x = yrs.since.phd, y = salary, color = sex)) +
  geom_point(size = 3, alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE, formula =  y ~ poly(x, 2), size = 1.5)  +
  labs(x = "years Since Ph.D",
       title = "Academic salary by Sex and Years Experience",
       caption = "Https//:www.salary.org",
       y = "",
       color = "Sex") +
  scale_y_continuous(label = dollar) +
  scale_color_brewer(palette = "Set1")


# Method 2: Faceting
ggplot(data = Salaries, mapping = aes(x = salary)) +
  geom_histogram(fill = "cornflowerblue", color = 'white') +
  facet_wrap(~ rank, ncol = 1)

ggplot(data = Salaries, mapping = aes(x = salary)) +
  geom_histogram(fill = "cornflowerblue", color = 'white') +
  facet_grid(sex ~ rank) +
  labs(title = "Salary by rank with faceting", x = "", y = "") +
  scale_x_continuous(label = dollar)

library(gapminder)
data(gapminder)
plotdata <- gapminder %>% 
  filter(continent == "Asia")

#Visualization 

ggplot(data = plotdata, mapping = aes(x = year, y = lifeExp)) +
  geom_line(color = "grey") +
  geom_point(color = "blue") +
  facet_wrap(~ country) +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Changes in Life expectancy",
       X = "Year",
       y = "Life Expectancy")






