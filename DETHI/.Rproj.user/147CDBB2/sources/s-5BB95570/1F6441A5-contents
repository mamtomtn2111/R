library(ggplot2)
library(dplyr)
library(carData)
library(scales)

data(Salaries)

ggplot(data = Salaries, mapping = aes(x = salary, y = rank)) +
  geom_point() +
  labs(title = "The scatter plot rank and salary") +
  geom_jitter()

#tạo biểu đồ hoàn chỉnh
ggplot(data = Salaries, mapping = aes(x = salary, y = factor(rank,
        labels = c("Assistant\nProfessor","Associate\nProfessor","Professor")), color = rank)) +
      geom_jitter(alpha = 0.7, size = 1.5) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "The strip plot between rank and salary",
       subtitle = "The salary for 9 months",
       x = "", y = "", caption = "www.dataset.org") +
  scale_x_continuous(label = dollar)

data("iris")

ggplot(data = Salaries, mapping = aes(x = salary, y = factor(rank,
       labels = c("Assistant\nProfessor","Associate\nProfessor","Professor")), color = rank))+
  geom_jitter(alpha = 0.7, size = 1.5) +
  theme_minimal() +
  geom_boxplot(size = 1, outlier.shape = 2, outlier.colour = "red", outlier.size = 2.5, alpha = 0.3) +
  theme(legend.position = "none") +
  labs(title = "The strip plot between rank and salary",
       subtitle = "The salary for 9 months",
       x = "", y = "", caption = "www.dataset.org") +
  scale_x_continuous(label = dollar)
  

library(gapminder)
data(gapminder)

plotdata <- gapminder %>% 
  filter(continent == "Asia" & year == 2007)

ggplot(data = plotdata, mapping = aes(x = lifeExp, y = reorder(country, lifeExp))) +
  geom_point(color = "blue", size = 2.5) +
  geom_segment(aes(x = 40,
                   xend = lifeExp,
                   y = reorder(country, lifeExp),
                   yend = reorder(country, lifeExp)),
               color = "#00a4ef") +
  labs(x = "Life expectancy (years)",
       y = "",
       title = "Life expectancy by country",
       subtitle = "Gapminder data for Asia -2007")+
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




