library(ggplot2)
library(carData)
library(gapminder)
library(scales)
data("Salaries")

ggplot(data = Salaries, mapping = aes(x = salary)) +
  geom_histogram(fill = "cornflowerblue", color = "white") +
  facet_wrap(~ rank, nrow = 1) +
  labs(title = "Salary by rank with faceting", x = "", y = "") +
  scale_x_continuous(labels = dollar)

ggplot(data = Salaries, mapping = aes(x = salary)) +
  geom_histogram(fill = "cornflowerblue", color = "white") +
  facet_grid(rank ~ sex) +
  labs(title = "Salary by rank with faceting", x = "", y = "") +
  scale_x_continuous(labels = dollar)

library(dplyr)
plotdata <- Salaries %>% 
  group_by(sex, rank, discipline) %>% 
  summarize(n = n(),
            mean = mean(salary),
            sd = sd(salary),
            se = sd/sqrt(n))

ggplot(data = plotdata, mapping = aes(x = sex, y = mean, color = sex)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, max = mean + se), width = 0.1) +
  scale_y_continuous(breaks = seq(7000, 14000, 10000), label = dollar) +
  facet_grid(. ~ rank + discipline) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none") +
  labs(x = "", y = "",
       title = "Academic salary by gender, discipline, rank",
       subtitle = "Means and Standard Error",
       caption = "Https://www.dataset.org-vn") +
  scale_color_brewer(palette = "Set1")

