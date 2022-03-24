#install.packages("gapminder")
library(gapminder)
library(ggplot2)
library(dplyr)

names(gapminder)
dim(gapminder)
??gapminder

#The relationshop between lLifeExp and GDPPerCap
ggplot(data = gapminder, mapping = aes(x = gdpPercap, y=lifeExp, color = continent))+
  geom_point(aes(color = "continent")) +
  scale_x_log10() +
  geom_smooth(method = "loess")

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y=lifeExp))+
  geom_point(aes(color = "continent")) +
  scale_x_log10() +
  geom_smooth(method = "loess")