#Họ tên: Nguyễn Vũ hải
#Mã SV: DTC195480201CLC0006
#Lớp CNTT K18 CLC

#Bài 1
data("iris")
Setosa <-  iris %>% 
  filter(Species == "setosa") %>% 
  arrange(Sepal.Length)

distributionSetosa <- seq(
  from = min(Setosa$Sepal.Length), 
  to = max(Setosa$Sepal.Length), 
  length = 100)

SetosaMean <- mean(Setosa$Sepal.Length)
SetoSastd <- sd(Setosa$Sepal.Length)
distributionY <- dnorm(
  x = distributionSetosa,
  mean = SetosaMean, 
  sd = SetoSastd)

plot(density(Setosa$Sepal.Length))
lines(
  x = distributionX,
  y = distributionY,
  col = "red")

#Bài 2
library(gapminder)
library(dplyr)
library(ggplot2)
data("gapminder")

VietNam <-  gapminder %>% 
  filter(country == "Vietnam")

#Tuổi tác
#LineChart
C1 <-  ggplot(data = VietNam, mapping = aes(x = year, y=lifeExp))+
  geom_line(color = "red") +
  labs(title = "Changes in Life expectancy VietNam",
       X = "Year",
       y = "Life Expectancy")

#PointChart
C2 <- ggplot(data = VietNam, mapping = aes(x = year, y=lifeExp))+
  geom_point(color = "red") +
  labs(title = "Changes in Life expectancy VietNam",
       X = "Year",
       y = "Life Expectancy")

#Thu nhập bình quân
#LineChart
C3 <- ggplot(data = VietNam, mapping = aes(x = year, y=gdpPercap))+
  geom_line(color = "red") +
  labs(title = "Changes in Life GDPpercap VietNam",
       X = "Year",
       y = "Dollar")

#PointChart
C4 <-  ggplot(data = VietNam, mapping = aes(x = year, y=gdpPercap))+
  geom_point(color = "red") +
  labs(title = "Changes in Life GDPpercap VietNam",
       X = "Year",
       y = "Dollar")

#Ghép chung biểu đồ

library("gridExtra")
grid.arrange(C1, C3,
             ncol=2, nrow=2, widths=c(4, 4), heights=c(4, 4))

ggplot(data = VietNam, mapping = aes(x = year, y=c(gdpPercap,lifeExp)))+
  geom_line(color = "red") +
  labs(title = "Changes in Life expectancy VietNam",
       X = "Year",
       y = "Dollar")

