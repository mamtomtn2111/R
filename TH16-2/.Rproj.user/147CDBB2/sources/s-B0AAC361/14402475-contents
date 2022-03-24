library(plotly)
library(dplyr)
library(ggplot2)

x <- c(1:3)
y <- c(2:4)
f <- c(1:3)
df <- data.frame(x,y,f)

figure <- df %>% 
  plot_ly(x = ~x, y = ~y, frame = ~f,
          type = "scatter", mode = "makers", showlegend = F)
figure
data("gapminder")
vietname_data <- gapminder %>% 
  filter(country == "Vietnam" | country == "China")
# Trace animations for gaminder
df1 <- gapminder 
fig1 <- vietname_data %>% 
  plot_ly(x = ~gdpPercap, y = ~lifeExp, size = ~pop,
          color = ~continent, frame = ~year,
          text = ~ country, hoverinfo = "text",
          type = "scatter", mode = "markers")
fig1 <- fig1 %>% 
  layout(xaxis = list(type= "log"))
fig1

# Configure the animation
fig1 <- fig1 %>% 
  animation_opts(frame = 1500, easing = "sin", redraw = FALSE)

fig1
