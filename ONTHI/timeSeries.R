library(ggplot2)
library(scales)
library(dplyr)

data("economics")
economics %>% head()

#Mo phong du lieu kinh te My
ggplot(data = economics, mapping = aes(x = date, y = psavert)) +
  geom_line() +
  labs(title = "Personal Savings Rate",
       x = "Date",
       y = "Personal Savings Rate")

ggplot(data = economics, mapping = aes(x = date, y = psavert)) +
  geom_line(color = "indianred3", size = 1) +
  geom_smooth() +
  scale_x_date(date_breaks = "5 years", labels = date_format("%b-%y")) +
  labs(title = "Personal Savings Rate",
       subtitle = "From 1967 to 2015",
       x = "",
       y = "Personal Savings Rate") +
  theme_minimal()


#So sanh du lieu chung khoan
library(quantmod)
# Take data for Apple
apple <- getSymbols("AAPL", return.class = "data.frame",
                    from = "2021-05-05")

#Thực hien format row. thanh date
apple <- AAPL %>%
  mutate(Date = as.Date(row.names(.),format = "X%Y.%m.%d")) %>%
  select(Date, AAPL.Close) %>%
  rename(Close = AAPL.Close) %>%
  mutate(Company = "Apple")

# facebook
facebook <- getSymbols("FB", return.class = "data.frame",
                       from = "2021-05-05")
facebook <- FB %>%
  mutate(Date = as.Date(row.names(.),format = "X%Y.%m.%d")) %>%
  select(Date, FB.Close) %>%
  rename(Close = FB.Close) %>%
  mutate(Company = "Facebook")

data_series <- rbind(apple, facebook)

ggplot(data = data_series, mapping = aes(x = Date, y = Close,
                                         color = Company)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "1 month",
               label = date_format("%b-%Y")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(100, 450), breaks = seq(100,400,50),
                     labels = dollar) +
  labs(title = "NASDAQ Closing Prices",
       subtitle = "From May 2021 to February 2022",
       caption = "source: Yahoo Finance",
       x = "",
       y = "Closing Price") +
  scale_color_brewer(palette = "Set1")


#Dumbell chart
library(ggalt)
library(tidyr)
library(gapminder)
data(gapminder)
# filter data thanh long
plotdata_long <- filter(gapminder, continent == "Asia" &
                          year %in% c(1952, 2007)) %>%
  select(country, year, lifeExp)
# Convert thanh data dang wide
plotdata_wide <- spread(plotdata_long, year, lifeExp)
names(plotdata_wide) <- c("Country", "year1952", "year2007")
# Initial dumbell chart
ggplot(data = plotdata_wide, mapping = aes(y = Country,
                                           x = year1952,
                                           xend = year2007)) +
  geom_dumbbell()
#Part2
ggplot(data = plotdata_wide,
       mapping = aes(y = reorder(Country, year1952),
                     x = year1952,
                     xend = year2007)) +
  geom_dumbbell(size = 1.2, size_x = 3, size_xend = 3,
                colour = "grey", colour_x = "blue",
                colour_xend = "red") +
  theme_minimal() +
  labs(title = "Change in Life Expectancy",
       subtitle = "From 1952 to 2007",
       x = "Life Expectancy (years)",
       y = "")
