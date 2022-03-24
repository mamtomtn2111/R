library(quantmod)
library(dplyr)
library(scales)
library(ggplot2)
library(ggthemes)

#collecting data for apple (AAPL)
apple <- getSymbols("AAPL", return.class = "data.frame",
                    from = "2020-03-01")

apple <- AAPL %>% 
  mutate(Date = as.Date(row.names(.))) %>% 
  select(Date, AAPL.Close) %>% 
  rename(Close = AAPL.Close) %>% 
  mutate(Company = "Apple")

facebook <- getSymbols("FB", return.class = "data.frame",
                       from = "2020-03-01")

facebook <- FB %>% 
  mutate(Date = as.Date(row.names(.))) %>% 
  select(Date, FB.Close) %>% 
  rename(Close = FB.Close) %>% 
  mutate(Company = "Facebook")
rbind(apple, facebook) -> data_timeseries

#Visualization
ggplot(data = data_timeseries, mapping = aes(x = Date, y = Close,
                                             color = Company)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "1 month", labels = date_format("%b-%y")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1 )) +
  scale_y_continuous(limits = c(100, 400), breaks = seq(100, 400, 50),
                     labels = dollar) +
  labs(title = "NASDAQ Closing Prices",
       subtitle = "From 2020 to 2022",
       caption = "Source: Yahoo finace",
       x = "",
       y = "Closing Prices") +
  scale_color_brewer(palette = "Set1") +
  theme_economist()



