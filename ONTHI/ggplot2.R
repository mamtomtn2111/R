library(ggplot2)
library(scales)
library(mosaicData)
library(dplyr)
data("Marriage")

table(Marriage$race)
ggplot(data = Marriage, mapping = aes(x = race)) +
  geom_bar(fill = "#00adef", color = "black") +
  labs(title = "Participants by race",
       subtitle = "THe different",
       caption = "Source:abc",
       x = "Race",
       y = "Frequency")

#Barchart with percent
ggplot(data = Marriage, mapping = aes(x = race, y = ..count../sum(..count..))) +
  geom_bar(fill = "#00adef", color = "black") +
  scale_y_continuous(labels = percent) +
  labs(title = "Participants by race",
       subtitle = "THe different",
       caption = "Source: http://mosaica-web.org",
       x = "race",
       y = "Percent")

#Make the order in ascending or decending by reorder(x-value, n) or - n
plotdata <- Marriage  %>%
  count(race) %>% 
  mutate(pct = n/sum(n), pct_label = paste0(round(pct*100),"%"))

ggplot(data = plotdata, mapping = aes(x = reorder(race, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#00adef", color = "black") +
  labs(title = "Participants by race",
       subtitle = "different",
       caption = "Souce:http://mosaica-web.org",
       x = "race",
       y = "frequency") -> p1

p1

p1 + geom_text(aes(label = n), vjust = -0.5)

ggplot(data = plotdata, mapping = aes(x = reorder(race, -pct), y = pct)) +
  geom_bar(stat = "identity", fill = "#00adef", color = "black") +
  geom_text(aes(label = pct_label), vjust = -0.5) +
  scale_y_continuous(labels = percent, breaks = seq(0,1, 0.2)) +
  labs(title = "Participants by race",
       subtitle = "different",
       caption = "Souce:http://mosaica-web.org",
       x = "race",
       y = "frequency") -> p2
p2

mydata <- CPS85
plotdata <- filter(mydata, wage <= 40)
ggplot(data = plotdata, mapping = aes(x = exper, y = wage, color = sex)) +
  geom_point(alpha = 0.7, size = 2) -> p2

p2 + geom_smooth(method = "lm", size = 1.2, se = TRUE) -> p2
p2 + 
  scale_x_continuous(breaks = seq(0, 70, 10)) +
  scale_y_continuous(breaks = seq(0, 30 , 5), labels = dollar) +
  scale_color_manual(values = c("indianred3", "cornflowerblue")) -> p2
p2

#count sector
plotdata <- mydata  %>%
  count(educ) %>% 
  mutate(pct = n/sum(n), pct_label = paste0(round(pct*100,1),"%"))

ggplot(data = plotdata, mapping = aes(x = reorder(educ, pct), y = pct)) +
  geom_bar(stat = "identity", fill = "#00adef", color = "black") +
  geom_text(aes(label = pct_label), vjust = -0.5) +
  scale_y_continuous(labels = percent, breaks = seq(0,1, 0.2)) +
  labs(title = "Participants by sector",
       subtitle = "different",
       caption = "Souce:http://mosaica-web.org",
       x = "sector",
       y = "Number of employee") -> p2
p2

#p2 + facet_wrap()

plotdata2 <- mydata %>% 
  filter(educ >= 12) %>% 
  count(sector) %>% 
  mutate(pct = n/sum(n), pct_label = paste0(round(pct*100,1),"%"))

plotdata <- filter(mydata, educ == 12)
ggplot(data = plotdata, mapping = aes(x = sector, fill = sex)) +
  geom_bar(position = position_dodge(preserve = "single"))
ggplot(data = plotdata, mapping = aes(x = sector, fill = sex)) +
  geom_bar(position = "stack")

ggplot(data = plotdata2, mapping = aes(x = reorder(sector, pct), y = pct)) +
  geom_bar(stat = "identity", fill = "#00adef", color = "black") +
  geom_text(aes(label = pct_label), vjust = -0.5) +
  scale_y_continuous(labels = percent, breaks = seq(0,1, 0.2)) +
  labs(title = "Participants by sector with high education",
       subtitle = "different",
       caption = "Souce:http://mosaica-web.org",
       x = "sector",
       y = "Number of employee")
