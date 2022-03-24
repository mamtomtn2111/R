#Nguyễn Vũ Hải

library(dplyr)
library(ggplot2)
library(gapminder)

# Lấy dữ liệu từ gapminder
data("gapminder")
# Thống kê 10 dòng đầu dữ liệu, 10 dòng cuối dữ liệu
head(gapminder, n = 10)
tail(gapminder, n = 10)

# Xem chiều dữ liệu
dim(gapminder)

#Xem chiều cấu trúc biến
str(gapminder)

# Thống kê tổng quan
summary(gapminder)

# Thống kê số lượng các quốc gia từng châu lục bằng table
table(gapminder$continent)

#Thao tác với DLYPER
#Lọc dữ liệu ở việt nam
gapminder %>% filter(
  continent == "Asia",
  country == "Vietnam",
  year %in% c(1997,2002,2007)
)

#Thông kê với summarize ở việt nam
gapminder %>% filter(
    year == 2007,
    continent == "Asia",
    country == "Vietnam"
  ) %>%
  summarise(mean(lifeExp))

#Kết hợp groupby để thông kê ra trung bình năm kinh nghiệm làm việc các nước năm 2007 
gapminder %>% 
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(average_lifeExp = mean(lifeExp))

#Sử dụng hàm groupby theo châu lục và summerise theo tổng số dân, cuối cùng là
#Sắp xếp giảm
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(total_pop = sum(pop)) %>%
  arrange(desc(total_pop))

#mutate thêm cột totalGDP bằng tổng GPT các nước
gapminder %>%
  filter(year == 2007) %>%
  mutate(totalGDP = gdpPercap * pop) %>%
  head(n = 10)

# Lọc dữ liệu cho năm 2007
gapminder2007 <- gapminder %>%
  filter(year == 2007)
gapminder2007 %>%
  head(n=10)

# Mô phỏng trực quan theo tham số gdp theo đầu người và kinh nghiệm
ggplot(data = gapminder2007, mapping = aes(x = gdpPercap, 
                                           y = lifeExp, color = continent)) +
  geom_point() +
  scale_x_log10()

# Thể hiện bubble chart theo quy mô dân số
ggplot(data = gapminder2007,
       mapping = aes(x = gdpPercap,y = lifeExp,
                     color = continent, size = pop)) +
  geom_point() +
  scale_x_log10()


# Vẽ đường hồi quy với geom_smooth.
# Tham số method = "loess" để chỉ ra sử dụng phương pháp bình phương tối thiểu LMSE
ggplot(data = gapminder,
       mapping = aes(x = gdpPercap, y = lifeExp, size = pop)) +
  geom_point(aes(color = continent)) +
  geom_smooth(method = "loess") +
  scale_x_log10()


#Gán nhãn cho biểu đồ
ggplot(data = gapminder,
       mapping = aes(x = gdpPercap, y = lifeExp, size = pop)) +
  geom_point(aes(color = continent)) +
  geom_smooth(method = "loess") +
  scale_x_log10() +
  labs(x =" Log GDP per Capita", y = "Life Expectancy") +
  ggtitle("Association between GDP and LifeExp") +
  theme(plot.title = element_text(lineheight = 0.8,
                                  face = "bold", hjust = 0.5))

# Phong cách tạp chí The Economist
#install.packages("ggthemes")
library(ggthemes)
ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent)) +
  geom_smooth(method = "loess") +
  scale_x_log10() +
  labs(x = "Log GDP per Capita", y = "Life Expectancy") +
  ggtitle("Association between GDP Per Capita and Life Expectancy") + 
  theme(plot.title = "", face = "bold", hjust = 0.5) +
  theme_economist()


#Histogram mô tả phân phối GDP
gapminder %>%
  filter(year == 2007) -> gapminder2007
gapminder2007 %>% head(n=10)

#Vẽ biểu đồ histogram
ggplot(data = gapminder2007, mapping = aes(gdpPercap)) +
  geom_histogram(fill = "#00adef", color = "red", bins = 20) +
  labs(title = "Distribution of GDP per Capita in 2007", y = "Frequency")

#Vẽ biểu đồ histogram theo %
library(scales)
ggplot(data = gapminder2007, mapping = aes(x = gdpPercap, y = ..count../sum(..count..))) +
  geom_histogram(fill = "#00adef", color = "red", bins = 20) +
  scale_y_continuous(labels = percent) +
  labs(title = "Distribution of GDP per Capita in 2007", y = "Frequency")

# Biểu đồ mật độ xác suất thông thường
ggplot(data = gapminder2007, mapping = aes(gdpPercap, fill = continent)) +
  geom_density(alpha = 0.7)

# Biểu đồ Ridgeplot, khắc phục những điểm yếu của biểu đồ ggplot trên
#install.packages("ggridges")
library(ggridges)
ggplot(data = gapminder2007, aes(x = gdpPercap, y = continent, fill = continent)) +
  geom_density_ridges(alpha = 0.7) +
  theme_ridges() +
  labs("RidgePlot for GDPPerCap") +
  theme(legend.position = "none")

# Lọc ra dữ liệu các quốc gia ở châu Á
asia <- gapminder %>%
  filter(continent == "Asia" & year == 2007)
asia %>%
  head(n=10)

# Lọc ra dữ liệu cho các quốc gia ở châu Âu
europe <- gapminder %>%
  filter(continent == "Europe" & year == 2007)
europe %>%
  head(n=10)

# Trực quan dữ liệu châu Á để so sánh
ggplot(data = asia, mapping = aes(x = country, y = lifeExp, fill = country)) +
  geom_bar(stat = "identity", width = 0.9) +
  coord_flip()

# Sắp xếp lại trật tự cho châu Á và gán cho đối tượng là graph1
ggplot(data = asia, mapping = aes(x = reorder(country, lifeExp), y = lifeExp, fill = country)) +
  geom_bar(stat = "identity", width = 0.9) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x="", y="Life Expectancy of Asia") -> graph1
graph1


# Tương tự với châu Âu gán cho đối tượng là graph2
ggplot(data = europe, mapping = aes(x = reorder(country, lifeExp), y = lifeExp, fill = country)) +
  geom_bar(stat = "identity", width = 0.9) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x="", y="Life Expectancy of Europe") -> graph2
graph2

#Hiện thị nhiều biểu đồ (Cụ thể là hai biểu đồ graph1 và graph2)
#install.packages("gridExtra")
library(gridExtra)
grid.arrange(graph1, graph2, ncol = 2)