library(PerformanceAnalytics)
library(ggplot2)
library(mosaicData)
library(scales)
library(skimr)
library(dplyr)
library(ggfortify)
data("CPS85")
mydata <- CPS85

str(mydata)

skim(mydata, wage, exper)

#vẽ biểu đồ
ggplot(mydata, aes(x = exper, y = wage))+
  geom_point()
#loại giá trị outliner

plotdata <- mydata %>% 
  filter(wage<40)

ggplot(plotdata, aes(x = exper, y = wage, color = sex))+
  geom_point(size = 1.5, alpha = 0.4)+
  geom_smooth(method = "lm", size = 1.5, color = "red")+ 
  scale_x_continuous(breaks = seq(0, 60, 10))+
  scale_y_continuous(breaks = seq(0, 40, 5), labels = dollar) +
  scale_color_manual(values = c('blue','red')) +
  labs(title = "The relationship between wage and Exper",
       subtitle = "The current survey in USA",
       x = "Experience (year)",
       y = "Wage hourly",
       caption = "source: CPS85") +
  facet_wrap(~sector) +
  theme_minimal()

# Vẽ lại mô hình 
ggplot(plotdata, aes(x = exper, y = wage, color = sex))+
  geom_point(size = 1.5, alpha = 0.4)+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),se =FALSE,size = 1.5, color = "red")
  
#Build model 
fit <- lm(wage ~ poly(exper, 2), data = plotdata)
autoplot(fit)
summary(fit)
prediction_value <- fitted(fit)

residual_value <- residuals(fit)

ketqua  <- data.frame(plotdata$wage, prediction_value, residual_value, plotdata$exper)
names(ketqua) <- c("Wages","Predictions","Residuals","Exper")

ketqua %>% ggplot() +
  geom_point(aes(x = Exper, y = Wages),
             col = "blue", size = 2, alpha = 0.8) +
  geom_point(aes(x = Exper, y = Predictions),
             col = "red", size = 2, alpha = 0.8)

update_data <- plotdata %>% 
  select(wage, educ, exper, age) 

head(update_data)

#correlation
chart.Correlation(update_data, histogram = TRUE, method = "pearson")

fit2 <-  lm(wage ~ educ + exper + age, data = update_data)
summary(fit2)
prediction_value <- fitted(fit2)
residual_value <- residuals(fit2)
ketqua <- data.frame(plotdata$wage, prediction_value, residual_value, plotdata$exper)

names(ketqua) <- c("Wages", "Predictions", "Residuals","Exper")

#truwcj quan keet qua huan luyen

ketqua %>% ggplot() +
  geom_point(aes(x = Exper, y = Wages),col = "blue", size = 2, alpha = 0.8) +
  geom_point(aes(x = Exper, y = Predictions),col = "red", size = 2, alpha = 0.8)

