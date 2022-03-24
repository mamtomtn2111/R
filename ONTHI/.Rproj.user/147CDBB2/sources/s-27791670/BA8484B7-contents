library(MASS)
library(ggplot2)
library(PerformanceAnalytics)
library(tidyverse)
library(dplyr)
library(ggExtra)
library(gridExtra)
library(corrplot)
library(Hmisc)

data("Boston")

dim(Boston)
glimpse(Boston)

sum(is.na(Boston))
#Checking the missing values in each column
sapply(Boston, anyNA)

#Checking the duplicated values
sum(duplicated(Boston))

ggplot(data = Boston, mapping = aes(x = medv)) +
  geom_histogram(binwidth = 5, color= "red",
                 fill = "cornflowerblue")
??Boston
counts <- table(Boston$chas)
counts

ggplot(data = Boston, mapping = aes(x = as.factor(chas))) +
  geom_bar()

#Visualization by pie chart
ggplot(data = Boston, mapping = aes(x = factor(1), fill = as.factor(chas))) +
  geom_bar(stat = "count") +
  coord_polar("y")+
  labs(fill = "chas",title = "The number of house in front of charles river")

#The correlation between medv and lstat
ggplot(data = Boston, mapping = aes(x = medv, y = lstat, colour = as.factor(rad)))+
  geom_point(size = 2, alpha = 0.5)

# THe correlation
p <- ggplot(data = Boston, mapping = aes(x = medv, y = lstat, color = rad)) +
  geom_point()
ggMarginal(p, type = "histogram") ->s1
ggMarginal(p, type = "density") ->s2
ggMarginal(p, type = "boxplot") ->s3
ggMarginal(p, type = "violin") ->s4
ggMarginal(p, type = "densigram") ->s5

grid.arrange(s1,s2,s3,s4,s5, ncol = 3)

# Remove chas and read 

Boston_num <-  Boston[, -c(4,9)]
#using the base
cor_matrix <- cor(Boston_num)
cor_matrix <- round(cor_matrix, 2)
ggcorrplot::ggcorrplot(cor_matrix, hc.order = TRUE)

#Using the numerical correlation
corrplot(cor_matrix, method = "number", type = "upper", diag = FALSE)
corrplot(cor_matrix, method = "shade", type = "upper", diag = FALSE)
corrplot(cor_matrix, method = "shade", type = "lower", diag = FALSE)

#Histogram of all values
ggplot(gather(Boston),mapping = aes(x = value)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ key, scales = "free") +
  theme_gray()

hist.data.frame(Boston)

#Boston %>% 
#  gather(key, val, -medv) %>% 
#  ggplot(aes(x = val, y = medv)) %>% 
#  geom_point()

#Train model
library(caTools)
mydata <-  Boston
mydata["ID"] <- c(1:506)
set.seed(123)

split <- sample.split(mydata$ID, SplitRatio = 2/3)

table(split)

training_data <- subset(mydata, split == TRUE)
testing_data <- subset(mydata, split == FALSE)

#model
model1 <- lm(medv ~ lstat + age + rm, data = training_data)
summary(model1)

#prediction in training data
prediction_training <- predict(model1, training_data)

SSR_training <- sum((prediction_training - training_data$medv)^2)
SST_training <- sum((mean(training_data$medv) - training_data$medv)^2)

#The coeficient of datemindation
R2_training <- 1 - (SSR_training/SST_training)


#On the testing data
testing_training <- predict(model1, testing_data)

SSR_testing <- sum((testing_training - testing_data$medv)^2)
SST_testing <- sum((mean(training_data$medv) - testing_data$medv)^2)

#The coeficient of datemindation
R2_testing <- 1 - (SSR_testing/SST_testing)










