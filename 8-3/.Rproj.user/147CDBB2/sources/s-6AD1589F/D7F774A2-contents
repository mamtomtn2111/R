#install.packages(c("tidyverse","dslabs"))
library(tidyverse)
library(dslabs)

data(murders)

head(murders,10)
tail(murders,10)

is.data.frame(murders)
str(murders)
murders.shape()
dim(murders)
names(muders)
colnames(murders)
count(murders)

is.factor(murders$region)

#Truy cập vào biến trong dataframe
#Check độ dài của biến pop
length(murders$population)
nlevels(murders$region)
table(murders$region)

#Thực hành
states <- murders$state

ranks <- rank(murders$population)

ind <- order(murders$population)

my_df <- data.frame(states[ind], rank(ind))


CriminalIdx <- order(-murders$total)
StateHighestCriminal = data.frame(head(states[CriminalIdx],10))
