benford_law <- function(d){
  prob_d <- log10(1+1/d)
  return(prob_d)
}

myvector <- c(1:10)
benford_prob <- benford_law(myvector)

barplot(benford_prob, names.arg = myvector, xlab = "THe First Digit",
        ylab = "Probability", main = "Probability simulation",
        ylim = c(0, 0.35), col = "cornflowerblue", border = "black")

x <- runif(100, 10, 50)

#function to get the first digit
first_digit <- function(n){
  substr(gsub('[.]','',n), 1, 1)
}

#Test function
myvector <- c(1.2, 2.3333,3.14, 5432, 5.11, 1.14)
ketqua <- first_digit(myvector)
print(ketqua)

prob_firsts_digit <- function(x){
  data.frame(table(first_digit(x))/length(x))
}

prob_firsts_digit(myvector) -> ketqua2

N <- 1000
set.seed(1234)
x1 <- runif(N, 0, 100)
mydata1 <- prob_firsts_digit(x1)
names(mydata1) <- c("Digits","Probability")
mydata1[-1,] -> mydata1

lines(x = benford_prob[,1],y = mydata1$Probability, col = "red", lwd = 2,
      type = "b", pch = 10, cex = 0.5, bg = "red")

x2 <- runif(100000, 0, 10000)
mydata2 <- prob_firsts_digit(x2)

#library benford
library(benford.analysis)
data("corporate.payment")

benford_test <- benford(corporate.payment$Amount)
plot(benford_test)

benford_test

#Check the suspicious elements (payments - transactions)
suspects <- getSuspects(benford_test, corporate.payment)

library(imager)
library(imagerExtra)
library(benford.analysis)
library(dplyr)
# Load the image
ngoctrinh <- load.image("G:/Tài liệu học tập/2021-2022/HK2/Trực quan hóa dữ liệu/FileProject/TH16-2/image.jpg") %>%
  grayscale()
# Make the DCT (Discrete Cosine Transformation)
ngoctrinh_df <- DCT2D(ngoctrinh) %>%
  as.data.frame()
# Apply the Benford
bfd.ngoctrinh <- benford(ngoctrinh_df$value, number.of.digits = 1,
                         discrete = T, round = 1, sign = "both")
# Plot the result
plot(bfd.ngoctrinh)