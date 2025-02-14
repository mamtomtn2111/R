#install.packages("markovchain", dependencies = TRUE, INSTALL_opts = '--no-lock')
#options("install.lock"=FALSE)
library(markovchain)
mcWeather <- new("markovchain", states = c("sunny", "cloudy", "rain"),
                 transitionMatrix = matrix(data = c(0.70, 0.2, 0.1,
                                                    0.3, 0.4, 0.3,
                                                    0.2, 0.45, 0.35), byrow = TRUE, nrow = 3),
                 name = "weather")
mcWeather

#Thuong
initialState <- c(0, 1 ,0)
after2Days <- (mcWeather*mcWeather) * initialState
after7Days <- (mcWeather^7) * initialState
after2Days
after7Days

#Ma tran chuyen vi
initialState <- c(0, 1 ,0)
after2Days <- (t(mcWeather)*t(mcWeather)) * initialState
after7Days <- (t(mcWeather)^7) * initialState
after2Days
after7Days

#Trang thai va chieu
states(mcWeather)
dim(mcWeather)

#Print va Show
print(mcWeather)
show(mcWeather)

#Vẽ biểu đồ xác suất chuyển
plot(mcWeather)
mcIgraph

#Hàm triển khai markovchain
weatherOfDays <- rmarkovchain(n = 365, object = mcWeather, t0 = "sunny")
weatherOfDays[1:30]

weatherFittedMLE <- markovchainFit(data = weatherOfDays,
                                   method = "mle",name = "Weather MLE") 
weatherFittedMLE$estimate

weatherPredict <- predict(object = weatherFittedMLE$estimate, newdata = c("rain"),
                          n.ahead = 3)

weatherFittedLAPLACE <- markovchainFit(data = weatherOfDays,
                                       method = "laplace", laplacian = 0.01,
                                       name = "Weather LAPLACE")
weatherFittedLAPLACE$estimate

predict(object = weatherFittedMLE$estimate, newdata = c("sunny"),
        n.ahead = 3)

