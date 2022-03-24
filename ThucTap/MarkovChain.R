#install.packages("markovchain", dependencies = TRUE, INSTALL_opts = '--no-lock')
#options("install.lock"=FALSE)
library(markovchain)
mcWeather <- new("markovchain", states = c("sunny", "cloudy", "rain"),
                 transitionMatrix = matrix(data = c(0.70, 0.2, 0.1,
                                                    0.3, 0.4, 0.3,
                                                    0.2, 0.45, 0.35), byrow = TRUE, nrow = 3),
                 name = "weather")

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

#Vẽ biểu đồ
plot(mcWeather)
mcIgraph

mcDf <- as(mcWeather, "data.frame")
mcNew <- as(mcDf, "markovchain")
mcDf

steadyStates(mcWeather)

mcIgraph <- as(mcWeather, "igraph")
myMarr <- matrix(c(.1,.8,.1,.5,.3,.2,.2,.6,.2), byrow = TRUE, ncol = 3)
myMC <- as(myMarr, "markovchain")
myMC

conditionalDistribution(mcWeather, "sunny")

steadyStates(mcWeather)

gamblerRuinMarkovchain <- function(moneyMax, prob = 0.5){
  require(matlab)
  matr <- zeros(moneyMax + 1)
  states <- as.character(seq(0, moneyMax, by = 1))
  rownames(matr) = states; colnames(matr) = states
  matr[1,1] = 1; matr[moneyMax + 1, moneyMax + 1] = 1
  for(i in 2:moneyMax){
    matr[i, i -1] = 1 - prob; matr[i, i + 1] = prob
  }
  out <- new("markovchain", transitionMatrix = matr, 
             name = paste("Gambler ruin", moneyMax,"dim", sep = " "))
  return(out)
}
mcGR4 <- gamblerRuinMarkovchain(moneyMax = 4, prob = 0.5)
steadyStates(mcGR4)

absorbingStates(mcGR4)
absorbingStates(mcWeather)

.commclassesKernel <- function (P){
  m <- ncol(P)
  stateNames <- rownames(P)
  T <- zeros (m)
  i <- 1
  while (i <= m) {
    a <- i
    b <- zeros (1, m)
    b[1,i] <-1
    old <- 1
    new <- 0
    while (old != new) {
      old <- sum (find (b > 0))
      n <- size (a) [2]
      matr <- matrix (as.numeric (P[a,]), ncol = m,
                      nrow = n)
      c <- colSums (matr)
      d <- find (c)
      n <- size (d) [2]
      b[1, d] <- ones (1, n)
      new <- sum (find (b>0))
      a <- d
    }
  T[i,] <- b
  i <- i+1}
    F <- t(T)
    C <- (T > 0) & (F > 0)
    v <- (apply (t (C) == t(T), 2, sum) == m)
    colnames (C) <- stateNames
    rownames (C) <- stateNames
    names (v) <- stateNames
    out <- list (C = C, v = v)
    return (out)
}

P <- matlab::zeros (10)
P[1, c(1, 3)] <- 1/2;
P[2, 2] <- 1/3; P[2,7] <- 2/3
P[3, 1] <- 1;
P[4, 5] <- 1;
P[5, c(4, 5, 9)] <- 1/3
P[6, 6] <- 1;
P[7, 7] <- 1/4; P[7,9] <- 3/4;
P[8, c(3, 4, 8, 10)] <- 1/4;
P[9, 2] <- 1;
P[10, c(2, 5, 10)] <- 1/3;
rownames (P) <- letters[1:10]
colnames (P) <- letters[1:10]
probMc <- new ("markovchain", transitionMatrix = P, 
               name = "Probability MC")
.commclassesKernel (P)

summary(probMc)
transientStates(probMc)

probMcCanonic <- canonicForm(probMc)
probMc
probMcCanonic

is.accessible(object = probMc, "b", "c")

E <- matrix (0, nrow = 4, ncol = 4)
E[1, 2] <- 1
E[2, 1] <- 1/3; E[2, 3] <- 2/3
E[3, 2] <- 1/4; E[3, 4] <- 3/4
E[4, 3] <- 1
mcE <- new ("markovchain", states = c("a", "b", "c", "d"),
           transitionMatrix = E,
           name = "E")
is.irreducible (mcE)
period(mcE)

require(matlab)
mathematicaMatr <- zeros(5)
mathematicaMatr[1,] <- c(0, 1/3, 0, 2/3, 0)
mathematicaMatr[2,] <- c(1/2, 0, 0, 0, 1/2)
mathematicaMatr[3,] <- c(0, 0, 1/2, 1/2, 0)
mathematicaMatr[4,] <- c(0, 0, 1/2, 1/2, 0)
mathematicaMatr[5,] <- c(0, 0, 0, 0, 1)
statesNames <- letters[1:5]
mathematicaMc <- new("markovchain", transitionMatrix = mathematicaMatr,
                     name = "Mathematica MC", states = statesNames)

plot(mathematicaMc)

.firstpassageKernel <- function (P, i, n) {
  G <- P
  H <- P[i,]
  E <- 1 - diag(size (P) [2])
  for (m in 2:n) {
    G <- P %*% (G * E)
    H <- rbind (H, G[i,])
    return (H)
  }
}

firstPassagePdf <- firstPassage(object = mcWeather, state = "sunny",
                                n = 10)
firstPassagePdf[3,3]

weatherOfDays <- rmarkovchain(n = 365, object = mcWeather, t0 = "sunny")
weatherOfDays[1:30]

weatherFittedMLE <- markovchainFit(data = weatherOfDays,
                                   method = "mle",name = "Weather MLE") 
weatherFittedMLE$estimate

weatherFittedLAPLACE <- markovchainFit(data = weatherOfDays,
                                       method = "laplace", laplacian = 0.01,
                                       name = "Weather LAPLACE")
weatherFittedLAPLACE$estimate

createSequenceMatrix(stringchar = weatherOfDays)

predict(object = weatherFittedMLE$estimate, newdata = c("sunny"),
        n.ahead = 3)