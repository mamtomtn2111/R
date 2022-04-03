library(markovchain)
DriverZone <- c("North", "South", "West")
DriverZone

ZoneTransition <- matrix(c(0.3, 0.3, 0.4, 0.4, 0.4, 0.2, 0.5, 0.3, 0.2),
                         nrow = 3,
                         byrow = TRUE,
                         dimname = list(DriverZone, DriverZone))


MCZone <- new("markovchain", states = DriverZone,
              byrow = TRUE,
              transitionMatrix = ZoneTransition,
              name = "DriverMoment"
              )

MCZone^2
steadyStates(MCZone)

data("rain", package = "markovchain")
table(rain$rain)

mcAlofi <- markovchainFit(data = rain$rain, name = "Alofi MC")$estimate
mcAlofi
transientStates(mcAlofi)
is.accessible(mcAlofi)
s0 <- t(as.matrix(c(0,1,0)))
s3 <- s0 * (mcAlofi^3) ; s3
s8 <- s0 * (mcAlofi^8) ; s8
steadyStates(mcAlofi)
