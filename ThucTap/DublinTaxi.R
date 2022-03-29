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
