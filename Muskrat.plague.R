library("deSolve")

parameters <- c(New.muskrat.rate = 20,
                Catch.rate.per.trap = 0.2,
                Traps.per.license = 11,
                Initial.licenses = 10)

parameters2 <- c(New.muskrat.rate = 20,
                Catch.rate.per.trap = 0.195,
                Traps.per.license = 10,
                Initial.licenses = 10)

parameters3 <- c(New.muskrat.rate = 20,
                Catch.rate.per.trap = 0.205,
                Traps.per.license = 10,
                Initial.licenses = 10)

InitialConditions <- c(Muskrat.population = 100)

times <- seq(2020,
             2030,
             1)

intg.method<-c("rk4")

muskrat.plague <- function(t, state, parameters) {
  with(as.list(c(state,parameters)), {
    #auxiliares
    Muskrats.caught.per.trap <- Muskrat.population * Catch.rate.per.trap
    Total.traps <- Initial.licenses * Traps.per.license

    #flujo
    New.muskrats <- Muskrat.population * New.muskrat.rate
    Muskrats.caught <- Muskrats.caught.per.trap * Total.traps

    #estado
    dMuskrat.population <- (New.muskrats - Muskrats.caught)

    list(c(dMuskrat.population), New.muskrats = New.muskrats, Muskrats.caught = Muskrats.caught )

  })
}

out <- ode(y = InitialConditions,
           times = times,
           func = muskrat.plague,
           parms = parameters,
           method = intg.method)

out2 <- ode(y = InitialConditions,
           times = times,
           func = muskrat.plague,
           parms = parameters2,
           method = intg.method)

out3 <- ode(y = InitialConditions,
           times = times,
           func = muskrat.plague,
           parms = parameters3,
           method = intg.method)


plot(out)

plot(out2)

plot(out3)

plot(out,out2,out3)

summary(out)
