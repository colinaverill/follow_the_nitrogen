#First experiment- start two forests under high or low N dep (0.1 or 1 g N / m2 / yr)
#Assume C:N of AM forests of 20, EM forest 30.
#Assume low N dep forest is EM, high N dep forest is AM.
#clear environment, source model function.
rm(list=ls())
source('follow_N.r')

#Setup model run.----
am.track <- list()
em.track <- list()
#Run simulation 100 years.
for(i in 1:100){
  #
  if(i == 1){
    am.track[[i]] <- follow_N(Ndep = 1.0)
    em.track[[i]] <- follow_N(Ndep = 0.1)
    next
  }
  am.track[[i]] <- follow_N(N.plant = am.track[[i-1]][1],
                            N.soil  = am.track[[i-1]][2],
                            Ndep    = 1.0)
  em.track[[i]] <- follow_N(N.plant = em.track[[i-1]][1],
                            N.soil  = em.track[[i-1]][2],
                            Ndep    = 0.1)
}
am.track <- data.frame(do.call(rbind, am.track))
em.track <- data.frame(do.call(rbind, em.track))

#Plot soil C over time.----
am.CN <- 20
em.CN <- 30
am.C <- am.track$N.soil * am.CN
em.C <- em.track$N.soil * em.CN
time <- c(1:100)
limy <- c(0, max(c(am.C,em.C)))

par(mfrow = c(1,2))
plot(am.C~ time, bty = 'l', cex = 0, ylab = 'Soil Carbon', ylim = limy)
lines(smooth.spline(am.C ~ time), lwd = 2, col = 'purple')
lines(smooth.spline(em.C ~ time), lwd = 2, col = 'green')
legend(x = 20, y = 3000, col = c('purple','green'), lwd = 2, bty = 'n', legend = c('AM','EM'))

#Percent carbon difference.
C.diff <- ((em.C / am.C ) - 1) * 100
plot(C.diff ~ time, bty = 'l', ylab = '% Difference in EM vs. AM soil C storage', ylim = c(0, max(C.diff)), cex = 0)
lines(smooth.spline(C.diff ~ time), lwd = 2, col = 'black')
