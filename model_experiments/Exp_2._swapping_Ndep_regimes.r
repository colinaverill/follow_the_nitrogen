#Experiment 2- build 2 forests for 100 years under low (EM) or high (AM) N deposition.
#Then, turn off N deposition in the high N case, or turn it on in the low N case.
#Assume C:N of AM forests of 20, EM forest 30.
#Assume low N dep forest is EM, high N dep forest is AM.
#clear environment, source model function.
rm(list=ls())
source('follow_N.r')

#Run Forests for 100 years under low (em) or high (am) N deposition.----
hi.track <- list()
lo.track <- list()
#Run simulation 100 years.
for(i in 1:100){
  #Initalize pools with defaults from function.
  if(i == 1){
    hi.track[[i]] <- follow_N(Ndep = 1.0)
    lo.track[[i]] <- follow_N(Ndep = 0.1)
    next
  }
  hi.track[[i]] <- follow_N(N.plant = hi.track[[i-1]][1],
                            N.soil  = hi.track[[i-1]][2],
                            Ndep    = 1.0)
  lo.track[[i]] <- follow_N(N.plant = lo.track[[i-1]][1],
                            N.soil  = lo.track[[i-1]][2],
                            Ndep    = 0.1)
}

#Swap Ndep rates, run another 100 years.----
hi.lo.track <- list()
lo.hi.track <- list()
for(i in 1:100){
  if(i == 1){
    hi.lo.track[[i]] <- follow_N(N.plant = hi.track[[100]][1],
                            N.soil  = hi.track[[100]][2],
                            Ndep    = 0.1)
    lo.hi.track[[i]] <- follow_N(N.plant = lo.track[[100]][1],
                            N.soil  = lo.track[[100]][2],
                            Ndep    = 1.0)
    next
  }
  hi.lo.track[[i]] <- follow_N(N.plant = hi.lo.track[[i-1]][1],
                               N.soil  = hi.lo.track[[i-1]][2],
                               Ndep    = 0.1)
  lo.hi.track[[i]] <- follow_N(N.plant = lo.hi.track[[i-1]][1],
                               N.soil  = lo.hi.track[[i-1]][2],
                               Ndep    = 1.0)
  
}
   hi.track <- data.frame(do.call(rbind,    hi.track))
   lo.track <- data.frame(do.call(rbind,    lo.track))
hi.lo.track <- data.frame(do.call(rbind, hi.lo.track))
lo.hi.track <- data.frame(do.call(rbind, lo.hi.track))

#Visualize Changes in C stocks.----
#AM -> EM @ 100 and 200 years, hi.lo.track.
#EM -> AM @ 100 and 200 years, lo.hi.track.