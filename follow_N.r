#' follow_N.r
#' This is the basic follow the nitrogen function.
#' It simulates plant N uptake from soil, plant biomass N turnover, N deposition and N leaching.
#' Embed this in a for loop to run a simulation in time. Example below.
#'
#' @param N.soil      Inital soil N stock in g N / m2. Default 350, mean from the FIA to 20cm.
#' @param N.plant     Initial plant N stock in g N / m2. Default 0.
#' @param Ndep        N deposition rate. Default 0.1 g N / m2 / yr. (Same as 1 kg N / ha / yr.)
#' @param MRT         Mean Residence Time of N in plant biomass in years. Default 6 years from Finzi et al. 2007 PNAS.
#' @param N.loss      Exogenous loss rate of N in g N / m2 / yr. Default 0.1 g N / m2 / yr from Aber et al. 1998 Bioscience.
#' @param N.uptake    N uptake rate of the Forest. Default 6- N uptake rate at Duke FACE - Finzi et al. 2007 PNAS.
#' @param fert.effect This accounts for the fact the N deposition increases forest N uptake. Parameters from Thomas et al. 2010 Nature Geoscience.
#'
#' @return         Returns new plant and soil N stocks after 1 year of simulation.
#' @export
#'
#' @examples
#' #Run 100 years of simulation.
#' eco.N.state <- c(0, 350) #initial condition.
#' track <- list()
#' for(i in 1:100){
#'     eco.N.state <- follow_N(eco.N.state[1], eco.N.state[2])
#'     track[[i]] <- eco.N.state
#' }
#' track <- data.frame(do.call(rbind, track))
#' par(mfrow = c(1,2))
#' plot(track$N.plant, main = 'Plant Nitrogen')
#' plot(track$N.soil , main = 'Soil Nitrogen' )
#' 
follow_N <- function(N.plant = 0, N.soil = 300 ,
                     Ndep = 0.1, MRT = 6, N.loss = 0.1, N.uptake = 6,
                     fert.effect = T){
  #Allowing for N deposition to increase plant growth, following Thomas et al. 2010.
  if(fert.effect == T){
    stimulation <- (Ndep - 0.1)*0.5
    N.uptake = N.uptake * stimulation
  }
  #Differential equations.
  dN.plant = N.uptake - N.plant/MRT
  dN.soil  = N.plant/MRT + Ndep - N.uptake - N.loss
  #Update state variables.
  N.plant  = N.plant + dN.plant
  N.soil   = N.soil + dN.soil
  #Return vector of updated state variables.
  out <- c(N.plant, N.soil)
  names(out) <- c('N.plant','N.soil')
  return(out)
}