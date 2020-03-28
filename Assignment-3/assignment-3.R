### BIOS331 MULTISPECIES DYNAMICS ###

library(ggplot2)
library(tidyverse)
library(reshape)

##Plotting function to see data with two lines
plot.growth2 <- function(mydata, mytitle){
  
  p <- ggplot2::ggplot(mydata, aes(x=Generations, y=value, col=Group)) + 
    geom_line() +
    xlab('Generations') +
    ylab('Population (N)') +
    theme_minimal() +
    ggtitle(label = mytitle)
  
  print(p)
  
}

##Lotka-Voltera equations for competition

lot.vol.comp <- function(N01, N02, r1, r2, K1, K2, alph, bet, timestep){

  p1 <- numeric(length = timestep)
  p1[1] <- N01
  
  p2 <- numeric(length = timestep)
  p2[1] <- N02
  
  for(i in 2:timestep){
    Nt1 <- p1[i - 1]
    Nt2 <- p2[i - 1]
    p1[i] <- Nt1 * exp(r1*((K1 - Nt1 - alph * Nt2)/K1))
    p2[i] <- Nt2 * exp(r2*((K2 - Nt2 - bet * Nt1)/K2))
    
  }
  
  p <- cbind(p1, p2) %>%
    as.data.frame() %>%
    setNames(., c("Speces 1", "Species 2")) %>%
    tibble::add_column(Generations = 1:timestep) %>%
    reshape::melt(id.vars="Generations") %>%
    setNames(., c("Generations", "Group", "value"))
  
  return(p)
  
}

competition <- lot.vol.comp(N0 = 10, N02 = 25, r1 = 0.05, r2 = 0.05, 
                     K1 = 100, K2 = 100, alph = 0.3, bet = 0.1, 
                     timestep = 100)

plot.growth2(mydata = competition, mytitle = "Competition")


###Predation equations

pred.prey <- function(K=100, r=1, a=0.1, h=1, b=0.1, d, preypop=50, predpop=5, timestep=1000){
  
  pred <- numeric(length = timestep)
  pred[1] <- predpop
  
  prey <- numeric(length = timestep)
  prey[1] <- preypop
  
  for(i in 2:timestep){
    
    Npred <- pred[i - 1]
    Nprey <- prey[i - 1]
    
    pred[i] <- Npred * exp((b*a*Nprey/(1+a*h*Nprey))-d)
    prey[i] <- Nprey * exp(r*((K - Nprey)/K) - ((a * Npred)/(1+a*h*Nprey)))
    
  }
  
  p <- cbind(pred, prey) %>%
    as.data.frame() %>%
    setNames(., c("Predator", "Prey")) %>%
    tibble::add_column(Generations = 1:timestep) %>%
    reshape::melt(id.vars="Generations") %>%
    setNames(., c("Generations", "Group", "value"))
  
  return(p)
  
}

predpreygrowth <- pred.prey(d=0.08)
predpreygrowth <- pred.prey(r = 0.5, a = 0.06, h = 1, b = 0.05, d = 0.0213)
plot.growth2(mydata = predpreygrowth, mytitle = "Predator-Prey Dynamics")
