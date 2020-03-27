###BIOS 331 POPULATION GROWTH###

library(ggplot2)
library(tidyverse)
library(reshape)

##Plotting function to see data 
plot.growth <- function(mydata, mytitle){
  
  p <- ggplot(mydata, aes(x=Generations, y=N)) + 
    geom_line() +
    xlab('Generations') +
    ylab('Population (N)') +
    theme_minimal() +
    ggtitle(label = mytitle)
  
  print(p)
  
}

plot.growth2 <- function(mydata, mytitle){
  
  p <- ggplot2::ggplot(mydata, aes(x=Generations, y=value, col=Group)) + 
    geom_line() +
    xlab('Generations') +
    ylab('Population (N)') +
    theme_minimal() +
    ggtitle(label = mytitle)
  
  print(p)
  
}

##Discrete-time exponential population growth
##Nt+1 = Nt * e^r
##r = 0.01, r = 0.1, r = -0.1, r = 0.5, r = 1

discrete.exp <- function(N0, r, timestep){
  
  p <- numeric(length = timestep)
  p[1] <- N0
  
  for(i in 2:timestep){
    Nt <- p[i - 1]
    p[i] <- Nt * exp(r)
    
  }
  
  p <- as.data.frame(p) %>%
    add_column(., Generations=1:timestep) %>%
    setNames(., c("N", "Generations"))
  
return(p)
  
}

dis.exp.growth <- discrete.exp(N0 = 10, r = 0.01, timestep = 25) %>%
  setNames(., c("r = 0.01", "Generations"))
dis.exp.growth1 <- discrete.exp(N0 = 10, r = 0.1, timestep = 25) %>%
  setNames(., c("r = 0.1", "Generations"))
dis.exp.growth2 <- discrete.exp(N0 = 10, r = -0.1, timestep = 25) %>%
  setNames(., c("r = -0.1", "Generations"))

df <- full_join(dis.exp.growth, dis.exp.growth1, by="Generations") %>%
  full_join(dis.exp.growth2, by="Generations") %>%
  melt(., id.vars="Generations") %>%
  setNames(., c("Generations", "Group", "value"))

plot.growth2(mydata = df, mytitle = "Logisitic Population Growth")

dis.exp.growth3 <- discrete.exp(N0 = 10, r = 0.5, timestep = 25)
dis.exp.growth4 <- discrete.exp(N0 = 10, r = 1, timestep = 25)


plot.growth(mydata = dis.exp.growth, mytitle = "Discrete Exponential Growth")


##Logistic population growth
##Nt+1 = Nt * e^r(K-Nt/K)

##r = 0.5, r = 1.5, r = 2.3, r = 2.6, r = 2.69, Chaos (r = ?)


logistic.exp <- function(N0, r, timestep, K){

  p <- numeric(length = timestep)
  p[1] <- N0
  
  for(i in 2:timestep){
    Nt <- p[i - 1]
    p[i] <- Nt * exp(r * ((K-Nt)/K))
    
  }
  
  p <- as.data.frame(p) %>%
    add_column(., Generations=1:timestep) %>%
    setNames(., c("N", "Generations"))
  
  return(p)
  
}

log.growth <- logistic.exp(N0=10, r=0.05, timestep = 25, K=100)
plot.growth(mydata = log.growth, mytitle = "Logistic Exponential Growth")

##Modified logistic growth
##Nt+1 = Nt * e^r(1-[Nt/K]^alpha)

##r= 0.5, alpha=0.5, r= 1.5, alpha=0.5, r= 2.3, alpha=0.5, 
##r= 0.5, alpha=1.5, r= 1.5, alpha=1.5, r= 2.3, alpha=1.5

mod.logistic.exp <- function(N0, r, timestep, K, alph){
  
  ##create a blank matrix for data population 
  p <- numeric(length = timestep)
  p[1] <- N0
  
  for(i in 2:timestep){
    Nt <- p[i - 1]
    p[i] <- Nt * exp(r * (1 - (Nt/K)^alph))
    
  }
  
  p <- as.data.frame(p) %>%
    add_column(., Generations=1:timestep) %>%
    setNames(., c("N", "Generations"))
  
  return(p)
  
}

mod.growth <- mod.logistic.exp(N0 = 10, r = 0.05, timestep = 25, K = 100, alph = 0.5)
plot.growth(mydata = mod.growth, mytitle = "Modified Logistic Exponential Growth")
