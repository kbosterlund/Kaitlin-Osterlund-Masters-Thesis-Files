# Temperature Cycles and Spatial Synchrony
# Modelling Chapter
# Kaitlin Osterlund

#### Rosenzweig-MacArthur Model with Cyclic Environmental Component ####
# Predator-prey model with prey density that grows with density-dependence
# and predator functional response that is non-linear (type II) with sin 
# wave carrying capacity of prey to simulate cyclic environmental perturbations
# two-patch model - each patch starts at different set initial densities

rm(list=ls(all=T))

library(deSolve)
library(tidyverse)
library(ggplot2)

# Sine wave oscillator
sine.wave<-function(t,x,p){
  z1<-x[1]
  z2<-x[2]
  with(as.list(p),{
    dz1.dt=1
    dz2.dt=cos(z1*(2*pi)*a2)*a1
    return(list(c(dz1.dt,dz2.dt)))
  })
}

tmax<-600 #maximum number of time increments
Time<-seq(1,tmax,by = 0.1)

z1_init<-1 #must be equal to 1. We always start counting time upwards from 1 in R
z2_init<-0 #initial value of the sine wave


a1<-300 # amplitude
# low = 50, medium = 150, high = 300

a2<-(1/28) # period
# 16, 20, 24, 28, 32, 36, 40, 44

# setup cycling parameter
params_sine.wave <- c(a1=a1,a2=a2)
K = 4000 # carrying capacity
sine.wave_model <- ode(c(z1_init,z2_init), Time, sine.wave, params_sine.wave)

wave.signal <- sine.wave_model[,-2]
wave.signal[,2] <- wave.signal[,2] + K

K.wave.input <- approxfun(wave.signal, rule = 2)

# RM model with cyclic K
predpreyRM <- function(t,y,p){
  K.wave.param <- K.wave.input(t)
  H <- y[1]
  P <- y[2]
  with(as.list(p),{
    dH.dt = b*H*(1-(1/K.wave.param)*H)-w*P*H/(D+H)
    dP.dt = e*w*P*H/(D+H)-s*P
    
    return(list(c(dH.dt,dP.dt)))
  })
}


# Set initial predator prey densities
# 50 = asynch, 30 = partial asynch, 20 = close to synch
H.start_patch1 <- 10 # Patch 1 prey
P.start_patch1 <- 0 # Patch 1 predator

H.start_patch2 <- 10 # Patch 2 prey
P.start_patch2 <- 0 # Patch 2 predator

# define parameters
b <- 0.9 # per capita birth rate of prey 
e <- 0.07 # per capita birth rate of predator
w <- 6 # per capita death rate of predator
D <- 1200 # maximum capture rate of prey by predators
s <- 0.2 # half-saturation rate, prey density
K <- 4000 # prey carrying capacity
K_cyc <- sine.wave_model[,3] + K # cyclic carrying capacity

p_RM <- c(b=b, e=e, K=K, w=w, D=D)

# Produce the RM model for both patches

RM_patch1 <- ode(c(H.start_patch1, P.start_patch1), Time, predpreyRM, p_RM)
RM_patch2 <- ode(c(H.start_patch2, P.start_patch2), Time, predpreyRM, p_RM)

# Arrange model dataframe for plotting
RM_merged <- cbind(RM_patch1, RM_patch2)
RM_merged_adjusted <- RM_merged[,-4]
RM_pred.prey_df <- as.data.frame(RM_merged_adjusted)
RM_pred.prey_full.df <- data.frame(RM_pred.prey_df,K_cyc)
colnames(RM_pred.prey_full.df) <- c("time","Prey_Patch.1","Predator_Patch.1",
                                    "Prey_Patch.2","Predator_Patch.2","Cyclic.K")

log_RM_merged_adjusted <- log(RM_merged_adjusted)

# Plot model over time
matplot(Time, cbind(RM_merged_adjusted[,2:5]), type="l", xlab = "Time (Days)",
        ylab = "Protist Density (protists/mL)", lty = c(1,6,1,6), lwd = 2,
        col = c("blue","blue","red","red"))
legend("topright", legend = c("H - Patch 1", "P - Patch 1", "H - Patch 2", "P - Patch 2"),
       lty = c(1,6,1,6), col = c("blue","blue","red","red"), lwd = 2, cex = 0.5)

# Plot model over time - log transformed
matplot(Time, cbind(log_RM_merged_adjusted[,2:5]), type="l", xlab = "Time (Days)",
        ylab = "Log Protist Density (log protists/mL)", lty = c(1,6,1,6), lwd = 2,
        col = c("blue","blue","red","red"))
legend("bottomright", legend = c("H - Patch 1", "P - Patch 1", "H - Patch 2", "P - Patch 2"),
       lty = c(1,6,1,6), col = c("blue","blue","red","red"), lwd = 2, cex = 0.5)

