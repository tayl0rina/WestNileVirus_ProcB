#Fecundity final

##-- Modelling attempt 1 - adult survival probability data 

#-- packages from LSHTM tutorial - always do tidyverse last

library(gapminder)
library(epiDisplay)
library(magrittr)
library(foreign)
library(psych)
library(ggplot2)
library(AICcmodavg)
library(tidyverse)

#install.packages("epiDisplay", dependencies = T)
#-- set working directory
getwd()
setwd("~/Vet Epi/MSc Project/Model")


##-- IMPORT DATASETS --##

Shocket20_fecundity <- read.csv("./data/fecundity/Shocket_fecundity.csv",TRUE)
names(Shocket20_fecundity) [1] <- "temperature"
view(Shocket20_fecundity)




##-- DIDNT NEED TO MERGE OR ORDER ---##



write.csv(Shocket20_fecundity, file = "mo_fecundity_Final_F.csv")




##-- LINEAR PLOT --##

##-- INCLUDING WEIGHTS

temperature <- Shocket20_fecundity$temperature
eggs <- Shocket20_fecundity$egc
weight <- Shocket20_fecundity$weight
t <- seq(0, 40, by=0.1)

plot(temperature, eggs, pch=16, main = "Linear Model - Eggs per Raft vs Temperature", xlab = "Temperature (Celsius)", ylab = "Eggs per Raft")

lm_fecundity <- lm(eggs~temperature, data= Shocket20_fecundity, weights = weight)
lm_fecundity

lines(temperature, predict(lm_fecundity, list(t)))

AICc(lm_fecundity, k=2)
#92.79829




##-- QUADRATIC --##


q_fecundity <- function(q, temperature, tmin, tmax) {-q*(temperature-tmin)*(temperature-tmax)}

q_fecundity(2,2,15,40)
#-- check eq works

m.q_fecundity <- nls(eggs~q_fecundity(q, temperature, tmin, tmax), data=Shocket20_fecundity, start=list(q=-0.1, tmin=7, tmax=50), weights = weight, trace=T)

plot(temperature, eggs, xlim=c(0, 40) , main= "Fecundity (F)", xlab = "Temperature (Celsius)", ylab = "Eggs per Gonadotrophic Cycle")

p_fecundity <- predict(m.q_fecundity, list(temperature=t))
lines(t, lwd = 3, col="lightseagreen", p_fecundity)

AICc(m.q_fecundity, k=2)
#99.14627



##-- BRIERE  --NOT ADAPTED

b_fecundity_eq <- function(q, temperature, tmin, tmax) {-q*temperature*(temperature-tmin)*((tmax-temperature)^(1/2))*(temperature<=tmax)*(temperature>=tmin)} 

b_fecundity_eq(2,2,15,40)

m.b_fecundity <- nls(eggs~b_fecundity_eq(q, temperature, tmin, tmax), data=Shocket20_fecundity, start=list(q=0.1, tmin=1, tmax=40), weights = weight, trace=T, algorithm = "port")
#Error in numericDeriv(form[[3L]], names(ind), env) : 
#Missing value or an infinity produced when evaluating the model
#m.b_fecundity

#plot(temperature, eggs, pch=16, main = "Briere Model - Eggs per raft vs Temperature", xlab = "Temperature (Celsius)", ylab = "Eggs per raft")
#lines(temperature, predict(m.b_fecundity, list(temperature)))

#AICc(m.b_fecundity, k=2)



##-- FINAL PLOT - QUADRATIC
plot(temperature, eggs, xlim=c(0, 40) , main= "Fecundity (F)", xlab = "Temperature (Celsius)", ylab = "Eggs per Gonadotrophic Cycle")

f <- ifelse(p_fecundity<0, 0, p_fecundity)
lines(t, lwd = 3, col="lightseagreen", f)


