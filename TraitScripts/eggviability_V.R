
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

Spanoudis18_eviability <- read.csv("./data/eggviability/Spanoudis18_eviability_modestus.csv",TRUE)
names(Spanoudis18_eviability) [1] <- "temperature"
view(Spanoudis18_eviability)




##-- ONLY 1 DATASET - DIDNT COMBINE ---##


write.csv(Spanoudis18_eviability, file = "mo_eviability_V.csv")


##-- LINEAR PLOT --##

##-- INCLUDING WEIGHTS

temperature <- Spanoudis18_eviability$temperature
eviability <- Spanoudis18_eviability$viability
weight <- Spanoudis18_eviability$weight

plot(temperature, eviability, pch=16, main = "Linear Model - Egg Viability vs Temperature", xlab = "Temperature (Celsius)", ylab = "Egg Viabilty")

lm_eviability <- lm(eviability~temperature, data= Spanoudis18_eviability, weights = weight)
lm_eviability

lines(temperature, predict(lm_eviability, list(temperature)))

AICc(lm_eviability, k=2)
#0.7619032




##-- QUADRATIC --##


q_eviability <- function(q, temperature, tmin, tmax) {-q*(temperature-tmin)*(temperature-tmax)}

q_eviability(2,2,15,40)
#-- check eq works

m.q_eviability <- nls(eviability~q_eviability(q, temperature, tmin, tmax), data=Spanoudis18_eviability, start=list(q=-0.1, tmin=7, tmax=50), weights = weight, trace=T)

plot(temperature, eviability, main= "Egg Viability (V)", xlab = "Temperature (Celsius)", ylab = "Egg Viability")

lines(temperature, lwd = 3, col="lightseagreen", predict(m.q_eviability, list(temperature)))

AICc(m.q_eviability, k=2)
#-136.7244


##-- BRIERE - NOT USED

b_eviability_eq <- function(q, temperature, tmin, tmax) {-q*temperature*(temperature-tmin)*((tmax-temperature)^(1/2))*(temperature<=tmax)*(temperature>=tmin)} 

b_eviability_eq(2,2,15,40)

m.b_eviability <- nls(eviability~b_eviability_eq(q, temperature, tmin, tmax), data=Spanoudis18_eviability, start=list(q=-83, tmin=1, tmax=40), weights = weight, trace=T, algorithm = "port")
#Error in numericDeriv(form[[3L]], names(ind), env) : 
#Missing value or an infinity produced when evaluating the model
#m.b_eviability

#plot(temperature, eviability, pch=16, main = "Briere Model - Egg Viability vs Temperature", xlab = "Temperature (Celsius)", ylab = "Egg Viability")
#lines(temperature, predict(m.b_eviability, list(temperature)))

#AICc(m.b_eviability, k=2)


##-- FINAL GRAPH - QUADRATIC

q <- function(q, temperature, tmin, tmax) {-q*(temperature-tmin)*(temperature-tmax)}
t <- seq(-10, 40, by=0.1)

plot(temperature, eviability, xlim=c(0, 40) , ylim=c(0,1), main= "Egg Viability (V)", xlab = "Temperature (Celsius)", ylab = "Egg Viability (V)")

p_a <- predict(m.q_eviability, list(temperature=t))


f <- ifelse(p_a<0, 0, ifelse(p_a>1, 1, p_a))
f[is.na(f)] = 0
lines(t, lwd = 3, col="lightseagreen", f)


