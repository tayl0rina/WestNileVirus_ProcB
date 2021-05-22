
library(gapminder)
library(epiDisplay)
library(magrittr)
library(foreign)
library(psych)
library(ggplot2)
library(AICcmodavg)
library(tidyverse)


#-- set working directory
getwd()
setwd("~/Vet Epi/MSc Project/Model")


##-- IMPORT DATASETS --##


Madder83_eggtime <- read.csv("./data/eggtime/Madder83_eggtime.csv",TRUE)
view(Madder83_eggtime)
names(Madder83_eggtime) [1] <- "temperature"


##-- COMBINE AND ORDER BY TEMPERATURE (NEW COLUMN FOR DEVELOPMENT RATE)---##


order(Madder83_eggtime$temperature, decreasing=FALSE)
#orders dataset by temp

merged_edr_o <- Madder83_eggtime[order(Madder83_eggtime$temperature , decreasing=FALSE),]
head(merged_edr_o)

write.csv(merged_edr_o, file = "mo_edr_S.csv")



##-- Create new rate column
merged_edr_o$edr <- 1/(merged_edr_o$egg_time)
view(merged_edr_o)



##-- LINEAR PLOT - ED TIME - NOT USED -- USED EDR INSTEAD BELOW--##

##-- INCLUDING WEIGHTS

temperature <- merged_edr_o$temperature
edt <- merged_edr_o$egg_time
weight <- merged_edr_o$weight
edr <- merged_edr_o$edr
t <- seq(-10, 50, by=0.1)

plot(temperature, edt, pch=16, main = "Linear Model - Egg development time vs Temperature", xlab = "Temperature (Celsius)", ylab = "Days in egg")

lm_edt <- lm(egg_time~temperature, data= merged_edr_o, weights = weight)
lm_edt

lines(temperature, predict(lm_edt, list(temperature)))

AICc(lm_edt, k=2)
#11.27149



##-- LINEAR EDR - USED --##

plot(temperature, edr,  xlim=c(0, 40) , ylim=c(0,1), main = "Egg development rate (EDR)", xlab = "Temperature (Celsius)", ylab = "Egg development rate (EDR)")

lm_edr <- lm(edr~temperature, data= merged_edr_o, weights = weight)
lm_edr
lines(temperature, predict(lm_edr, list(temperature)))

AICc(lm_edr, k=2)
#-8.739496



##-- QUADRATIC - NOT USED --##

q_edr <- function(q, temperature, tmin, tmax) {-q*(temperature-tmin)*(temperature-tmax)}

q_edr(2,2,15,40)
#-- check eq works

m.q_edr <- nls(edr~q_edr(q, temperature, tmin, tmax), data=merged_edr_o, start=list(q=0.1, tmin=7, tmax=50), weights = weight, trace=T)
#ERROR - step factor reduced below minifactor
#plot(temperature, edr, main= "Egg development rate", xlab = "Temperature (Celsius)", ylab = "Egg development rate")

#lines(temperature, lwd = 3, col="lightseagreen", predict(m.q_edr, list(temperature)))

#AICc(m.q_edr, k=2)




##-- BRIERE  - NOT USED

b_lsurvival_eq <- function(q, temperature, tmin, tmax) {-q*temperature*(temperature-tmin)*((tmax-temperature)^(1/2))*(temperature<=tmax)*(temperature>=tmin)} 

b_lsurvival_eq(2,2,15,40)

m.b_lsurvival <- nls(lsurvival~b_lsurvival_eq(q, temperature, tmin, tmax), data=merged_lsurvival_o, start=list(q=0.1, tmin=1, tmax=40), weights = weight, trace=T, algorithm = "port")
#Error in numericDeriv(form[[3L]], names(ind), env) : 
#Missing value or an infinity produced when evaluating the model
#m.b_lsurvival

#plot(temperature, lsurvival, pch=16, main = "Briere Model - Larval Survival vs Temperature", xlab = "Temperature (Celsius)", ylab = "Larval Survival")
#lines(temperature, predict(m.b_lsurvival, list(temperature)))

#AICc(m.b_lsurvival, k=2)





##-- FINAL PLOT - LINEAR
plot(temperature, edr,  xlim=c(0, 40) , ylim=c(0,1), main = "Egg development rate (EDR)", xlab = "Temperature (Celsius)", ylab = "Egg development rate (EDR)")

p_a <- predict(lm_edr, list(temperature=t))
f <- ifelse(p_a<0, 0,  p_a)
lines(t, lwd = 3, col="lightseagreen", f)

AICc(lm_edr, k=2)
#-8.739496

