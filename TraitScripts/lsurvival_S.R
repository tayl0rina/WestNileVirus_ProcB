
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

Ciota14_lsurvival_lab <- read.csv("./data/larvalsurvival_S/Ciota14_lsurvival_lab.csv",TRUE)
names(Ciota14_lsurvival_lab) [1] <- "temperature"
view(Ciota14_lsurvival_lab)

Ciota14_lsurvival_field <- read.csv("./data/larvalsurvival_S/Ciota14_lsurvival_field.csv",TRUE)
names(Ciota14_lsurvival_field) [1] <- "temperature"
view(Ciota14_lsurvival_field)

Loetti11_lsurvival <- read.csv("./data/larvalsurvival_S/Loetti11_lsurvival.csv",TRUE)
view(Loetti11_lsurvival)
names(Loetti11_lsurvival) [1] <- "temperature"
#weird symbols again

Ruybal16_lsurvival <- read.csv("./data/larvalsurvival_S/Ruybal16_lsurvival.csv",TRUE)
view(Ruybal16_lsurvival)

Madder83_lsurvival_field <- read.csv("./data/larvalsurvival_S/Madder83_lsurvival_field.csv",TRUE)
view(Madder83_lsurvival_field)
names(Madder83_lsurvival_field) [1] <- "temperature"

Madder83_lsurvival_lab <- read.csv("./data/larvalsurvival_S/Madder83_lsurvival_lab.csv",TRUE)
view(Madder83_lsurvival_lab)
names(Madder83_lsurvival_lab) [1] <- "temperature"


##-- COMBINE AND ORDER BY TEMPERATURE (NEW COLUMN FOR DEVELOPMENT RATE)---##

merged_lsurvival <- bind_rows(list(Ciota14_lsurvival_field,
                             Ciota14_lsurvival_lab,
                             Loetti11_lsurvival,
                             Ruybal16_lsurvival,
                             Madder83_lsurvival_field,
                             Madder83_lsurvival_lab
                             
), .id = "source")
view(merged_lsurvival)

order(merged_lsurvival$temperature, decreasing=FALSE)
#orders dataset by temp

merged_lsurvival_o <- merged_lsurvival[order(merged_lsurvival$temperature , decreasing=FALSE),]
head(merged_lsurvival_o)

write.csv(merged_lsurvival_o, file = "mo_lsurvival_S.csv")

ggplot(data = merged_lsurvival_o, aes(x = temperature, y = survival, color=source)) +
  geom_point() +
  theme_bw() +
  labs(title = "Larval survival vs temperature",
       x = "Temperature",
       y = "Larval survival (%)",
       color = "source")
#To see which papers are causing problems 



##-- LINEAR PLOT --##

##-- INCLUDING WEIGHTS

temperature <- merged_lsurvival_o$temperature
lsurvival <- merged_lsurvival_o$survival
weight <- merged_lsurvival_o$weight

plot(temperature, lsurvival, pch=16, main = "Linear Model - Larval Survival vs Temperature", xlab = "Temperature (Celsius)", ylab = "Larval Survival")

lm_lsurvival <- lm(lsurvival~temperature, data= merged_lsurvival_o, weights = weight)
lm_lsurvival

lines(temperature, predict(lm_lsurvival, list(temperature)))

AICc(lm_lsurvival, k=2)
#15.31859




##-- QUADRATIC --##


q_lsurvival <- function(q, temperature, tmin, tmax) {-q*(temperature-tmin)*(temperature-tmax)}

q_lsurvival(2,2,15,40)
#-- check eq works

m.q_lsurvival <- nls(lsurvival~q_lsurvival(q, temperature, tmin, tmax), data=merged_lsurvival_o, start=list(q=-0.1, tmin=7, tmax=50), weights = weight, trace=T)

plot(temperature, lsurvival, main= "Larval Survival to Adulthood (S)", xlab = "Temperature (Celsius)", ylab = "Probability of Survival")

lines(temperature, lwd = 3, col="lightseagreen", predict(m.q_lsurvival, list(temperature)))

AICc(m.q_lsurvival, k=2)
#-13703.98



##-- BRIERE - NOT USED  

b_lsurvival_eq <- function(q, temperature, tmin, tmax) {-q*temperature*(temperature-tmin)*((tmax-temperature)^(1/2))*(temperature<=tmax)*(temperature>=tmin)} 

b_lsurvival_eq(2,2,15,40)

m.b_lsurvival <- nls(lsurvival~b_lsurvival_eq(q, temperature, tmin, tmax), data=merged_lsurvival_o, start=list(q=0.1, tmin=1, tmax=40), weights = weight, trace=T, algorithm = "port")
#Error in numericDeriv(form[[3L]], names(ind), env) : 
#Missing value or an infinity produced when evaluating the model
#m.b_lsurvival

#plot(temperature, lsurvival, pch=16, main = "Briere Model - Larval Survival vs Temperature", xlab = "Temperature (Celsius)", ylab = "Larval Survival")
#lines(temperature, predict(m.b_lsurvival, list(temperature)))

#AICc(m.b_lsurvival, k=2)



#FINAL GRAPH - QUADRATIC
q <- function(q, temperature, tmin, tmax) {-q*(temperature-tmin)*(temperature-tmax)}
t <- seq(0, 40, by=0.1)

plot(temperature, lsurvival, xlim=c(0, 40) , ylim=c(0,1), main= "Larval survival Probability (S)", xlab = "Temperature (Celsius)", ylab = "Larval survival probability (S)")

p_a <- predict(m.q_lsurvival, list(temperature=t))


f <- ifelse(p_a<0, 0, ifelse(p_a>1, 1, p_a))
f[is.na(f)] = 0
lines(t, lwd = 3, col="lightseagreen", f)



