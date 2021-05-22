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
#setwd("~/Vet Epi/MSc Project/Model")


##-- IMPORT DATASETS --##
Datafolder <- 'L:/R&D/DES/DES_Dept_Staff/BRR/Project files/VEO/WP5/Data/TraitData'

Ruybal16_biterate <- read.csv(paste0(Datafolder, "/biterate_a/Ruybal16_biterate.csv"), header=TRUE)
view(Ruybal16_biterate)
names(Ruybal16_biterate) [1] <- "temperature"

Madder83_biterate_field <- read.csv(paste0(Datafolder, "/biterate_a/Madder83_biterate_field.csv"),header = TRUE)
view(Madder83_biterate_field)
names(Madder83_biterate_field) [1] <- "temperature"

Madder83_biterate_lab <- read.csv(paste0(Datafolder, "/biterate_a/Madder83_biterate_lab.csv"), header =TRUE)
view(Madder83_biterate_lab)
names(Madder83_biterate_lab) [1] <- "temperature"


##-- COMBINE AND ORDER BY TEMPERATURE (NEW COLUMN FOR DEVELOPMENT RATE)---##

merged_biterate <- bind_rows(list(Ruybal16_biterate, 
                                   Madder83_biterate_field,
                                   Madder83_biterate_lab
                                   
), .id = "source")
view(merged_biterate)

order(merged_biterate$temperature, decreasing=FALSE)
#orders dataset by temp

merged_biterate_o <- merged_biterate[order(merged_biterate$temperature , decreasing=FALSE),]
head(merged_biterate_o)

write.csv(merged_biterate_o, file = "mo_biterate_a.csv")

ggplot(data = merged_biterate_o, aes(x = temperature, y = gon_c, color=source)) +
  geom_point() +
  theme_bw() +
  labs(title = "Gonotrophic Cycle vs temperature",
       x = "Temperature",
       y = "G Cycle",
       color = "source")
#To see distribution of papers 

##-- Create new bite rate column
merged_biterate_o$biterate <- 1/(merged_biterate_o$gon_c)
view(merged_biterate_o)

ggplot(data = merged_biterate_o, aes(x = temperature, y = biterate, color=source)) +
  geom_point() +
  theme_bw() +
  labs(title = "Bite Rate vs temperature",
       x = "Temperature",
       y = "Bite rate",
       color = "source")


##-- LINEAR PLOT --##

##-- INCLUDING WEIGHTS

temperature <- merged_biterate_o$temperature
biterate <- merged_biterate_o$biterate
weight <- merged_biterate_o$weight

plot(temperature, biterate, pch=16, main = "Linear Model - Bite Rate vs Temperature", xlab = "Temperature (Celsius)", ylab = "Bite rate")

lm_biterate <- lm(biterate~temperature, data= merged_biterate_o, weights = weight)
lm_biterate

lines(temperature,lwd = 3, col="lightseagreen", predict(lm_biterate, list(temperature)))

AICc(lm_biterate, k=2)
#-38.39809




##-- QUADRATIC --##


q_biterate <- function(q, temperature, tmin, tmax) {-q*(temperature-tmin)*(temperature-tmax)}

q_biterate(2,2,15,40)
#-- check eq works

m.q_biterate <- nls(biterate~q_biterate(q, temperature, tmin, tmax), data=merged_biterate_o, start=list(q=-0.1, tmin=7, tmax=50), weights = weight, trace=T)

plot(temperature, biterate, main= "Bite Rate (a)", xlab = "Temperature (Celsius)", ylab = "Bite rate")

lines(temperature, lwd = 3, col="lightseagreen", predict(m.q_biterate, list(temperature)))

AICc(m.q_biterate, k=2)
#-574.5023



##-- BRIERE  --Not used 

b_biterate_eq <- function(q, temperature, tmin, tmax) {-q*temperature*(temperature-tmin)*((tmax-temperature)^(1/2))*(temperature<=tmax)*(temperature>=tmin)} 

b_biterate_eq(2,2,15,40)

m.b_biterate <- nls(biterate~b_biterate_eq(q, temperature, tmin, tmax), data=merged_biterate_o, start=list(q=0.1, tmin=1, tmax=40), weights = weight, trace=T, algorithm = "port")
#Error in numericDeriv(form[[3L]], names(ind), env) : 
#Missing value or an infinity produced when evaluating the model
m.b_biterate

plot(temperature, biterate, pch=16, main = "Briere Model - biterate vs Temperature", xlab = "Temperature (Celsius)", ylab = "bite rate")
lines(temperature, predict(m.b_biterate, list(temperature)))

AICc(m.b_biterate, k=2)




##-- FINAL GRAPH - QUADRATIC
q <- function(q, temperature, tmin, tmax) {-q*(temperature-tmin)*(temperature-tmax)}
t <- seq(-10, 40, by=0.1)

plot(temperature, biterate, xlim=c(0, 40), ylim=c(0,0.5) , main= "Bite rate (a)", xlab = "Temperature (Celsius)", ylab = "Bite rate (a)")

p_a <- predict(m.q_biterate, list(temperature=t))


f <- ifelse(p_a<0, 0, p_a)
lines(t, lwd = 3, col="lightseagreen", f)


write.csv('biterateTemp.csv', row.names=FALSE)
