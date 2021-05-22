###-- adult longevity


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

Ciota14_adultlongevity_fieldcaught <- read.csv("C:/Users/Gabriella/Documents/Vet Epi/MSc Project/Model/data/adultsurvival_p/Ciota14_adultlongevity_fieldcaught.csv",FALSE)
names(Ciota14_adultlongevity_fieldcaught) [1] <- "temperature"
names(Ciota14_adultlongevity_fieldcaught) [2] <- "longevity_d"
names(Ciota14_adultlongevity_fieldcaught) [3] <- "weight"
view(Ciota14_adultlongevity_fieldcaught)

Ciota14_adultlongevity_lab <- read.csv("./data/adultsurvival_p/Ciota14_adultlongevity_lab.csv",FALSE)

names(Ciota14_adultlongevity_lab) [1] <- "temperature"
names(Ciota14_adultlongevity_lab) [2] <- "longevity_d"
names(Ciota14_adultlongevity_lab) [3] <- "weight"
view(Ciota14_adultlongevity_lab)

andreadis14_adultlongevity_female <- read.csv("./data/adultsurvival_p/andreadis14_adultlongevity_female.csv",TRUE)

names(andreadis14_adultlongevity_female) [1] <- "temperature"
names(andreadis14_adultlongevity_female) [2] <- "longevity_d"
names(andreadis14_adultlongevity_female) [3] <- "weight"
view(andreadis14_adultlongevity_female)

Ruybal16_mediansurvival <- read.csv("./data/adultsurvival_p/Ruybal16_mediansurvival.csv",TRUE)

names(Ruybal16_mediansurvival) [1] <- "temperature"

view(Ruybal16_mediansurvival)

##-- COMBINE AND ORDER BY TEMPERATURE ---##

merged_longevity <- bind_rows(list(Ciota14_adultlongevity_fieldcaught, 
                              Ciota14_adultlongevity_lab, 
                              andreadis14_adultlongevity_female, 
                              Ruybal16_mediansurvival
), .id = "source")
view(merged_longevity)


ggplot(data = merged_longevity, aes(x = temperature, y = longevity_d, color=source)) +
  geom_point() +
  theme_bw() +
  labs(title = "Longevity vs Temperature",
       x = "Temperature",
       y = "Adult Longevity (days)",
       color = "source")


order(merged_longevity$temperature, decreasing=FALSE)
#orders dataset by temp

merged_longevity_o <- merged_longevity[order(merged_longevity$temperature , decreasing=FALSE),]
head(merged_longevity_o)

write.csv(merged_longevity_o, file = "m0_longevity_u.csv")

ggplot(data = merged_longevity_o, aes(x = temperature, y = longevity_d, color=source)) +
  geom_point() +
  theme_bw() +
  labs(title = "Larval survival vs temperature",
       x = "Temperature",
       y = "Larval survival (%)",
       color = "source")
#To see data distribution 



##-- LINEAR PLOT --##

##-- INCLUDING WEIGHTS

temperature <- merged_longevity_o$temperature
t <- seq(-10, 40, by=0.1)
longevity <- merged_longevity_o$longevity_d
weight <- merged_longevity_o$weight

plot(temperature, longevity, xlim=c(0, 40), main = "Adult longevity (1/mu)", xlab = "Temperature (Celsius)", ylab = "Adult longevity (days)")

lm_longevity <- lm(longevity~temperature, data= merged_longevity_o, weights = weight)
lm_longevity

lines(temperature, predict(lm_longevity, list(t)))

AICc(lm_longevity, k=2)
#355.8244



##-- QUADRATIC --##

q<- function(q, temperature, tmin, tmax) {-q*(temperature-tmin)*(temperature-tmax)*(temperature<=tmax)*(temperature>=tmin)}

q(2,2,15,40)
#-- check eq works

m.q_longevity <- nls(longevity~q(q, temperature, tmin, tmax), data=merged_longevity_o, start=list(q=-0.1, tmin=7, tmax=50), weights = weight, trace=T)
#error singular gradient
#m.q_longevity
#plot(temperature, longevity, main= "Longevity (1/u)", xlab = "Temperature (Celsius)", ylab = "Longevity (days)")

#lines(temperature, lwd = 3, col="lightseagreen", predict(m.q_longevity, list(t)))

#AICc(m.q_longevity, k=2)



##-- BRIERE  

b <- function(q, temperature, tmin, tmax) {-q*temperature*(temperature-tmin)*((tmax-temperature)^(1/2))*(temperature<=tmax)*(temperature>=tmin)} 

b(2,2,15,40)

m.b_longevity <- nls(longevity~b(q, temperature, tmin, tmax), data=merged_longevity_o, start=list(q=0.1, tmin=1, tmax=40), weights = weight, trace=T, algorithm = "port")
#error - singular convergence
#m.b_longevity

#plot(temperature, longevity, pch=16, main = "Briere Model - Longevity vs Temperature", xlab = "Temperature (Celsius)", ylab = "Longevity")
#lines(temperature, predict(m.b_longevity, list(temperature)))

#AICc(m.b_longevity, k=2)





##-- FINAL PLOT - LINEAR

plot(temperature, longevity, xlim=c(0, 40), main = "Adult longevity (1/mu)", xlab = "Temperature (Celsius)", ylab = "Adult longevity (days)")
p_a <- predict(lm_longevity, list(temperature=t))
f <- ifelse(p_a<0, 0, p_a)
lines(t, lwd = 3, col="lightseagreen", f)






##-- NOT USED


###-- Mortality rate (1/adult lifespan) -- DID NOT USE IN END

##-- Create new rate column
merged_longevity_o$deathrate <- 1/(merged_longevity_o$longevity_d)
deathrate <- merged_longevity_o$deathrate


##Linear

plot(temperature, deathrate, pch=16, main = "Linear Model - deathrate vs Temperature", xlab = "Temperature (Celsius)", ylab = "Deathrate")

lm_dr <- lm(deathrate~temperature, data= merged_longevity_o, weights = weight)
lm_dr

lines(temperature, predict(lm_dr, list(temperature)))

AICc(lm_dr, k=2)
#-120.9224

##-- QUADRATIC

q<- function(q, temperature, tmin, tmax) {-q*(temperature-tmin)*(temperature-tmax)*(temperature<=tmax)*(temperature>=tmin)}

q(2,2,15,40)
#-- check eq works

m.q_deathrate <- nls(deathrate~q(q, temperature, tmin, tmax), data=merged_longevity_o, start=list(q=-0.1, tmin=7, tmax=50), weights = weight, trace=T)
m.q_deathrate
#error - singular gradient
#plot(temperature, deathrate, main= "DR (1/u)", xlab = "Temperature (Celsius)", ylab = "DR (days)")


#lines(temperature, lwd = 3, col="lightseagreen", predict(m.q_deathrate, list(temperature)))

#AICc(m.q_deathrate, k=2)


