
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

Ciota14_ldt_lab <- read.csv("./data/larvalddt/Ciota14_ldt_lab.csv",TRUE)
view(Ciota14_ldt_lab)

Ciota14_ldt_field <- read.csv("./data/larvalddt/Ciota14_ldt_field.csv",TRUE)
view(Ciota14_ldt_field)

Loetti11_ldt <- read.csv("./data/larvalddt/Loetti11_ldt.csv",TRUE)
view(Loetti11_ldt)
names(Loetti11_ldt) [1] <- "temperature"
#weird symbols again

Ruybal16_ldt <- read.csv("./data/larvalddt/Ruybal16_ldt.csv",TRUE)
view(Ruybal16_ldt)

Madder83_ldt_field <- read.csv("./data/larvalddt/Madder83_ldt_field.csv",TRUE)
view(Madder83_ldt_field)
names(Madder83_ldt_field) [1] <- "temperature"

Madder83_ldt_lab <- read.csv("./data/larvalddt/Madder83_ldt_lab.csv",TRUE)
view(Madder83_ldt_lab)
names(Madder83_ldt_lab) [1] <- "temperature"


##-- COMBINE AND ORDER BY TEMPERATURE (NEW COLUMN FOR DEVELOPMENT RATE)---##

merged_ldt <- bind_rows(list(Ciota14_ldt_field,
                             Ciota14_ldt_lab,
                             Loetti11_ldt,
                             Ruybal16_ldt,
                             Madder83_ldt_field,
                             Madder83_ldt_lab
), .id = "source")
view(merged_ldt)

order(merged_ldt$temperature, decreasing=FALSE)
#orders dataset by temp

merged_ldt_o <- merged_ldt[order(merged_ldt$temperature , decreasing=FALSE),]
head(merged_ldt_o)

write.csv(merged_ldt_o, file = "mo_ldr_D.csv")


ggplot(data = merged_ldt_o, aes(x = temperature, y = ldt, color=source)) +
  geom_point() +
  theme_bw() +
  labs(title = "Larval development time vs temperature",
       x = "Temperature",
       y = "Larval development time (days)",
       color = "source")
#To see which papers are causing problems 

##-- Create new rate column
merged_ldt_o$ldr <- 1/(merged_ldt_o$ldt)
view(merged_ldt_o)

ggplot(data = merged_ldt_o, aes(x = temperature, y = ldr, color=source)) +
  geom_point() +
  theme_bw() +
  labs(title = "Larval development rate vs temperature",
       x = "Temperature",
       y = "Larval development rate",
       color = "source")


##-- LINEAR PLOT - LARVAL DEVELOPMENT RATE ONLY -##


##-- INCLUDING WEIGHTS

temperature <- merged_ldt_o$temperature
ldr <- merged_ldt_o$ldr
weight <- merged_ldt_o$weight

plot(temperature, ldr, pch=16, main = "Linear Model - Larval Development Rate vs Temperature", xlab = "Temperature (Celsius)", ylab = "Larval Development Rate")

lm_ldr <- lm(ldr~temperature, data= merged_ldt_o, weights = weight)
lm_ldr

lines(temperature, predict(lm_ldr, list(temperature)))

AICc(lm_ldr, k=2)
#-277.8033 (-270.103 without weights)




##-- QUADRATIC --##


q_ldr <- function(q, temperature, tmin, tmax) {-q*(temperature-tmin)*(temperature-tmax)}

q_ldr(2,2,15,40)
#-- check eq works

m.q_ldr <- nls(ldr~q_ldr(q, temperature, tmin, tmax), data=merged_ldt_o, start=list(q=-0.1, tmin=7, tmax=35), weights = weight, trace=T)

plot(temperature, ldr, pch=16, main= "Quadratic Model - Larval Development Rate vs Temperature", xlab = "Temperature (Celsius)", ylab = "Larval development rate")

lines(temperature, predict(m.q_ldr, list(temperature)))

AICc(m.q_ldr, k=2)
#-12535.64 (-282.967 without weights)



##-- BRIERE  

b_ldr_eq <- function(q, temperature, tmin, tmax) {-q*temperature*(temperature-tmin)*((tmax-temperature)^(1/2))*(temperature<=tmax)*(temperature>=tmin)} 

b_ldr_eq(2,2,15,40)


m.b_ldr <- nls(ldr~b_ldr_eq(q, temperature, tmin, tmax), data=merged_ldt_o, start=list(q=0.1, tmin=7, tmax=40), weights = weight, trace=T, algorithm = "port")
#Error in numericDeriv(form[[3L]], names(ind), env) : 
#Missing value or an infinity produced when evaluating the model
m.b_ldr

plot(temperature, ldr, pch=16, main = "Briere Model - Larval Development Rate vs Temperature", xlab = "Temperature (Celsius)", ylab = "Larval development rate")
lines(temperature, lwd = 3, col="lightseagreen", predict(m.b_ldr, list(temperature)))

AICc(m.b_ldr, k=2)
#-12537.16 (-295.5255 without weights)




##-- FINAL GRAPH - BRIERE
q <- function(q, temperature, tmin, tmax) {-q*(temperature-tmin)*(temperature-tmax)}
t <- seq(-10, 50, by=0.1)

plot(temperature, ldr, xlim=c(0, 40) , ylim=c(0,0.15), main= "Larval Development Rate (LDR)", xlab = "Temperature (Celsius)", ylab = "Larval development rate (LDR)")

p_a <- predict(m.b_ldr, list(temperature=t))


f <- ifelse(p_a<0, 0, ifelse(p_a>1, 1, p_a))
f[is.na(f)] = 0
lines(t, lwd = 3, col="lightseagreen", f)


