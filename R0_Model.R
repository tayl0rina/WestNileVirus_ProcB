##-- R0 Model! --##

library(gapminder)
library(epiDisplay)
library(magrittr)
library(foreign)
library(psych)
library(ggplot2)
library(AICcmodavg)
library(tidyverse)


getwd()
setwd("~/Vet Epi/MSc Project/Model")



host_pref <- 0.81
#host preference
rb <- 0.194
#recovery rate of birds




temperature <- seq(-10, 50, by=0.1)
view(temperature)

linear <- function(q, temperature, c) {q*temperature+c}
quadratic <- function(q, temperature, tmin, tmax) {-q*(temperature-tmin)*(temperature-tmax)*(temperature<=tmax)*(temperature>=tmin)}
briere <- function(q, temperature, tmin, tmax) {q*temperature*(temperature-tmin)*((tmax-temperature)^(1/2))*(temperature<=tmax)*(temperature>=tmin)}



# Bite Rate -a-
a <- quadratic(6.744986e-05, temperature, 8.720364e+00, 2.506042e+02)
plot(temperature, a)


# Transmission prob -bc-
bc_notcapped <- briere(0.001025, temperature, 14.016973, 34.763334)

bc <- ifelse(bc_notcapped>1,1,bc_notcapped)
bc[is.na(bc)] = 0
plot(temperature, bc)
bc
#prevents exceeding 1

# Mortality rate -u- 
lf_notcapped <- linear(-5.236, temperature, 178.315 )
plot (temperature, lf_notcapped)

#lf <- ifelse(lf_notcapped<0, 0, lf_notcapped)
#only capped the mortality rate 

plot (temperature, lf)

u_notcapped <- 1/lf_notcapped
plot(temperature, u_notcapped)

u <- ifelse(u_notcapped>1000,1000,ifelse(u_notcapped<0, 1000, u_notcapped))

plot (temperature, u)
plot (temperature, u_notcapped)
view(table (temperature, u))


# EIP
EIP <- quadratic( 0.136, temperature,  5.429, 34.362 )
plot(temperature, EIP)



# Fecundity -F (small f for this)-

f_uc <- quadratic(0.6355, temperature,  5.9923, 38.9397 )
f <- ifelse(f_uc<0, 0, f_uc)

plot(temperature, f)


# Larval survival to adult probability -S-
S <- quadratic(0.004351201, temperature,  7.543676344, 36.449676942)
plot(temperature, S)


## Egg to adult development rate -D- 


# Larval to adult development rate -ldr-
ldr <- briere(6.082e-05, temperature,  5.411e+00,  3.655e+01)
ldr[is.na(ldr)] = 0
plot(temperature, ldr)

#egg dev rate -edr-
edr_nc <- linear(0.03097, temperature, -0.20664)
edr <- ifelse(edr_nc<0, 0, edr_nc)
plot(temperature, edr)

# -D- 

D <- (ldr*edr)/(ldr+edr)
D[is.na(D)] = 0
plot(temperature, D)

# Egg Viability -V-
V <- quadratic(0.002648737, temperature, -0.909808636, 34.259688479) 
plot(temperature, V)


# Proportion ovipositing -O-
#O_continuous <- linear(-0.02945, temperature, 1.12068)

#O<- ifelse(O_continuous>1,1, ifelse(O<0, 0, O_continuous))
#plot(temperature, O)



##-- R0 EQUATION --##
R0 <- (((a^3)*host_pref*bc*exp(-u*EIP)*f*V*S*D)/(rb*u^3))^(1/2)
plot(temperature, R0)
R0
view(R0)

R0_rel <- R0/(max(R0, na.rm = TRUE))
plot(temperature, R0_rel, xlim=c(0, 40), ylim=c(0,1) , main=expression("Relative R"[0]), xlab = "Temperature (Celsius)", ylab =expression("Relative R"[0]), type = "n")
R0_rel
lines(temperature, lwd = 3, col="lightseagreen", R0_rel)


R0[is.na(R0)] = 0

R0_tab <- data.frame("temperature" = temperature, "R0" = R0)
view(R0_tab)

write.csv(R0_tab, file = "R0_temp.csv")
