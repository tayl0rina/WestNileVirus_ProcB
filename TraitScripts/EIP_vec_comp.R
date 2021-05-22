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

##-- IMPORT DATASETS --##

Dohm02_EIP_veccomp <- read.csv("./data/EIP_veccomp/Dohm02_EIP_veccomp.csv",TRUE)
names(Dohm02_EIP_veccomp) [1] <- "temperature"
view(Dohm02_EIP_veccomp)

Kilpatrick08_EIP_veccomp <- read.csv("./data/EIP_veccomp/Kilpatrick08_EIP_veccomp.csv",TRUE)
names(Kilpatrick08_EIP_veccomp) [1] <- "temperature"
view(Kilpatrick08_EIP_veccomp)

##-- COMBINE AND ORDER ---##

m_EIP_veccomp <- bind_rows(list(Kilpatrick08_EIP_veccomp,
                                   Dohm02_EIP_veccomp
                                   
), .id = "source")
view(m_EIP_veccomp)

order(m_EIP_veccomp$temperature, decreasing=FALSE)
#orders dataset by temp

m_EIP_veccomp_o <- m_EIP_veccomp[order(m_EIP_veccomp$temperature , decreasing=FALSE),]
head(m_EIP_veccomp_o)

write.csv(m_EIP_veccomp_o, file = "mo_EIP_veccomp.csv")

ggplot(data = m_EIP_veccomp_o, aes(x = temperature, y = vec_comp, color=source)) +
  geom_point() +
  theme_bw() +
  labs(title = "EIP or veccomp vs temperature",
       x = "Temperature",
       y = "EIP or vec comp",
       color = "source")
#To see which papers are different 


##-- EXTRINSIC INCUBATION FIRST _______________________________________________________________________________________________________

##-- LINEAR PLOT --##

##-- INCLUDING WEIGHTS

temperature <- m_EIP_veccomp_o$temperature
EIP <- m_EIP_veccomp_o$EIP
vec_comp <- m_EIP_veccomp_o$vec_comp
weight <- m_EIP_veccomp_o$weight

plot(temperature, EIP, pch=16, main = "Linear Model - EIP vs Temperature", xlab = "Temperature (Celsius)", ylab = "EIP")

lm_EIP <- lm(EIP~temperature, data= m_EIP_veccomp_o, weights = weight)
lm_EIP

lines(temperature, predict(lm_EIP, list(temperature)))

AICc(lm_EIP, k=2)
#94.6952


##-- QUADRATIC --##


q <- function(q, temperature, tmin, tmax) {-q*(temperature-tmin)*(temperature-tmax)}

#q_lsurvival(2,2,15,40)
#-- check eq works

m.q_EIP <- nls(EIP~q(q, temperature, tmin, tmax), data=m_EIP_veccomp_o, start=list(q=-0.1, tmin=7, tmax=50), weights = weight, trace=T)
m.q_EIP
plot(temperature, EIP, main= "Extrinsic Incubation Period", xlab = "Temperature (Celsius)", ylab = "EIP")

lines(temperature, lwd = 3, col="lightseagreen", predict(m.q_EIP, list(temperature)))

AICc(m.q_EIP, k=2)
#-504.3944


##-- BRIERE --NOT USED

b_eq <- function(q, temperature, tmin, tmax) {-q*temperature*(temperature-tmin)*((tmax-temperature)^(1/2))*(temperature<=tmax)*(temperature>=tmin)} 

#b_lsurvival_eq(2,2,15,40)

m.b_EIP <- nls(EIP~b_eq(q, temperature, tmin, tmax), data=m_EIP_veccomp_o, start=list(q=0.1, tmin=1, tmax=40), weights = weight, trace=T, algorithm = "port")
#Error in numericDeriv(form[[3L]], names(ind), env) : 
#Missing value or an infinity produced when evaluating the model
m.b_EIP

#plot(temperature, lsurvival, pch=16, main = "Briere Model - Larval Survival vs Temperature", xlab = "Temperature (Celsius)", ylab = "Larval Survival")
#lines(temperature, predict(m.b_lsurvival, list(temperature)))

#AICc(m.b_lsurvival, k=2)





##-- FINAL GRAPH -- QUADRATIC
q <- function(q, temperature, tmin, tmax) {-q*(temperature-tmin)*(temperature-tmax)}
t <- seq(0, 40, by=0.1)

plot(temperature, EIP, xlim=c(0, 40) , ylim=c(0,40), main= "Extrinsic Incubation period (EIP)", xlab = "Temperature (Celsius)", ylab = "EIP (days)")

p_a <- predict(m.q_EIP, list(temperature=t))


f <- ifelse(p_a<0, 0, p_a)
lines(t, lwd = 3, col="lightseagreen", f)



##-- VECTOR COMPETENCE ___________________________________________________________________________________________________________________________________________


##-- LINEAR PLOT --##-- VEC_COMP --##

##-- INCLUDING WEIGHTS

temperature <- m_EIP_veccomp_o$temperature
EIP <- m_EIP_veccomp_o$EIP
vec_comp <- m_EIP_veccomp_o$vec_comp
weight <- m_EIP_veccomp_o$weight

plot(temperature, vec_comp, pch=16, main = "Linear Model - vec_comp vs Temperature", xlab = "Temperature (Celsius)", ylab = "EIP")

lm_vec_comp <- lm(vec_comp~temperature, data= m_EIP_veccomp_o, weights = weight)
lm_vec_comp

lines(temperature, predict(lm_vec_comp, list(temperature)))

AICc(lm_vec_comp, k=2)
#-6.220129


##-- QUADRATIC --##


q <- function(q, temperature, tmin, tmax) {-q*(temperature-tmin)*(temperature-tmax)}

#q_lsurvival(2,2,15,40)
#-- check eq works

m.q_vec_comp <- nls(vec_comp~q(q, temperature, tmin, tmax), data=m_EIP_veccomp_o, start=list(q=-0.1, tmin=7, tmax=50), weights = weight, trace=T)

plot(temperature, vec_comp, main= "Vector Competence (bc)", xlab = "Temperature (Celsius)", ylab = "Vector Competence")

lines(temperature, lwd = 3, col="lightseagreen", predict(m.q_vec_comp, list(temperature)))

AICc(m.q_vec_comp, k=2)
#-610.5065


##-- BRIERE-- FINAL  

b_eq <- function(q, temperature, tmin, tmax) {-q*temperature*(temperature-tmin)*((tmax-temperature)^(1/2))*(temperature<=tmax)*(temperature>=tmin)} 
#

#b_eq_restricted <- restrict(b_eq, method = c("ser", "manual"), thresh = 1.0, resmat = NULL)

#b_lsurvival_eq(2,2,15,40)

m.b_vec_comp <- nls(vec_comp~b_eq(q, temperature, tmin, tmax), data=m_EIP_veccomp_o, start=list(q=0.1, tmin=1, tmax=40), weights = weight, trace=T, algorithm = "port")
#Error in numericDeriv(form[[3L]], names(ind), env) : 
#Missing value or an infinity produced when evaluating the model
m.b_vec_comp


plot(temperature, vec_comp, pch=16, main = "Briere Model - Vector Competence vs Temperature", xlab = "Temperature (Celsius)", ylab = "Vector competence")
lines(temperature, predict(m.b_vec_comp, list(temperature)))

AICc(m.b_vec_comp, k=2)
#-613.103



##-- FINAL GRAPH -- VEC COMP -- BRIERE

q <- function(q, temperature, tmin, tmax) {-q*(temperature-tmin)*(temperature-tmax)}
t <- seq(0, 40, by=0.1)

plot(temperature, vec_comp, xlim=c(0, 40) , ylim=c(0,1), main= "Vector Competence (bc)", xlab = "Temperature (Celsius)", ylab = "Vector competence (bc)")

p_a <- predict(m.b_vec_comp, list(temperature=t))


f <- ifelse(p_a<0, 0, ifelse(p_a>1, 1, p_a))
f[is.na(f)] = 0
lines(t, lwd = 3, col="lightseagreen", f)
