#sensitivity_analysis2

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

temperature <- seq(-10, 50, by=0.01)

d_linear <- function(q) {-q}
d_quadratic <- function(q, temperature, tmin, tmax) {q*(tmin+tmax-(2*temperature))}
d_briere <- function(q, temperature, tmin, tmax) {q*(
  ((temperature-tmin)*(tmax-temperature)+temperature*(tmax-temperature)-(1/2)*temperature*(temperature-tmin))/
    ((tmax-temperature)^0.5))}




# bite rate -a-
da_dT <- d_quadratic(6.744986e-05, temperature, 8.720364e+00, 2.506042e+02)

dR0_da <- (3/(2*a))*R0

da <- dR0_da*da_dT

plot(temperature, da)
plot(temperature, R0)


# Vector competence -bc-
dbc_dT <- d_briere(0.001025, temperature, 14.016973, 34.763334)

dR0_dbc <- R0/(2*bc)

dbc <- dR0_dbc*dbc_dT

plot(temperature, dbc)

#(havent capped as did in the normal R0 equation)



#mortality rate -u-


dlf_dT <- d_linear(5.236)
rep_dlf_dT <- rep(dlf_dT, times=length(temperature))
du_dT <- -dlf_dT*u^2

plot(temperature, du_dT)


u <- ifelse(u_notcapped<0, 0, u_notcapped)
#not capped at 1 but means that R0 equation does not have values beyond 35deg. old code below.

#u <- ifelse(u_notcapped>1,1,ifelse(u_notcapped<0, 0, u_notcapped))

dR0_du <- (-1/2)*((3/u)+EIP)*R0
plot(temperature, dR0_du)

du <- du_dT*dR0_du

plot(temperature, du)

# extrinsic incubation period -EIP- 
dEIP_dT <- d_quadratic( 0.136, temperature,  5.429, 34.362 )

dr0_dEIP <- -(1/2)*u*R0

dEIP <- dEIP_dT*dr0_dEIP

plot(temperature, dEIP)


#egg viability -V- 
dV_dT <- d_quadratic(0.002648737, temperature, -0.909808636, 34.259688479) 

dR0_dV <- (1/(2*V))*R0

dV <- dV_dT*dR0_dV

plot(temperature, dV)


# fecundity -f- 
df_dT <- d_quadratic(0.6355, temperature,  5.9923, 38.9397 )

dR0_df <- (1/(2*f_uc))*R0

df <- df_dT*dR0_df

plot(temperature, df)


# larval to adult survival prob -S-
dS_dT <- d_quadratic(0.004351201, temperature,  7.543676344, 36.449676942)

dR0_dS <- (1/(2*S))*R0

dS <- dS_dT*dR0_dS

plot(temperature, dS)



# development rate - E and L DR - 

dldr_dT <- d_briere(6.082e-05, temperature,  5.411e+00,  3.655e+01)
dedr_dT <- d_linear(0.03097)
length(dldr_dT)
length(dedr_dT)

dD_dT <- ((edr_nc^2)*dldr_dT)+((ldr^2)*dedr_dT)/((ldr+edr_nc)^2)


dR0_dD <- (1/(2*D))*R0

dD <- dD_dT*dR0_dD

plot(temperature, dD)
#???? strange


# R0
dR0_dT <- da+dbc+du+dEIP+dV+df+dS+dD


plot(temperature, dR0_dT)


y0 <- replicate(6001, 0)
#so same length as temp

plot(temperature, xlim=c(10, 35), ylim=c(-36, 24) ,  
     #main="Sensitivity Analysis", 
     xlab = "Temperature (Celsius)", 
     ylab ="dRR0/dT", type = "n")

lines(temperature, lwd = 3, col=1, dR0_dT)
lines(temperature, lwd = 3, col='lightgreen', da)
lines(temperature, lwd = 3, col='cornflowerblue', dbc)
lines(temperature, lwd = 3, col='lightpink1', du)
lines(temperature, lwd = 3, col='turquoise2', dEIP)
lines(temperature, lwd = 3, col='darkgoldenrod1', dV)
lines(temperature, lwd = 3, col='darkorchid', df)
lines(temperature, lwd = 3, col='gray', dS)
lines(temperature, lwd = 3, col='sienna2', dD)

lines(temperature, lwd = 2, lty=2, col=1, y0)
lines(temperature, lwd = 3, col=1, dR0_dT)


legend(9.5, -3, legend=c("dRR0/dT", "a", "bc", "u", "EIP", "V", "f", "S", "D"),
       col=c(1, 'lightgreen', 'cornflowerblue','lightpink1','turquoise2','darkgoldenrod1','darkorchid','gray','sienna2'), 
       lty=1, lwd=4, cex=1)




#Doesnt work 
#temp_df <- data.frame(temperature,da,dbc,du,dEIP,dV,df,dS,dD)

#ggplot(temp_df, aes(x=temperature), na.rm=F) +
#  xlim=(0,35) +
 # geom_line(aes(y = dR0_dT), color = "darkred") + 
#      geom_line(aes(y = da), color = "darkred") + 
#      geom_line(aes(y = dbc), color = "darkred") + 
#      geom_line(aes(y = du), color = "darkred") + 
#      geom_line(aes(y = dEIP), color = "darkred") + 
#      geom_line(aes(y = dV), color = "darkred") + 
#      geom_line(aes(y = df), color = "darkred") + 
#      geom_line(aes(y = dS), color = "darkred") + 
#      geom_line(aes(y = dD), color = "darkred") + 
#      geom_line(aes(y = 0), color = "darkred", linetype='dashed') + 
#  scale_x_continuous(breaks=seq(0,35,5)) +
#  theme(axis.text=element_text(size=12),
#        axis.title=element_text(size=14,face="bold")) +
#  guides(fill=FALSE) +
#  labs(title = "Sensitivity Analysis",
#       x = "Temperature (Celsius)",
#       y = "dR0/dT")
#view(temp_df)
