##-- Map attempt --##

library(gapminder)
library(epiDisplay)
library(magrittr)
library(foreign)
library(psych)
library(ggplot2)
library(AICcmodavg)
library(ncdf4)
library(raster)
library(data.table)
library(tidyverse)
getwd()
setwd("C:\\Users\\Gabriella\\Documents\\Vet Epi\\MSc Project\\Map\\Temperature")


nc <- nc_open("C:\\Users\\Gabriella\\Documents\\Vet Epi\\MSc Project\\Map\\Temperature\\tg_ens_mean_0.1deg_reg_2011-2019_v21.0e.nc")
print(nc)

#nc <- nc_open("L:/R&D/CERA/GISTeam/Data/ECADs/tn_0.25deg_day_2019_grid_ensmean.nc")


time <- ncdf4::ncvar_get(nc, varid="time")

time_d <- as.Date(time, format="%j", origin=as.Date("1950-01-01"))

b <- brick('tg_ens_mean_0.1deg_reg_2011-2019_v21.0e.nc')
#b <- brick("L:/R&D/CERA/GISTeam/Data/ECADs/tn_0.25deg_day_2019_grid_ensmean.nc")
b

crs(b, asText=FALSE)
#gives projection


#_________________________________________________________________________________________________________________________________________________________________

##-- R0 map edits for each month - 

##-- MAY --##
b_may <- b[[which(getZ(b) >= as.Date("2019-05-01") & getZ(b) <= as.Date("2019-05-31"))]]
b_may

b_may_ave <- mean(b_may, na.rm = TRUE)
b_may_ave
plot(b_may_ave)

writeRaster(b_may_ave , "may19_temp.nc", format = "CDF", overwrite = TRUE)




tempmap <- values(b_may_ave)

#Change this^ and re-run down to line for each month.


linear <- function(q, tempmap, c) {q*tempmap+c}
quadratic <- function(q, tempmap, tmin, tmax) {-q*(tempmap-tmin)*(tempmap-tmax)*(tempmap<=tmax)*(tempmap>=tmin)}
briere <- function(q, tempmap, tmin, tmax) {q*tempmap*(tempmap-tmin)*((tmax-tempmap)^(1/2))*(tempmap<=tmax)*(tempmap>=tmin)}

### Rachel's changes
tempR <- seq(-10, 50, by = 0.01) #increased res to 0.01
tempmapR <- round(tempmap, 2) # since we have done tempR to 0.01, we need tempmap to be the same
#lue of R0 corresponds to the value for tempR so we need to line up the values of tempmapR with tempR

#R0map <- R0[match(as.character(tempmapR), as.character(tempR))]
R0map <- Rvec[match(as.character(tempmapR), as.character(tempR))] 



length(R0map)

# for some reason the matching didn't work when they were numeric, so switched to character to match
#so the match finds the index of tempR which has that temp, and then since R0 is in the same order as tempR, we can extract R0 with that index too

#change tempmap above, re-run code up to line and then code below

R0map_may19 <- b_may_ave
values(R0map_may19) <- R0map
plot(R0map_may19)

writeRaster(R0map_may19 , "may19_R0.nc", format = "CDF", overwrite = TRUE)



#and another map indicating areas permissive for wnv
R0_may19 <- values(R0map_may19)
perm_may19 <- ifelse(R0_may19>0, 1, 
                     ifelse(is.na(R0_may19), is.na(R0_may19), 0))

values(R0map_may19) <- perm_may19


plot(R0map_may19)

writeRaster(R0map_may19 , "may19_R0perm.nc", format = "CDF", overwrite = TRUE)


#high risk areas (R0 >=0.75)

perm_may19 <- ifelse(R0_may19>=0.75, 1, 
                     ifelse(is.na(R0_may19), is.na(R0_may19), 0))

values(R0map_may19) <- perm_may19


plot(R0map_may19)

writeRaster(R0map_may19 , "may19_R0high.nc", format = "CDF", overwrite = TRUE)


#med risk areas (R0 >=0.5)

perm_may19 <- ifelse(R0_may19>=0.5, 1, 
                     ifelse(is.na(R0_may19), is.na(R0_may19), 0))

values(R0map_may19) <- perm_may19


plot(R0map_may19)

writeRaster(R0map_may19 , "may19_R0high.nc", format = "CDF", overwrite = TRUE)


#________________________________________________________________________________________________________________________
##-- JUNE --##

b_jun <- b[[which(getZ(b) >= as.Date("2019-06-01") & getZ(b) <= as.Date("2019-06-30"))]]
b_jun

b_jun_ave <- mean(b_jun, na.rm = TRUE)
b_jun_ave
plot(b_jun_ave)

writeRaster(b_jun_ave , "jun19_temp.nc", format = "CDF", overwrite = FALSE)


tempmap <- values(b_jun_ave)

#Change this^ and re-run down to line for each month.


linear <- function(q, tempmap, c) {q*tempmap+c}
quadratic <- function(q, tempmap, tmin, tmax) {-q*(tempmap-tmin)*(tempmap-tmax)*(tempmap<=tmax)*(tempmap>=tmin)}
briere <- function(q, tempmap, tmin, tmax) {q*tempmap*(tempmap-tmin)*((tmax-tempmap)^(1/2))*(tempmap<=tmax)*(tempmap>=tmin)}

### Rachel's changes
tempR <- seq(-10, 50, by = 0.01) #increased res to 0.01
tempmapR <- round(tempmap, 2) # since we have done tempR to 0.01, we need tempmap to be the same
#lue of R0 corresponds to the value for tempR so we need to line up the values of tempmapR with tempR

#R0map <- R0[match(as.character(tempmapR), as.character(tempR))]
R0map <- Rvec[match(as.character(tempmapR), as.character(tempR))] 


R0map_jun19 <- b_jun_ave
values(R0map_jun19) <- R0map
plot(R0map_jun19)

writeRaster(R0map_jun19 , "jun19_R0.nc", format = "CDF", overwrite = F)


#and another map indicating areas permissive for wnv

R0_jun19 <- values(R0map_jun19)
perm_jun19 <- ifelse(R0_jun19>0, 1, 
                     ifelse(is.na(R0_jun19), is.na(R0_jun19), 0))

values(R0map_jun19) <- perm_jun19


plot(R0map_jun19)

writeRaster(R0map_jun19 , "jun19_R0perm.nc", format = "CDF", overwrite = TRUE)

#high risk


perm_jun19 <- ifelse(R0_jun19>=0.75, 1, 
                     ifelse(is.na(R0_jun19), is.na(R0_jun19), 0))

values(R0map_jun19) <- perm_jun19


plot(R0map_jun19)

writeRaster(R0map_jun19 , "jun19_R0high.nc", format = "CDF", overwrite = TRUE)


#med risk
perm_jun19 <- ifelse(R0_jun19>=0.5, 1, 
                     ifelse(is.na(R0_jun19), is.na(R0_jun19), 0))

values(R0map_jun19) <- perm_jun19


plot(R0map_jun19)

writeRaster(R0map_jun19 , "jun19_R0high.nc", format = "CDF", overwrite = TRUE)


#________________________________________________________________________________________________________________________
##-- JULY --## 

b_jul <- b[[which(getZ(b) >= as.Date("2019-07-01") & getZ(b) <= as.Date("2019-07-31"))]]
b_jul

b_jul_ave <- mean(b_jul, na.rm = FALSE)
b_jul_ave
plot(b_jul_ave)

writeRaster(b_jul_ave , "jul19_temp.nc", format = "CDF", overwrite = FALSE)


tempmap <- values(b_jul_ave)

#Change this^ and re-run down to line for each month.


linear <- function(q, tempmap, c) {q*tempmap+c}
quadratic <- function(q, tempmap, tmin, tmax) {-q*(tempmap-tmin)*(tempmap-tmax)*(tempmap<=tmax)*(tempmap>=tmin)}
briere <- function(q, tempmap, tmin, tmax) {q*tempmap*(tempmap-tmin)*((tmax-tempmap)^(1/2))*(tempmap<=tmax)*(tempmap>=tmin)}

### Rachel's changes
tempR <- seq(-10, 50, by = 0.01) #increased res to 0.01
tempmapR <- round(tempmap, 2) # since we have done tempR to 0.01, we need tempmap to be the same
#lue of R0 corresponds to the value for tempR so we need to line up the values of tempmapR with tempR

#R0map <- R0[match(as.character(tempmapR), as.character(tempR))]
R0map <- Rvec[match(as.character(tempmapR), as.character(tempR))] 


R0map_jul19 <- b_jul_ave
values(R0map_jul19) <- R0map
plot(R0map_jul19)

writeRaster(R0map_jul19 , "jul19_R0.nc", format = "CDF", overwrite = F)

#permissive R0

R0_jul19 <- values(R0map_jul19)
perm_jul19 <- ifelse(R0_jul19>0, 1, 
                     ifelse(is.na(R0_jul19), is.na(R0_jul19), 0))

values(R0map_jul19) <- perm_jul19


plot(R0map_jul19)

writeRaster(R0map_jul19 , "jul19_R0perm.nc", format = "CDF", overwrite = TRUE)

#high risk
perm_jul19 <- ifelse(R0_jul19>=0.75, 1, 
                     ifelse(is.na(R0_jul19), is.na(R0_jul19), 0))

values(R0map_jul19) <- perm_jul19


plot(R0map_jul19)

writeRaster(R0map_jul19 , "jul19_R0high.nc", format = "CDF", overwrite = TRUE)

#med risk
perm_jul19 <- ifelse(R0_jul19>=0.5, 1, 
                     ifelse(is.na(R0_jul19), is.na(R0_jul19), 0))

values(R0map_jul19) <- perm_jul19


plot(R0map_jul19)

writeRaster(R0map_jul19 , "jul19_R0high.nc", format = "CDF", overwrite = TRUE)


#________________________________________________________________________________________________________________________
##-- AUGUST --##

b_aug <- b[[which(getZ(b) >= as.Date("2019-08-01") & getZ(b) <= as.Date("2019-08-31"))]]
b_aug

b_aug_ave <- mean(b_aug, na.rm = TRUE)
b_aug_ave
plot(b_aug_ave)

writeRaster(b_aug_ave , "aug19_temp.nc", format = "CDF", overwrite = FALSE)


tempmap <- values(b_aug_ave)

#Change this^ and re-run down to line for each month.


linear <- function(q, tempmap, c) {q*tempmap+c}
quadratic <- function(q, tempmap, tmin, tmax) {-q*(tempmap-tmin)*(tempmap-tmax)*(tempmap<=tmax)*(tempmap>=tmin)}
briere <- function(q, tempmap, tmin, tmax) {q*tempmap*(tempmap-tmin)*((tmax-tempmap)^(1/2))*(tempmap<=tmax)*(tempmap>=tmin)}

### Rachel's changes
tempR <- seq(-10, 50, by = 0.01) #increased res to 0.01
tempmapR <- round(tempmap, 2) # since we have done tempR to 0.01, we need tempmap to be the same
#lue of R0 corresponds to the value for tempR so we need to line up the values of tempmapR with tempR

#R0map <- R0[match(as.character(tempmapR), as.character(tempR))]
R0map <- Rvec[match(as.character(tempmapR), as.character(tempR))] 


R0map_aug19 <- b_aug_ave
values(R0map_aug19) <- R0map
plot(R0map_aug19)

writeRaster(R0map_aug19 , "aug19_R0.nc", format = "CDF", overwrite = T)

#permissive R0

R0_aug19 <- values(R0map_aug19)
perm_aug19 <- ifelse(R0_aug19>0, 1, 
                     ifelse(is.na(R0_aug19), is.na(R0_aug19), 0))

values(R0map_aug19) <- perm_aug19


plot(R0map_aug19)

writeRaster(R0map_aug19 , "aug19_R0perm.nc", format = "CDF", overwrite = T)

#high risk

perm_aug19 <- ifelse(R0_aug19>=0.75, 1, 
                     ifelse(is.na(R0_aug19), is.na(R0_aug19), 0))

values(R0map_aug19) <- perm_aug19


plot(R0map_aug19)

writeRaster(R0map_aug19 , "aug19_R0high.nc", format = "CDF", overwrite = F)

#med risk
perm_aug19 <- ifelse(R0_aug19>=0.5, 1, 
                     ifelse(is.na(R0_aug19), is.na(R0_aug19), 0))

values(R0map_aug19) <- perm_aug19


plot(R0map_aug19)

writeRaster(R0map_aug19 , "aug19_R0high.nc", format = "CDF", overwrite = F)

#________________________________________________________________________________________________________________________
##-- SEPTEMBER --##

b_sep <- b[[which(getZ(b) >= as.Date("2019-09-01") & getZ(b) <= as.Date("2019-09-30"))]]
b_sep

b_sep_ave <- mean(b_sep, na.rm = TRUE)
b_sep_ave
plot(b_sep_ave)

writeRaster(b_sep_ave , "sep19_temp.nc", format = "CDF", overwrite = FALSE)


tempmap <- values(b_sep_ave)

#Change this^ and re-run down to line for each month.


linear <- function(q, tempmap, c) {q*tempmap+c}
quadratic <- function(q, tempmap, tmin, tmax) {-q*(tempmap-tmin)*(tempmap-tmax)*(tempmap<=tmax)*(tempmap>=tmin)}
briere <- function(q, tempmap, tmin, tmax) {q*tempmap*(tempmap-tmin)*((tmax-tempmap)^(1/2))*(tempmap<=tmax)*(tempmap>=tmin)}

### Rachel's changes
tempR <- seq(-10, 50, by = 0.01) #increased res to 0.01
tempmapR <- round(tempmap, 2) # since we have done tempR to 0.01, we need tempmap to be the same
#lue of R0 corresponds to the value for tempR so we need to line up the values of tempmapR with tempR

#R0map <- R0[match(as.character(tempmapR), as.character(tempR))]
R0map <- Rvec[match(as.character(tempmapR), as.character(tempR))] 


R0map_sep19 <- b_sep_ave
values(R0map_sep19) <- R0map
plot(R0map_sep19)

writeRaster(R0map_sep19 , "sep19_R0.nc", format = "CDF", overwrite = F)

#permissive R0

R0_sep19 <- values(R0map_sep19)
perm_sep19 <- ifelse(R0_sep19>0, 1, 
                     ifelse(is.na(R0_sep19), is.na(R0_sep19), 0))

values(R0map_sep19) <- perm_sep19


plot(R0map_sep19)

writeRaster(R0map_sep19 , "sep19_R0perm.nc", format = "CDF", overwrite = TRUE)

#high risk
perm_sep19 <- ifelse(R0_sep19>=0.75, 1, 
                     ifelse(is.na(R0_sep19), is.na(R0_sep19), 0))

values(R0map_sep19) <- perm_sep19


plot(R0map_sep19)

writeRaster(R0map_sep19 , "sep19_R0high.nc", format = "CDF", overwrite = TRUE)

#med risk
perm_sep19 <- ifelse(R0_sep19>=0.5, 1, 
                     ifelse(is.na(R0_sep19), is.na(R0_sep19), 0))

values(R0map_sep19) <- perm_sep19


plot(R0map_sep19)

writeRaster(R0map_sep19 , "sep19_R0high.nc", format = "CDF", overwrite = TRUE)
            
            

#________________________________________________________________________________________________________________________
##-- OCTOBER --##
b_oct <- b[[which(getZ(b) >= as.Date("2019-10-01") & getZ(b) <= as.Date("2019-10-31"))]]
b_oct

b_oct_ave <- mean(b_oct, na.rm = TRUE)
b_oct_ave
plot(b_oct_ave)

writeRaster(b_oct_ave , "oct19_temp.nc", format = "CDF", overwrite = FALSE)

tempmap <- values(b_oct_ave)

#Change this^ and re-run down to line for each month.


linear <- function(q, tempmap, c) {q*tempmap+c}
quadratic <- function(q, tempmap, tmin, tmax) {-q*(tempmap-tmin)*(tempmap-tmax)*(tempmap<=tmax)*(tempmap>=tmin)}
briere <- function(q, tempmap, tmin, tmax) {q*tempmap*(tempmap-tmin)*((tmax-tempmap)^(1/2))*(tempmap<=tmax)*(tempmap>=tmin)}

### Rachel's changes
tempR <- seq(-10, 50, by = 0.01) #increased res to 0.01
tempmapR <- round(tempmap, 2) # since we have done tempR to 0.01, we need tempmap to be the same
#lue of R0 corresponds to the value for tempR so we need to line up the values of tempmapR with tempR

#R0map <- R0[match(as.character(tempmapR), as.character(tempR))]
R0map <- Rvec[match(as.character(tempmapR), as.character(tempR))] 

R0map_oct19 <- b_oct_ave
values(R0map_oct19) <- R0map
plot(R0map_oct19)

writeRaster(R0map_oct19 , "oct19_R0.nc", format = "CDF", overwrite = T)

#permissive R0

R0_oct19 <- values(R0map_oct19)
perm_oct19 <- ifelse(R0_oct19>0, 1, 
                     ifelse(is.na(R0_oct19), is.na(R0_oct19), 0))

values(R0map_oct19) <- perm_oct19


plot(R0map_oct19)

writeRaster(R0map_oct19 , "oct19_R0perm.nc", format = "CDF", overwrite = TRUE)

#high risk

perm_oct19 <- ifelse(R0_oct19>=0.75, 1, 
                     ifelse(is.na(R0_oct19), is.na(R0_oct19), 0))

values(R0map_oct19) <- perm_oct19


plot(R0map_oct19)

writeRaster(R0map_oct19 , "oct19_R0high.nc", format = "CDF", overwrite = TRUE)

#med risk

perm_oct19 <- ifelse(R0_oct19>=0.5, 1, 
                     ifelse(is.na(R0_oct19), is.na(R0_oct19), 0))

values(R0map_oct19) <- perm_oct19


plot(R0map_oct19)

writeRaster(R0map_oct19 , "oct19_R0high.nc", format = "CDF", overwrite = TRUE)



#_______________________________________________________________________________________________________________________________________________________________________________

#Aggregate map - permissive R0, HIGH OR MED - one at a time or change code  


months_permissive <- sum(R0map_may19,
                         R0map_jun19,
                         R0map_jul19,
                         R0map_aug19,
                         R0map_sep19,
                         R0map_oct19, na.rm = F)

plot(months_permissive)

writeRaster(months_permissive , "months_high.nc", format = "CDF", overwrite = TRUE)





nc_close(nc)
