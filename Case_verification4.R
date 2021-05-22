#Case ver 
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
setwd("C:\\Users\\Gabriella\\Documents\\Vet Epi\\MSc Project\\Map\\Case Verification")

#import case data for months may through december 
may_cases <- read.csv("may_cases.csv")
names(may_cases) [1] <- "x"

jul_cases <- read.csv("jul_cases.csv")
names(jul_cases) [1] <- "x"

aug_cases <- read.csv("aug_cases.csv")
names(aug_cases) [1] <- "x"

sep_cases <- read.csv("sep_cases.csv")
names(sep_cases) [1] <- "x"

oct_cases <- read.csv("oct_cases.csv")
names(oct_cases) [1] <- "x"

nov_cases <- read.csv("nov_cases.csv")
names(nov_cases) [1] <- "x"

dec_cases <- read.csv("dec_cases.csv")
names(dec_cases) [1] <- "x"


all_cases <- bind_rows(list(may_cases,
                                 jul_cases,
                                 aug_cases,
                                 sep_cases,
                                 oct_cases,
                                 nov_cases,
                                 dec_cases
                            
                                 
), .id = "source")
#jun cases =0




all_cases$source[all_cases$source =="1"] <- "May"
all_cases$source[all_cases$source =="2"] <- "July"
all_cases$source[all_cases$source =="3"] <- "August"
all_cases$source[all_cases$source =="4"] <- "September"
all_cases$source[all_cases$source =="5"] <- "October"
all_cases$source[all_cases$source =="6"] <- "November"
all_cases$source[all_cases$source =="7"] <- "December"

write.csv(all_cases, file = "cases.csv")

all_cases <- read.csv("cases.csv")



##-- Plot months permissive and cases_________________________________________________________________________________________________

monthsperm= raster ("C:\\Users\\Gabriella\\Documents\\Vet Epi\\MSc Project\\Map\\maps\\from_R\\months_perm.nc")

months_perm_df <- as.data.frame(months_perm[[1]], xy=T)

#cases same month 
cases_permissive <- all_cases

coordinates(all_cases)= ~ x + y
plot(monthsperm)
plot(all_cases, add=TRUE)

cases_permissive$months_perm <- raster::extract(monthsperm, all_cases, method = 'simple', df=TRUE)


write.csv(cases_permissive, file = "cases_permissive.csv")
permcases <- read.csv("cases_permissive.csv")



cases_perm <- data.frame(permcases$no_cases, permcases$months_perm.layer)
names(cases_perm) [1] <- "cases"
names(cases_perm) [2] <- "months_perm"

months_p <- cases_perm$months_perm
cases_p <- cases_perm$cases

cases_permissive_agg <- aggregate(cases_p, by=list(months_p), sum)
names(cases_permissive_agg) [1] <- "pmonths"
names(cases_permissive_agg) [2] <- "pcases"

cases_permissive_agg <- add_row(cases_permissive_agg, pmonths=0, pcases=0)
cases_permissive_agg <- add_row(cases_permissive_agg, pmonths=1, pcases=0)
cases_permissive_agg <- add_row(cases_permissive_agg, pmonths=2, pcases=0)


ggplot(data = cases_permissive_agg, aes(x = pmonths, y = pcases)) +
  geom_bar(colour="black", fill="lightseagreen", width=.8, stat="identity") + 
  scale_x_continuous(breaks=seq(0,6,)) +
  theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold")) +
  guides(fill=FALSE) +
  labs(title = "Number of permissive months in locations with reported cases",
       x = "Number of months with permissive R0",
       y = "Number of cases")


#-- SAME FOR MEDIUM RISK _________________________________________________________________________________________________________________

monthsmed= raster ("C:\\Users\\Gabriella\\Documents\\Vet Epi\\MSc Project\\Map\\maps\\from_R\\months_med.nc")

months_med_df <- as.data.frame(months_med[[1]], xy=T)

#cases same month 
cases_medium <- all_cases

coordinates(all_cases)= ~ x + y
plot(monthsmed)
plot(all_cases, add=TRUE)

cases_medium$months_med <- raster::extract(monthsmed, all_cases, method = 'simple', df=TRUE)


write.csv(cases_medium, file = "cases_medium.csv")
medcases <- read.csv("cases_medium.csv")



cases_med <- data.frame(medcases$no_cases, medcases$months_med.layer)
names(cases_med) [1] <- "cases"
names(cases_med) [2] <- "months_med"

months_m <- cases_med$months_med
cases_m <- cases_med$cases

cases_medium_agg <- aggregate(cases_m, by=list(months_m), sum)
names(cases_medium_agg) [1] <- "mmonths"
names(cases_medium_agg) [2] <- "mcases"

cases_medium_agg <- add_row(cases_medium_agg, mmonths=0, mcases=0)
cases_medium_agg <- add_row(cases_medium_agg, mmonths=1, mcases=0)
cases_medium_agg <- add_row(cases_medium_agg, mmonths=2, mcases=0)



ggplot(data = cases_medium_agg, aes(x = mmonths, y = mcases)) +
  geom_bar(colour="black", fill="lightseagreen", width=.8, stat="identity") + 
  scale_x_continuous(breaks=seq(0,6,)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) + 
  guides(fill=FALSE) +
  labs(title = "Number of High risk months in locations with reported cases",
       x = "Number of months with relative R0>=0.5",
       y = "Number of cases")


#Cumulative frequency 
cases_high_agg$cumfreqh <- cumsum(cases_high_agg)


ggplot(data = cases_high_agg, aes(x = hmonths, y = cumfreqh)) +
  geom_bar(colour="black", fill="lightseagreen", width=.8, stat="identity") + 
  scale_x_continuous(breaks=seq(0,6,)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  guides(fill=FALSE) +
  labs(title = "Number of High risk months in locations with reported cases",
       x = "Number of months with relative R0>=0.75",
       y = "Number of cases")


#-- SAME FOR High RISK _________________________________________________________________________________________________________________

monthshigh= raster ("C:\\Users\\Gabriella\\Documents\\Vet Epi\\MSc Project\\Map\\maps\\from_R\\months_high.nc")

months_high_df <- as.data.frame(months_high[[1]], xy=T)

#cases same month 
cases_high <- all_cases

coordinates(all_cases)= ~ x + y
plot(monthshigh)
plot(all_cases, add=TRUE)

cases_high$months_high <- raster::extract(monthshigh, all_cases, method = 'simple', df=TRUE)


write.csv(cases_high, file = "cases_high.csv")
highcases <- read.csv("cases_high.csv")



cases_high <- data.frame(highcases$no_cases, highcases$months_high.layer)
names(cases_high) [1] <- "cases"
names(cases_high) [2] <- "months_high"

months_h <- cases_high$months_high
cases_h <- cases_high$cases

cases_high_agg <- aggregate(cases_h, by=list(months_h), sum)
names(cases_high_agg) [1] <- "hmonths"
names(cases_high_agg) [2] <- "hcases"

cases_high_agg <- add_row(cases_high_agg, hmonths=6, hcases=0)



ggplot(data = cases_high_agg, aes(x = hmonths, y = hcases)) +
  geom_bar(colour="black", fill="lightseagreen", width=.8, stat="identity") + 
  scale_x_continuous(breaks=seq(0,6,)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  guides(fill=FALSE) +
  labs(title = "Number of High risk months in locations with reported cases",
       x = "Number of months with relative R0>=0.75",
       y = "Number of cases")




##-- Checking Num of Na cases_________________________________________________________________________________________________


na_cases_perm <- cases_perm[is.na(cases_perm$months_perm),]
sum(na_cases_perm$cases)
#154

sum(cases_permissive_agg$pcases)
#1442

#Total cases may-dec = 1596


#Check location of Na cases 
na_permcases <- permcases[is.na(permcases$months_perm.layer),]

monthsperm= raster ("C:\\Users\\Gabriella\\Documents\\Vet Epi\\MSc Project\\Map\\maps\\from_R\\months_perm.nc")
months_perm_df <- as.data.frame(months_perm[[1]], xy=T)
plot(monthsperm)
coordinates(na_permcases)= ~ x + y
plot(na_permcases, add=TRUE)


