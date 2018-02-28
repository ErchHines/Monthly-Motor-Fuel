library(dplyr)
library(reshape2)

#Change the month in order to update to latest month of Report

reportMonth <- "September"


GrossVolGas <- read.csv("data/GrossVolGas.csv", header = TRUE)
MF33CO16 <- read.csv("data/MF33CO16.csv", header = TRUE)
MF33GA16 <- read.csv("data/MF33GA16.csv", header = TRUE)
MF33SF16 <- read.csv("data/MF33SF16.csv", header = TRUE)
MF33SF17 <- read.csv("data/MF33SF17.csv", header = TRUE)
#Becaue they annoyingly are using 0's instead of NA's on just this one table
MF33SF17[MF33SF17 == 0] <- NA
MF33SF17 <- MF33SF17[,colSums(is.na(MF33SF17))<nrow(MF33SF17)]
MF121TP1 <- read.csv("data/MF121TP1.csv", header = TRUE)
# MF121TP1[,c(3,5,7,9)] <- as.character(MF121TP1[,c(3,5,7,9)])
# MF121TP1[,c(3,5,7,9)] <- chron(dates = MF121TP1[,c(3,5,7,9)])
PercentChange <- read.csv("data/PercentChange.csv", header = TRUE)
#Most Recent data should be in last column, if not will need to change this
PercentChange <- PercentChange[,colSums(is.na(PercentChange))<nrow(PercentChange)]
# TGrossVOlGas <- t(GrossVolGas)
MGrossVolGas <- melt(GrossVolGas, id = c("StateName"))
meltMF33SF17 <- melt(MF33SF17, id = c("State"))


CAGA1216 <-read.csv("data/CAGA1216.csv", header = TRUE)
CAGA1216$Date <- as.Date(CAGA1216$Date, format="%m/%d/%Y")
CAGA1216$Gasoline <- as.integer(CAGA1216$Gasoline)
CAGA1216[order(CAGA1216$Date),]
