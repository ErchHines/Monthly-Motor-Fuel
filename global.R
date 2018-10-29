library(RColorBrewer)
library(scales)
library(geojsonio)
library(sp)
library(ggplot2)
library(reshape2)
library(ggseas)
library(forecast)
library(ggthemes)
library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)
library(leaflet)
library(DT)

#### Note: This application is a prototype and reads in data from .csv data files, 
# the final application will pull data directly from the server using RODBC

# loads a map with state boundaries
states <- geojson_read("data/us-states.geojson", what = "sp")

#Change the month in order to update to latest month of Report
reportMonth <- "September"
reportYear <- 2017

#Read in the csv tables
GrossVolGas <- read.csv("data/GrossVolGas.csv", header = TRUE)
MF33CO16 <- read.csv("data/MF33CO16.csv", header = TRUE)
MF33GA16 <- read.csv("data/MF33GA16.csv", header = TRUE)
MF33SF16 <- read.csv("data/MF33SF16.csv", header = TRUE)
MF33SF17 <- read.csv("data/MF33SF17.csv", header = TRUE)
MF121TP1 <- read.csv("data/MF121TP1.csv", header = TRUE)
PercentChange <- read.csv("data/PercentChange.csv", header = TRUE)

#Becaue they annoyingly are using 0's instead of NA's on just this one table
MF33SF17[MF33SF17 == 0] <- NA
MF33SF17 <- MF33SF17[,colSums(is.na(MF33SF17))<nrow(MF33SF17)]

#Most Recent data should be in last column, if not will need to change this
PercentChange <- PercentChange[,colSums(is.na(PercentChange))<nrow(PercentChange)]
MGrossVolGas <- melt(GrossVolGas, id = c("StateName"))
meltMF33SF17 <- melt(MF33SF17, id = c("State"))

#This brings in the raw 551 data 
raw551 <- read.csv("data/MF551_RAW.csv", header = TRUE)
raw551 <- subset(raw551, DESCRIPTION == "Gross Volume")
raw551$Date <- as.Date(raw551$Date, "%m/%d/%Y")

#combine gasoline and gasohol numbers
raw551$gas <- raw551$GASOHOL_VOLUME + raw551$GASOLINE_VOLUME
raw551$dieselLPG <- raw551$DIESEL_VOLUME + raw551$LPG_VOLUME

#Just use the dates for current report, remove early years with odd data
raw551 <- subset(raw551, Date >= "2006-01-01" & Date <= "2017-09-01")

#The following creates a table with the number of gallons per day
raw551Days <- raw551

#Use the days_in_month function from lubridate and apply over the date column
raw551Days$Days <- sapply(raw551Days$Date, days_in_month)

#Divide our gallons by the number of days in the month
raw551Days$gas <- raw551Days$gas / raw551Days$Days
raw551Days$dieselLPG <- raw551Days$dieselLPG / raw551Days$Days

# initialize global variable to record selected (clicked) rows
selected_points <- raw551[0, ]
str(selected_points)

