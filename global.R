library(dplyr)
library(reshape2)

allzips <- readRDS("data/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college * 100
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

GrossVolGas <- read.csv("data/GrossVolGas.csv", header = TRUE)
MF33CO16 <- read.csv("data/MF33CO16.csv", header = TRUE)
MF33GA16 <- read.csv("data/MF33GA16.csv", header = TRUE)
MF33SF16 <- read.csv("data/MF33SF16.csv", header = TRUE)
MF33SF17 <- read.csv("data/MF33SF17.csv", header = TRUE)
MF121TP1 <- read.csv("data/MF121TP1.csv", header = TRUE)
PercentChange <- read.csv("data/PercentChange.csv", header = TRUE)
TGrossVOlGas <- t(GrossVolGas)
MGrossVolGas <- melt(GrossVolGas, id = c("StateName"))


cleantable <- allzips %>%
  select(
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
    Rank = rank,
    Score = centile,
    Superzip = superzip,
    Population = adultpop,
    College = college,
    Income = income,
    Lat = latitude,
    Long = longitude
  )
