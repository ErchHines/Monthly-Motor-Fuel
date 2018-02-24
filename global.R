library(dplyr)
library(reshape2)

GrossVolGas <- read.csv("data/GrossVolGas.csv", header = TRUE)
MF33CO16 <- read.csv("data/MF33CO16.csv", header = TRUE)
MF33GA16 <- read.csv("data/MF33GA16.csv", header = TRUE)
MF33SF16 <- read.csv("data/MF33SF16.csv", header = TRUE)
MF33SF17 <- read.csv("data/MF33SF17.csv", header = TRUE)
MF121TP1 <- read.csv("data/MF121TP1.csv", header = TRUE)
PercentChange <- read.csv("data/PercentChange.csv", header = TRUE)
# TGrossVOlGas <- t(GrossVolGas)
MGrossVolGas <- melt(GrossVolGas, id = c("StateName"))


