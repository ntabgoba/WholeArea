#working with gdam
library(sp)
library(ggplot2)
library(dplyr)

jp1 <- readRDS("gdam/JPN_adm1.rds")
class(jp1)
dim(jp1)
jp2 <- readRDS("gdam/JPN_adm2.rds")
dim(jp2)
plot(jp2)
#play
plot(jp1, col = 'forestgreen', border = 'lightgrey', xlim = c(-1, 1), ylim = c(50.5, 52))

regionalValues <- runif(192)  # Simulate a value for each region between 0 and 1
plot(gadm, col = gray(regionalValues), border = 0)

#---------------------------------------------
library(raster)
# get the data 
ind2 <- getData('GADM', country='IND', level=2)
wb2 <- ind2[ind2$NAME_1=="West Bengal",]
cities <- data.frame(name="Purulia", lon=86.36521, lat=23.33208)

# plot
plot(wb2, border='gray', col='light gray')
points(cities[, 2:3], col='red', pch=20)
#---------------------------------------------


