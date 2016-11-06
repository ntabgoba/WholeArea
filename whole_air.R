#WholeArea
#1 Calculating the Cumulative Air Dose Rate
#2 Establish the region
#3 Calculate the CED
#4 Make percentages of Region, Population and CED
library(dplyr)
library(ggplot2)
library(leaflet)
# Data source: http://emdb.jaea.go.jp/emdb/en/portals/b131/
# Source:NRA for 2011 and 2012 Datasets
air_2011 <- read.csv(file = "FukushimaJune2011.csv", header = TRUE) # 45,273 entries
dim(air_2011)
View(air_2011)
names(air_2011) <- c("gridcode","pref","city","gridCenterNorthlat","gridCenterEastlng",
                     "gridCenterNorthlatDec","gridCenterEastlngDec","daichi_distance",
                     "no_samples","AvgAirDoseRate","NE_nLat","NE_eLong","NW_nLat","NW_eLong",
                     "SW_nLat","SW_eLong","SE_nLat","SE_eLong")
#remove background radiations, jp govt sets at 0.04µSv/h
air_2011<- subset(air_2011, AvgAirDoseRate > 0.04) # 45,268 entries
#Calculate annual external dose rate
air_2011$AnnualExtDose <- (air_2011$AvgAirDoseRate - 0.04)*(16 + 8*0.4)*365/1000
air_2011$pref <- as.character(air_2011$pref)
air_2011$city <- as.character(air_2011$city)
air_2011$gridcode <- as.character(air_2011$gridcode)
#make cuts of Annual External Air Dose
air_2011$AnnualExDoseRange <- cut(air_2011$AnnualExtDose, c(0,1,5,10,20,50,100,200,280))
#calculate area
air_2011AnnualExDoseRange_summary <- data.frame(table(air_2011$AnnualExDoseRange))
air_2011AnnualExDoseRange_summary$Areakm2 <- 0.01 * air_2011AnnualExDoseRange_summary$Freq
sum(air_2011AnnualExDoseRange_summary$Areakm2)

####
iro <- colorFactor(
        palette = "Blues",
        domain = air_2011$pop_quants
)

iro2 <- colorFactor(
        palette = "PuRd",
        domain = air_2011$AnnualExDoseRange
)
air_2011_plot <- leaflet() %>%
        addTiles()%>%
        addRectangles(data = air_2011,lng1 = ~SW_eLong, lat1 = ~SW_nLat,
                      lng2 = ~NE_eLong, lat2 = ~NE_nLat,
                      color = ~iro2(air_2011$AnnualExDoseRange))
air_2011_plot



# FukushimaMarch2012
air_2012 <- read.csv(file = "FukushimaMarch2012.csv", header = TRUE) #38,741 entries
names(air_2012) <- c("gridcode","pref","city","gridCenterNorthlat","gridCenterEastlng",
                     "gridCenterNorthlatDec","gridCenterEastlngDec","daichi_distance",
                     "no_samples","AvgAirDoseRate","NE_nLat","NE_eLong","NW_nLat","NW_eLong",
                     "SW_nLat","SW_eLong","SE_nLat","SE_eLong")
#remove background radiations, jp govt sets at 0.04µSv/h
air_2012<- subset(air_2012, AvgAirDoseRate > 0.04) # 45,268 entries
#Calculate annual external dose rate
air_2012$AnnualExtDose <- (air_2012$AvgAirDoseRate - 0.04)*(16 + 8*0.4)*365/1000
air_2012$pref <- as.character(air_2012$pref)
air_2012$city <- as.character(air_2012$city)
air_2012$gridcode <- as.character(air_2012$gridcode)
#make cuts of Annual External Air Dose
air_2012$AnnualExDoseRange <- cut(air_2012$AnnualExtDose, c(0,1,5,10,20,50,100,200,280))
#calculate area
air_2012AnnualExDoseRange_summary <- data.frame(table(air_2012$AnnualExDoseRange))
air_2012AnnualExDoseRange_summary$Areakm2 <- 0.01 * air_2012AnnualExDoseRange_summary$Freq
sum(air_2012AnnualExDoseRange_summary$Areakm2)

# Readings of Detailed Monitoring in the Restricted Area and Planned Evacuation Zone 
# (13th Vehicle-borne Survey) ( From March 2013 to April 2013 )
air_2013 <- read.csv(file = "10214700024_00_201303/10214700024_00_20130224.csv", header = TRUE)
air_2013b <- read.csv(file = "10214700024_00_201303/10214700024_00_20130303.csv", header = TRUE)
air_2013c <- read.csv(file = "10214700024_00_201303/10214700024_00_20130310.csv", header = TRUE)
dim(air_2013c)
View(air_2013)
#  Readings of Detailed Monitoring in the Areas to Which Evacuation Orders Have Been Issued 
# (17th Vehicle-borne Survey) ( From March 2014 to April 2014 )
air_2014 <- read.csv(file = "10202600017_07.csv", header = TRUE)
dim(air_2014)

#MinSci
# March 2015, Air Dose Rates Measured by Route Buses in Fukushima Prefecture
# http://emdb.jaea.go.jp/emdb/en/portals/b147/
air_2015 <- read.csv(file = "./MinSci/10214700026_00_201503/10214700026_00_20150329.csv", header = TRUE)
dim(air_2015)
View(air_2015)

