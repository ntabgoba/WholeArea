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
names(air_2011) <- c("gridcode","pref","city","gridCenterNorthlat","gridCenterEastlng",
                     "gridCenterNorthlatDec","gridCenterEastlngDec","daichi_distance",
                     "no_samples","AvgAirDoseRate","NE_nLat","NE_eLong","NW_nLat","NW_eLong",
                     "SW_nLat","SW_eLong","SE_nLat","SE_eLong")
#remove background radiations, jp govt sets at 0.04µSv/h
air_2011<- subset(air_2011, AvgAirDoseRate > 0.04) # 45,268 entries
#Calculate annual external dose rate
air_2011$AnnualExtDose <- (air_2011$AvgAirDoseRate - 0.04)*(8 + 16*0.4)*365/1000
air_2011$pref <- as.character(air_2011$pref)
air_2011$city <- as.character(air_2011$city)
air_2011$gridcode <- as.character(air_2011$gridcode)
#make cuts of Annual External Air Dose
air_2011$AnnualExDoseRange <- cut(air_2011$AnnualExtDose, c(0,1,3,5,10,20,50,100,200))
#calculate area
air_2011AnnualExDoseRange_summary <- data.frame(table(air_2011$AnnualExDoseRange))
air_2011AnnualExDoseRange_summary$Areakm2 <- 0.01 * air_2011AnnualExDoseRange_summary$Freq
sum(air_2011AnnualExDoseRange_summary$Areakm2) #452.68

####
nukeicon <- makeIcon(iconUrl = "nukeicon.png",iconWidth = 18, iconHeight=18)
iro2 <- colorFactor(
        palette = "PuRd",
        domain = air_2011$AnnualExDoseRange
)
air_2011_plot <- leaflet() %>%
        addTiles()%>%
        addRectangles(data = air_2011,lng1 = ~SW_eLong, lat1 = ~SW_nLat,
                      lng2 = ~NE_eLong, lat2 = ~NE_nLat,
                      color = ~iro2(air_2011$AnnualExDoseRange)) %>%
        addLegend("bottomright", pal = iro2, values = air_2011$AnnualExDoseRange,
                  title = "AnnualExDoseRange",
                  labFormat = labelFormat(prefix = "mSv/y "),
                  opacity = 1)%>%
        addMarkers(lat = 37.4211, lng = 141.0328,icon = nukeicon)
air_2011_plot



# FukushimaMarch2012
air_2012 <- read.csv(file = "FukushimaMarch2012.csv", header = TRUE) #38,741 entries
names(air_2012) <- c("gridcode","pref","city","gridCenterNorthlat","gridCenterEastlng",
                     "gridCenterNorthlatDec","gridCenterEastlngDec","daichi_distance",
                     "no_samples","AvgAirDoseRate","NE_nLat","NE_eLong","NW_nLat","NW_eLong",
                     "SW_nLat","SW_eLong","SE_nLat","SE_eLong")
air_2012$pref <- as.character(air_2012$pref)
air_2012$city <- as.character(air_2012$city)
air_2012$gridcode <- as.character(air_2012$gridcode)
#remove background radiations, jp govt sets at 0.04µSv/h
air_2012<- subset(air_2012, AvgAirDoseRate > 0.04) #38,740  entries
#Calculate annual external dose rate
air_2012$AnnualExtDose <- (air_2012$AvgAirDoseRate - 0.04)*(8 + 16*0.4)*365/1000

#make cuts of Annual External Air Dose
air_2012$AnnualExDoseRange <- cut(air_2012$AnnualExtDose, c(0,1,3,5,10,20,50,100,200))
#calculate area
air_2012AnnualExDoseRange_summary <- data.frame(table(air_2012$AnnualExDoseRange))
air_2012AnnualExDoseRange_summary$Areakm2 <- 0.01 * air_2012AnnualExDoseRange_summary$Freq
sum(air_2012AnnualExDoseRange_summary$Areakm2)  #387.4km²
View(air_2012AnnualExDoseRange_summary)
iro2 <- colorFactor(
        palette = "PuRd",
        domain = air_2012$AnnualExDoseRange
)
air_2012_plot <- leaflet() %>%
        addTiles()%>%
        addRectangles(data = air_2012,lng1 = ~SW_eLong, lat1 = ~SW_nLat,
                      lng2 = ~NE_eLong, lat2 = ~NE_nLat,
                      color = ~iro2(air_2012$AnnualExDoseRange)) %>%
        addLegend("bottomright", pal = iro2, values = air_2012$AnnualExDoseRange,
                  title = "AnnualExDoseRange",
                  labFormat = labelFormat(prefix = "mSv/y "),
                  opacity = 1)%>%
        addMarkers(lat = 37.4211, lng = 141.0328,icon = nukeicon)
air_2012_plot

# 2013 FUKUSHIMA
air_2013a <- read.csv(file = "10214700024_00_201303/10214700024_00_20130224.csv", header = TRUE)
air_2013b <- read.csv(file = "10214700024_00_201303/10214700024_00_20130303.csv", header = TRUE)
air_2013c <- read.csv(file = "10214700024_00_201303/10214700024_00_20130310.csv", header = TRUE)
air_2013d <- read.csv(file = "10214700024_00_201303/10214700024_00_20130317.csv", header = TRUE)
air_2013e <- read.csv(file = "10214700024_00_201303/10214700024_00_20130324.csv", header = TRUE)
air_2013f <- read.csv(file = "10214700024_00_201303/10214700024_00_20130331.csv", header = TRUE)

rbind(dim(air_2013a),dim(air_2013b),dim(air_2013c),dim(air_2013d),dim(air_2013e),dim(air_2013f))
#concanete all the dataframes of 2013
air_2013 <- Reduce(rbind, list(air_2013a,air_2013b,air_2013c,air_2013d,air_2013e,air_2013f)) #entries 24,328  
names(air_2013) <- c("mdate","gridcode","gridCenterNorthlat","gridCenterEastlng","gridScornerNorthlatDec",
                     "gridWcornerEastlngDec","gridNcornerNorthlatDec","gridEcornerEastlngDec",
                     "daichi_distance","no_samples1cm","AvgAirDoseRate")
# subset by removing duplicated gridcodes (joints where buses cross)
air_2013 <- subset(air_2013, !duplicated(gridcode)) # 6,921 entries

air_2013<- subset(air_2013, AvgAirDoseRate > 0.04) #6,913 entries
#Calculate annual external dose rate
air_2013$AnnualExtDose <- (air_2013$AvgAirDoseRate - 0.04)*(8 + 16*0.4)*365/1000
# Min.    1st Qu. Median    Mean     3rd Qu.  Max. 
# 0.07008 0.56060 1.19100   1.25000  1.68200  6.72800 
#make cuts of Annual External Air Dose
air_2013$AnnualExDoseRange <- cut(air_2013$AnnualExtDose, c(0,1,3,5,10,20,50,100,200))
#calculate area
air_2013AnnualExDoseRange_summary <- data.frame(table(air_2013$AnnualExDoseRange))
air_2013AnnualExDoseRange_summary$Areakm2 <- 0.01 * air_2013AnnualExDoseRange_summary$Freq
sum(air_2013AnnualExDoseRange_summary$Areakm2)  #69.13km²

iro2 <- colorFactor(
        palette = "PuRd",
        domain = air_2013$AnnualExDoseRange
)
air_2013_plot <- leaflet() %>%
        addTiles()%>%
        addRectangles(data = air_2013,lng1 = ~gridWcornerEastlngDec,lat1 = ~gridScornerNorthlatDec, 
                      lng2 = ~gridEcornerEastlngDec, lat2 = ~gridNcornerNorthlatDec,
                      color = ~iro2(air_2013$AnnualExDoseRange)) %>%
        addLegend("bottomright", pal = iro2, values = air_2013$AnnualExDoseRange,
                  title = "AnnualExDoseRange",
                  labFormat = labelFormat(prefix = "mSv/y "),
                  opacity = 1)%>%
        addMarkers(lat = 37.4211, lng = 141.0328,icon = nukeicon) 
air_2013_plot

#  Readings of Detailed Monitoring in the Areas to Which Evacuation Orders Have Been Issued 
# (17th Vehicle-borne Survey) ( From March 2014 to April 2014 )
air_2014a <- read.csv(file = "10214700025_00_201403/10214700025_00_20140223.csv", header = TRUE)
air_2014b <- read.csv(file = "10214700025_00_201403/10214700025_00_20140302.csv", header = TRUE)
air_2014c <- read.csv(file = "10214700025_00_201403/10214700025_00_20140309.csv", header = TRUE)
air_2014d <- read.csv(file = "10214700025_00_201403/10214700025_00_20140316.csv", header = TRUE)
air_2014e <- read.csv(file = "10214700025_00_201403/10214700025_00_20140323.csv", header = TRUE)
air_2014f <- read.csv(file = "10214700025_00_201403/10214700025_00_20140330.csv", header = TRUE)

rbind(dim(air_2014a),dim(air_2014b),dim(air_2014c),dim(air_2014d),dim(air_2014e),dim(air_2014f))
#concanete all the dataframes of 2014
air_2014 <- Reduce(rbind, list(air_2014a,air_2014b,air_2014c,air_2014d,air_2014e,air_2014f)) #entries 20,049  
names(air_2014) <- c("mdate","gridcode","gridCenterNorthlat","gridCenterEastlng","gridScornerNorthlatDec",
                     "gridWcornerEastlngDec","gridNcornerNorthlatDec","gridEcornerEastlngDec",
                     "daichi_distance","no_samples1cm","AvgAirDoseRate")
# subset by removing duplicated gridcodes (joints where buses cross)
air_2014 <- subset(air_2014, !duplicated(gridcode)) # 7,253 entries

air_2014<- subset(air_2014, AvgAirDoseRate > 0.04) #7,148 entries
#Calculate annual external dose rate
air_2014$AnnualExtDose <- (air_2014$AvgAirDoseRate - 0.04)*(8 + 16*0.4)*365/1000
 
#make cuts of Annual External Air Dose
air_2014$AnnualExDoseRange <- cut(air_2014$AnnualExtDose, c(0,1,3,5,10,20,50,100,200))
#calculate area
air_2014AnnualExDoseRange_summary <- data.frame(table(air_2014$AnnualExDoseRange))
air_2014AnnualExDoseRange_summary$Areakm2 <- 0.01 * air_2014AnnualExDoseRange_summary$Freq
sum(air_2014AnnualExDoseRange_summary$Areakm2)  # 71.48km²

iro2 <- colorFactor(
        palette = "PuRd",
        domain = air_2014$AnnualExDoseRange
)
air_2014_plot <- leaflet() %>%
        addTiles()%>%
        addRectangles(data = air_2014,lng1 = ~gridWcornerEastlngDec,lat1 = ~gridScornerNorthlatDec, 
                      lng2 = ~gridEcornerEastlngDec, lat2 = ~gridNcornerNorthlatDec,
                      color = ~iro2(air_2014$AnnualExDoseRange)) %>%
        addLegend("bottomright", pal = iro2, values = air_2014$AnnualExDoseRange,
                  title = "AnnualExDoseRange",
                  labFormat = labelFormat(prefix = "mSv/y "),
                  opacity = 1)%>%
        addMarkers(lat = 37.4211, lng = 141.0328,icon = nukeicon) 
air_2014_plot
#MinSci
# March 2015, Air Dose Rates Measured by Route Buses in Fukushima Prefecture
# http://emdb.jaea.go.jp/emdb/en/portals/b147/
air_2015 <- read.csv(file = "./MinSci/10214700026_00_201503/10214700026_00_20150329.csv", header = TRUE)
dim(air_2015)
View(air_2015)

