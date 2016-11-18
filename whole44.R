#Whole area on a 4x4 km2
library(leaflet)
library(dplyr)
nukeicon <- makeIcon(iconUrl = "nukeicon.png",iconWidth = 18, iconHeight=18)

air_11 <- read.csv("44/") #21350     8 dim, 105 NAME_2 level

names(air_11_15) <- c("mdate","pref","city","NorthlatDec","EastlngDec",
                      "daichi_distance","AvgAirDoseRate","AvgAirDoseRateCm")
air_11_15$mdate <- as.Date(air_11_15$mdate)
air_11_15$pref <- as.character(air_11_15$pref)
air_11_15$city <- as.character(air_11_15$city)
air_11_15$gride <- latlong_to_meshcode(lat = air_11_15$NorthlatDec, long = air_11_15$EastlngDec,order = 3)
#remove background radiations, jp govt sets at 0.04µSv/h
air_11_15<- subset(air_11_15, AvgAirDoseRate > 0.04) #  21327     8
#Calculate annual external dose rate
air_11_15$AnnualExtDose <- (air_11_15$AvgAirDoseRate - 0.04)*(8 + 16*0.4)*365/1000

#make cuts of Annual External Air Dose
air_11_15$AnnualExDoseRange <- cut(air_11_15$AnnualExtDose, c(0,1,3,5,10,20,50,100,200))
#calculate area
air_11_15AnnualExDoseRange_summary <- data.frame(table(air_11_15$AnnualExDoseRange))
air_11_15AnnualExDoseRange_summary$Areakm2 <- air_11_15AnnualExDoseRange_summary$Freq
sum(air_11_15AnnualExDoseRange_summary$Areakm2)  # 341,200km²

iro2 <- colorFactor(
        palette = "PuRd",
        domain = air_11_15$AnnualExDoseRange
)
air_11_15_plot <- leaflet() %>%
        addTiles()%>%
        addCircles(data = air_11_15,lng = ~EastlngDec, lat = ~NorthlatDec,
                   color = ~iro2(air_11_15$AnnualExDoseRange)) %>%
        addLegend("bottomright", pal = iro2, values = air_11_15$AnnualExDoseRange,
                  title = "AnnualExDoseRange",
                  labFormat = labelFormat(prefix = "mSv/y "),
                  opacity = 1)%>%
        addMarkers(lat = 37.4211, lng = 141.0328,icon = nukeicon)
air_11_15_plot

unique_gride <- unique()

air_11_15_summary <- air_11_15 %>% summarise(group_by(unique(gride)) %>% transmute(MinADR = min(AnnualExtDose), MaxADR = max(AnnualExtDose),MeanADR = mean(AnnualExtDose), SdADR =sd(AnnualExtDose)))

