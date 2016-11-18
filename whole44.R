#Whole area on a 4x4 km2
air_2011 <- read.csv("fukprefMay-June2015.csv") #2871x7 dim, 58 NAME_2 level

names(air_2011) <- c("mdate","pref","city","NorthlatDec","EastlngDec",
                      "daichi_distance","AvgAirDoseRate")
air_2011$mdate <- as.Date(air_2011$mdate)
air_2011$pref <- as.character(air_2011$pref)
air_2011$city <- as.character(air_2011$city)

#remove background radiations, jp govt sets at 0.04µSv/h
air_2011<- subset(air_2011, AvgAirDoseRate > 0.04) #2870    entries
#Calculate annual external dose rate
air_2011$AnnualExtDose <- (air_2011$AvgAirDoseRate - 0.04)*(8 + 16*0.4)*365/1000

#make cuts of Annual External Air Dose
air_2011$AnnualExDoseRange <- cut(air_2011$AnnualExtDose, c(0,1,3,5,10,20,50,100,200))
#calculate area
air_2011AnnualExDoseRange_summary <- data.frame(table(air_2011$AnnualExDoseRange))
air_2011AnnualExDoseRange_summary$Areakm2 <- 0.01 * air_2011AnnualExDoseRange_summary$Freq
sum(air_2011AnnualExDoseRange_summary$Areakm2)  # 28.7km²

iro2 <- colorFactor(
        palette = "PuRd",
        domain = air_2011$AnnualExDoseRange
)
air_2011_plot <- leaflet() %>%
        addTiles()%>%
        addCircles(data = air_2011,lng = ~EastlngDec, lat = ~NorthlatDec,
                   color = ~iro2(air_2011$AnnualExDoseRange)) %>%
        addLegend("bottomright", pal = iro2, values = air_2011$AnnualExDoseRange,
                  title = "AnnualExDoseRange",
                  labFormat = labelFormat(prefix = "mSv/y "),
                  opacity = 1)%>%
        addMarkers(lat = 37.4211, lng = 141.0328,icon = nukeicon)
air_2011_plot
