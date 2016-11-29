#------------------------------------------------------------------------------------------------------------------------
# 4x4 km mesh Data Set 
#------------------------------------------------------------------------------------------------------------------------
library(leaflet)
library(dplyr)
library(jpmesh)
library(ggplot2)
library(reshape2)

nukeicon <- makeIcon(iconUrl = "nukeicon.png",iconWidth = 18, iconHeight=18)

air_11 <- read.csv("44/Aug2011.csv") #2776    7, 105 NAME_2 level
View(air_11)
names(air_11) <- c("mdate","pref","city","NorthlatDec","EastlngDec",
                      "daichi_distance","AvgAirDoseRate")
air_11$mdate <- as.Date(air_11$mdate)
air_11$pref <- as.character(air_11$pref)
air_11$city <- as.character(air_11$city)
air_11$gride <- latlong_to_meshcode(lat = air_11$NorthlatDec, long = air_11$EastlngDec,order = 3)
#remove background radiations, jp govt sets at 0.04µSv/h
air_11<- subset(air_11, AvgAirDoseRate > 0.04) # 2776    8
#Calculate annual external dose rate
air_11$AnnualExtDose <- (air_11$AvgAirDoseRate - 0.04)*(8 + 16*0.4)*365/1000

#make cuts of Annual External Air Dose
air_11$AnnualExDoseRange <- cut(air_11$AnnualExtDose, c(0,1,3,5,10,15,20,25,30))

#remove duplicate grides
air_11 <- air_11[!duplicated(air_11$gride),] # 2392   10

#calculate area
air_11AnnualExDoseRange_summary <- data.frame(table(air_11$AnnualExDoseRange))
air_11AnnualExDoseRange_summary$Areakm2 <- air_11AnnualExDoseRange_summary$Freq
sum(air_11AnnualExDoseRange_summary$Areakm2)  # 

iro2 <- colorFactor(
        palette = "PuRd",
        domain = air_11$AnnualExDoseRange
)
air_11_plot <- leaflet() %>%
        addTiles()%>%
        addCircles(data = air_11,lng = ~EastlngDec, lat = ~NorthlatDec,
                   color = ~iro2(air_11$AnnualExDoseRange)) %>%
        addLegend("bottomright", pal = iro2, values = air_11$AnnualExDoseRange,
                  title = "AnnualExDoseRange",
                  labFormat = labelFormat(prefix = "mSv/y "),
                  opacity = 1)%>%
        addMarkers(lat = 37.4211, lng = 141.0328,icon = nukeicon)
air_11_plot

unique_gride <- unique()

air_11_15_summary <- air_11_15 %>% summarise(group_by(unique(gride)) %>% transmute(MinADR = min(AnnualExtDose), MaxADR = max(AnnualExtDose),MeanADR = mean(AnnualExtDose), SdADR =sd(AnnualExtDose)))

### 2012
air_12 <- read.csv("44/March2012.csv") #2674    7, 58 NAME_2 level

names(air_12) <- c("mdate","pref","city","NorthlatDec","EastlngDec",
                   "daichi_distance","AvgAirDoseRate")
air_12$mdate <- as.Date(air_12$mdate)
air_12$pref <- as.character(air_12$pref)
air_12$city <- as.character(air_12$city)
air_12$gride <- latlong_to_meshcode(lat = air_12$NorthlatDec, long = air_12$EastlngDec,order = 3)
#remove background radiations, jp govt sets at 0.04µSv/h
air_12<- subset(air_12, AvgAirDoseRate > 0.04) # 2776    8
#Calculate annual external dose rate
air_12$AnnualExtDose <- (air_12$AvgAirDoseRate - 0.04)*(8 + 16*0.4)*365/1000

#make cuts of Annual External Air Dose
air_12$AnnualExDoseRange <- cut(air_12$AnnualExtDose, c(0,1,3,5,10,15,20,25,30))

#remove duplicate grides
air_12 <- air_12[!duplicated(air_12$gride),] # 2295   10

## 2013
air_13 <- read.csv("44/May2013.csv") #2747    7, 58 NAME_2 level

names(air_13) <- c("mdate","pref","city","NorthlatDec","EastlngDec",
                   "daichi_distance","AvgAirDoseRate")
air_13$mdate <- as.Date(air_13$mdate)
air_13$pref <- as.character(air_13$pref)
air_13$city <- as.character(air_13$city)
air_13$gride <- latlong_to_meshcode(lat = air_13$NorthlatDec, long = air_13$EastlngDec,order = 3)
#remove background radiations, jp govt sets at 0.04µSv/h
air_13<- subset(air_13, AvgAirDoseRate > 0.04) # 2747    8
#Calculate annual external dose rate
air_13$AnnualExtDose <- (air_13$AvgAirDoseRate - 0.04)*(8 + 16*0.4)*365/1000

#make cuts of Annual External Air Dose
air_13$AnnualExDoseRange <- cut(air_13$AnnualExtDose, c(0,1,3,5,10,15,20,25,30))

#remove duplicate grides
air_13 <- air_13[!duplicated(air_13$gride),] # 2395   10


# 2014
air_14 <- read.csv("44/May2014.csv") #2904    7, 58 NAME_2 level

names(air_14) <- c("mdate","pref","city","NorthlatDec","EastlngDec",
                   "daichi_distance","AvgAirDoseRate")
air_14$mdate <- as.Date(air_14$mdate)
air_14$pref <- as.character(air_14$pref)
air_14$city <- as.character(air_14$city)
air_14$gride <- latlong_to_meshcode(lat = air_14$NorthlatDec, long = air_14$EastlngDec,order = 3)
#remove background radiations, jp govt sets at 0.04µSv/h
air_14 <- subset(air_14, AvgAirDoseRate > 0.04) # 2904    8
#Calculate annual external dose rate
air_14$AnnualExtDose <- (air_14$AvgAirDoseRate - 0.04)*(8 + 16*0.4)*365/1000

#make cuts of Annual External Air Dose
air_14$AnnualExDoseRange <- cut(air_14$AnnualExtDose, c(0,1,3,5,10,15,20,25,30))

#remove duplicate grides
air_14 <- air_14[!duplicated(air_14$gride),] # 2548   10


## 2015
air_15 <- read.csv("44/May2015.csv") #2871    7, 58 NAME_2 level

names(air_15) <- c("mdate","pref","city","NorthlatDec","EastlngDec",
                   "daichi_distance","AvgAirDoseRate")
air_15$mdate <- as.Date(air_15$mdate)
air_15$pref <- as.character(air_15$pref)
air_15$city <- as.character(air_15$city)
air_15$gride <- latlong_to_meshcode(lat = air_15$NorthlatDec, long = air_15$EastlngDec,order = 3)
#remove background radiations, jp govt sets at 0.04µSv/h
air_15 <- subset(air_15, AvgAirDoseRate > 0.04) # 2870    8
#Calculate annual external dose rate
air_15$AnnualExtDose <- (air_15$AvgAirDoseRate - 0.04)*(8 + 16*0.4)*365/1000

#make cuts of Annual External Air Dose
air_15$AnnualExDoseRange <- cut(air_15$AnnualExtDose, c(0,1,3,5,10,15,20,25,30))

#remove duplicate grides
air_15 <- air_15[!duplicated(air_15$gride),] # 2544   10

#FUKUSHIMA POPULATION
fuk_pop <- read.csv("44/fuk.csv")
length(unique(fuk_pop$gridcode))
View(fuk_pop)




#############
air_11_plot <- leaflet() %>%
        addProviderTiles("Stamen.Toner")%>%
        addCircles(data = air_11,lng = ~EastlngDec, lat = ~NorthlatDec,
                   color = ~iro2(air_11$AnnualExDoseRange),radius=2000,fillOpacity = 1) %>%
        addLegend("bottomright", pal = iro2, values = air_11$AnnualExDoseRange,
                  title = "AnnualExDoseRange",
                  labFormat = labelFormat(prefix = "mSv/y "),
                  opacity = 1)%>%
        addMarkers(lat = 37.4211, lng = 141.0328,icon = nukeicon)%>%
        addPolygons(data=fu_adm,color="black", fillOpacity=0, weight = 1, fill = FALSE)
air_11_plot

# ORDERED AnnualExDoseRange
unique(air_11$AnnualExDoseRange)
air_11_ordered <- air_11[order(air_11$AnnualExDoseRange),]
View(air_11_ordered)
nukeicon <- makeIcon(iconUrl = "nk.png",iconWidth = 18, iconHeight=18)
iro2 <- colorFactor(
        palette = "Reds",
        domain = air_11$AnnualExDoseRange
)
air_11_ordered_plot <- leaflet() %>%
        addProviderTiles("Stamen.Toner")%>%
        addCircles(data = air_11_ordered,lng = ~EastlngDec, lat = ~NorthlatDec,
                   color = ~iro2(air_11_ordered$AnnualExDoseRange),radius=2000,fillOpacity = 1,stroke=FALSE) %>%
        addLegend("bottomright", pal = iro2, values = air_11_ordered$AnnualExDoseRange,
                  title = "AnnualExDoseRange",
                  labFormat = labelFormat(prefix = "mSv/y "),
                  opacity = 1)%>%
        addMarkers(lat = 37.4211, lng = 141.0328,icon = nukeicon)%>%
        addPolygons(data=fu_adm,color="black", fillOpacity=0, weight = 1, fill = FALSE)
air_11_ordered_plot


## BEGIN FORTIFYING TO USE GGPLOT

p <- ggplot(fu_f) +
        geom_point(data = air_11_ordered, aes(x = EastlngDec, y = NorthlatDec, color = AnnualExDoseRange,shape=15),size=3)+
        scale_shape_identity()+
        scale_color_brewer(palette="Reds")+
        geom_polygon(data=fu_f,aes(x = long, y = lat, group = group),color="#999999",fill=NA)+
        coord_map()+
        annotate("text", x = 141.0328, y = 37.4211, label = "x",color="red", size=4)+
        ggtitle("Year 2011") +
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank())
p + facet_grid(~ month)
# 
# + theme_bw()
# update_labels(p, list(x = "Longitude", y = "Latitude"))


#Pie Charts
pie <- ggplot(air_11_ordered, aes(x = "sq.km", fill = AnnualExDoseRange)) +
                      geom_bar(width = 1) 
pie <- pie + coord_polar(theta = "y") 

pie + scale_fill_brewer(palette="Reds")+
        theme_minimal()

#------------------------------------------------------------------------------------------------------------------------
# 5 YEARS DATA COMBINED
#------------------------------------------------------------------------------------------------------------------------
air_11_15 <- read.csv("44/44whole.csv") #21,350     8, 105 NAME_2 level
air_11_15$Air.dose.rate.at.a.height.of.1.cm..μSv.h. <- NULL
names(air_11_15) <- c("mdate","pref","city","NorthlatDec","EastlngDec",
                   "daichi_distance","AvgAirDoseRate")
air_11_15$mdate <- as.Date(air_11_15$mdate)
air_11_15$pref <- as.character(air_11_15$pref)
air_11_15$city <- as.character(air_11_15$city)
air_11_15$gride <- latlong_to_meshcode(lat = air_11_15$NorthlatDec, long = air_11_15$EastlngDec,order = 3) # 21350     8
#remove background radiations, jp govt sets at 0.04µSv/h
air_11_15<- subset(air_11_15, AvgAirDoseRate > 0.04) # 2776    8
#Calculate annual external dose rate
air_11_15$AnnualExtDose <- (air_11_15$AvgAirDoseRate - 0.04)*(8 + 16*0.4)*365/1000

#make cuts of Annual External Air Dose
air_11_15$AnnualExDoseRange <- cut(air_11_15$AnnualExtDose, c(0,1,5,10,20,30)) # 21327    10

#remove duplicate grides
# air_11_15 <- air_11_15[!duplicated(air_11_15$gride),] # 2392   10

air1115 <- air_11_15 
#cut out year out of date variable
air1115$n_year <- strftime(air_11_15$mdate, "%Y") #21,327    11
#get out repeats per year
air_11_15new <- air1115 %>% distinct(n_year, gride, .keep_all = TRUE) #12,469    11

air_11_15new$n_year <- as.numeric(air_11_15new$n_year)

#calculate area
air_11_15AnnualExDoseRange_summary <- data.frame(table(air_11_15$AnnualExDoseRange))
air_11_15AnnualExDoseRange_summary$Areakm2 <- air_11_15AnnualExDoseRange_summary$Freq
sum(air_11_15AnnualExDoseRange_summary$Areakm2)  # 
##########

#plots
p <- ggplot() +
        #geom_rect(data = sez, aes(xmin = SW_eLong, xmax = NE_eLong, ymin = SW_nLat, ymax = NE_nLat, fill="red"))+
        geom_point(data = air_11_15new, aes(x = EastlngDec, y = NorthlatDec, color = AnnualExDoseRange,shape=15))+
        scale_shape_identity()+
        scale_color_brewer(palette="Reds")+
        geom_polygon(data=fu_f,aes(x = long, y = lat, group = group),color="#999999",fill=NA)+
        coord_map()+
        annotate("text", x = 141.0328, y = 37.4211, label = "x",color="red", size=4)+
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank())
p + facet_wrap(~ n_year)

#Pie Charts
pie <- ggplot(air_11_15new, aes(x = "sq.km", fill = AnnualExDoseRange)) +
        geom_bar(width = 1) 
pie <- pie + coord_polar(theta = "y") 

pie <- scale_fill_brewer(palette="Reds")
pie + facet_grid(~ new_m)


# Consistence check in combined dataset
# Number of grides where measurements are taken per year
no_grides.pyear <- with(air_11_15new,aggregate(gride ~ n_year,FUN=function(x){length(unique(x))}))
# grides of each year
ya_gride11 <- subset(air_11_15new, n_year==2011, gride)
ya_gride12 <- subset(air_11_15new, n_year==2012, gride)
ya_gride13 <- subset(air_11_15new, n_year==2013, gride)
ya_gride14 <- subset(air_11_15new, n_year==2014, gride)
ya_gride15 <- subset(air_11_15new, n_year==2015, gride)
# unlist grides of each year into a numeric vector, iterable in intersect (a fun of sets)
yg11 <- unlist(ya_gride11[,1]) # 2525
yg12 <- unlist(ya_gride12[,1]) # 2457
yg13 <- unlist(ya_gride13[,1]) # 2395
yg14 <- unlist(ya_gride14[,1]) # 2548
yg15 <- unlist(ya_gride15[,1]) # 2544
# get common grides found in each of the 5years
common_grides <- Reduce(intersect, list(yg11,yg12,yg13,yg14,yg15)) #2,273 grides, 9092km2

#keep obs of common grides in all year

air12345 <- air_11_15new[air_11_15new$gride %in% common_grides,]   #1104/12,469 rows lost

write.csv(air12345, file = "air12345.csv")

air12345 <- read.csv("air12345.csv",header = TRUE, stringsAsFactors = FALSE)
air12345$AnnualExDoseRange <- as.factor(air12345$AnnualExDoseRange)
air12345$AnnualExDoseRange <- cut(air12345$AnnualExtDose, c(0,1,5,10,40)) # 21327    10

# plot of all 44 on common grides
p <- ggplot() +
        geom_point(data = air_2011, aes(x=SW_eLong,y=SW_nLat),size=3,color="grey85")+
        geom_point(data = air12345, aes(x = EastlngDec, y = NorthlatDec, color = AnnualExDoseRange,shape=15))+
        scale_shape_identity()+
        scale_color_brewer(palette="Reds")+
        geom_polygon(data=fu_f,aes(x = long, y = lat, group = group),color="#999999",fill=NA)+
        coord_map()+
        annotate("text", x = 141.0328, y = 37.4211, label = "x",color="red", size=4)+
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank())
p + facet_wrap(~ n_year)

pp <- ggplot(air12345)+
        geom_point(aes(AnnualExtDose))
pp


pp <- ggplot(air12345)+
        geom_bar(aes(n_year,fill=AnnualExDoseRange))+
        ggtitle("Annual External Dose Range per km^2")
        theme( plot.background=element_blank())
pp <- pp + scale_fill_brewer(palette = "Reds")
pp + scale_y_discrete(name ="Area (km2)", 
                      labels=c("2000","4000","6000","8000"))

### Annual Ext Dose Area Distribution
airArea <- air12345 %>% 
        group_by(n_year,AnnualExtDose) %>% 
        summarise(count=n()) %>% 
        mutate(tarea=count*4,AnnualExDoseRange = cut(AnnualExtDose, c(0,1,5,10,40)))
ggplot(airArea, aes(x = factor(n_year), y = tarea, fill = factor(AnnualExDoseRange))) +
        geom_bar(stat="identity", width = 0.7) +
        labs(x = "Year", y = expression(paste("Land Area ", km^{2})),title="Annual External Dose area distribution", fill = "External Dose/year") +
        theme_minimal(base_size = 14)+
        scale_fill_brewer(palette = "Reds")


#------------------------------------------------------------------------------------------------------------------------
# AIR DOSE RATE WITHOUT DECONTAMINATION
#------------------------------------------------------------------------------------------------------------------------
### AIR DOSE RATE WITHOUT DECONTAMINATION
# D(t)=D(0)∙[0.69*exp {-( λ134Cs)∙t}+0.27*exp{-(λ137Cs)*t}]  :exp((log(0.5)/2.06)*225/365)
# calculate dates from 2011 Nov 05th, decided to bench mark on 2012-02-21
#Example 0.25uSv/h reduce to 0.248206uSv/h after 11 days
#0.25*(0.69*exp((log(0.5)/2.06)*11/365)+0.31*exp((log(0.5)/30.17)*11/365))
#[1] 0.248206

air12345$no.days <- as.numeric(difftime(as.POSIXct(air12345$mdate),as.POSIXct("2012-02-21"),units="days"))
#subsets of each year from 2012-2015
air2012 <- air12345[air12345$n_year == 2012,]
air2012n <- subset(air2012, select = c("gride","AnnualExtDose"))

air2013 <- air12345[air12345$n_year == 2013,]
air2013n <- subset(air2013, select = c("gride","no.days"))
names(air2013n) <- c("gride","no.days13")

air2014 <- air12345[air12345$n_year == 2014,]
air2014n <- subset(air2014, select = c("gride","no.days"))
names(air2014n) <- c("gride","no.days14")

air2015 <- air12345[air12345$n_year == 2015,]
air2015n <- subset(air2015, select = c("gride","no.days"))
names(air2015n) <- c("gride","no.days15")
#merge the 4 df
air.undeco <- Reduce(function(...) merge(..., by="gride",all=TRUE), list(air2012n, air2013n, air2014n,air2015n))

air.undeco$unAnnualExtDose13 <- air.undeco$AnnualExtDose * (0.69*exp(-0.336*air.undeco$no.days13/365) + 0.31*exp(-0.023*air.undeco$no.days13/365))
# colnames(air.undeco)[6] <- "unAnnualExtDose13"
air.undeco$unAnnualExtDose14 <- air.undeco$AnnualExtDose * (0.69*exp(-0.336*air.undeco$no.days14/365) + 0.31*exp(-0.023*air.undeco$no.days14/365))
air.undeco$unAnnualExtDose15 <- air.undeco$AnnualExtDose * (0.69*exp(-0.336*air.undeco$no.days15/365) + 0.31*exp(-0.023*air.undeco$no.days15/365))
write.csv(air.undeco, file = "air.undeco20120221Bench.csv")
## join grides, years and no.days again to create a df of same dim as original

air.undeco5 <- subset(air.undeco,select = c("gride","unAnnualExtDose13","unAnnualExtDose14","unAnnualExtDose15"))
air.undeco5n <- melt(air.undeco5,id.vars = c("gride"))
colnames(air.undeco5n) <- c("gride","undeco_year","undeco.AnnualExtDose")
air.undeco5n$undeco_year <- gsub("unAnnualExtDose","20",air.undeco5n$undeco_year)
#Merge with original data set
air.deco.undeco = merge(air12345, air.undeco5n, by.x=c("gride", "n_year"), by.y=c("gride", "undeco_year"), all = TRUE)

#Keep AnnualExtDose before 2012-02-21 constant, since its before we count decaying
air.deco.undeco$undeco.AnnualExtDose[is.na(air.deco.undeco$undeco.AnnualExtDose)] <- air.deco.undeco$AnnualExtDose[is.na(air.deco.undeco$undeco.AnnualExtDose)]
write.csv(air.deco.undeco, file = "air.deco.undeco.csv",row.names = FALSE)

airdu <- read.csv("air.deco.undeco.csv")
airdu$mdate <- as.Date(airdu$mdate)
airdu$pref <- as.character(airdu$pref)
airdu$city <- as.character(airdu$city)
airdu$undeco.AnnualExtDoseRange <- cut(airdu$undeco.AnnualExtDose, c(0,1,5,10,40)) 
airdu <- airdu[order(airdu$n_year),]

#PLOTS
wudb.airArea <- airdu %>% 
        group_by(n_year,undeco.AnnualExtDose) %>% 
        summarise(kawt=n()) %>% 
        mutate(untarea=kawt*4,undeco.AnnualExDoseRange = cut(undeco.AnnualExtDose, c(0,1,5,10,40)))
ggplot(wudb.airArea, aes(x = factor(n_year), y = untarea, fill = factor(undeco.AnnualExDoseRange))) +
        geom_bar(stat="identity", width = 0.7) +
        labs(x = "Year", y = expression(paste("Land Area ", km^{2})),title="Would Be Annual External Dose Area Without Decontamination", fill = "External Dose/year") +
        theme_minimal(base_size = 14)+
        scale_fill_brewer(palette = "Greens")

#wub be map
q <- ggplot() +
        geom_point(data = air_2011tepco, aes(x=SW_eLong,y=SW_nLat),size=3,color="grey85")+
        geom_point(data = airdu, aes(x = EastlngDec, y = NorthlatDec, color = undeco.AnnualExtDoseRange,shape=15))+
        scale_shape_identity()+
        scale_color_brewer(palette="Greens")+
        geom_polygon(data=fu_f,aes(x = long, y = lat, group = group),color="#999999",fill=NA)+
        coord_map()+
        annotate("text", x = 141.0328, y = 37.4211, label = "x",color="red", size=4)+
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank())
q + facet_wrap(~ n_year)

# Compare
plot(y = airdu$undeco.AnnualExtDose,x = airdu$mdate, col = "red", ylab = "Avg Air Dose Rate", 
     xlab = "Year", main = "Compare AvgAirDoseRate Decontaminated and Undecontaminated",add = TRUE)
lines(airdu$AnnualExtDose, col = "green")
legend("topright", legend = c("Decontaminated", "Undecontaminated"))

#------------------------------------------------------------------------------------------------------------------------
# AIR DOSE PER TOWN
#------------------------------------------------------------------------------------------------------------------------
## Clean the towns names of airdu dataset
towu <- c("Ōtama", "Aizuwakamatsu" , "Date","Kawamata", "Kōri","Kunimi","Fukushima","Futaba","Hirono","Katsurao","Kawauchi","Namie",
          "Naraha","Ōkuma", "Tomioka", "Hanawa","Samegawa","Tanagura","Yamatsuri","Asakawa","Furudono","Hirata","Ishikawa","Tamakawa",
          "Iwaki","Kagamiishi","Ten'ei","Aizubange","Yanaizu","Yugawa","Kitakata","Kōriyama","Hinoemata","Minamiaizu","Shimogō","Tadami",
          "Minamisōma","Motomiya","Nihonmatsu","Izumizaki","Nakajima","Nishigou","Yabuki","Aizumisato","Kaneyama","Mishima","Shōwa",
          "Shirakawa","Sōma","Iitate","Shinchi","Sukagawa","Tamura","Miharu","Ono","Bandai","Inawashiro","Kitashiobara","Nishiaizu")

towu1 <- as.vector(unique(sort(air_2011$City)))

# Function to much the names
mgsub <- function(pattern, replacement, x, ...) {
        if (length(pattern)!=length(replacement)) {
                stop("pattern and replacement do not have the same length.")
        }
        result <- x
        for (i in 1:length(pattern)) {
                result <- gsub(pattern[i], replacement[i], result, ...)
        }
        result
}

airdu$cityn <- mgsub(towu1, towu, airdu$city)
#hand edited these towns too
airdu$cityn[airdu$cityn == "Kawauchi village"] <- "Kawauchi"
airdu$cityn[airdu$cityn == "Yugawa village"] <- "Yugawa"
airdu$cityn[airdu$cityn == "Yanaizu town"] <- "Yanaizu"
airdu$cityn[airdu$cityn == "Yamatsuri town"] <- "Yamatsuri"
airdu$cityn[airdu$cityn == "Yabuki town"] <- "Yabuki town"
airdu$cityn[airdu$cityn == "Watari county Yamamoto town"] <- "Yamamoto"
airdu$cityn[airdu$cityn == "Tenei village"] <- "Tenei"
airdu$cityn[airdu$cityn == "Tenei"] <- "Ten'ei"
airdu$cityn[airdu$cityn == "Tamakawa village"] <- "Tamakawa"
airdu$cityn[airdu$cityn == "Tanagura town"] <- "Tanagura"
airdu$cityn[airdu$cityn == "Tadami town"] <- "Tadami"
airdu$cityn[airdu$cityn == "Ono town"] <- "Ono"
airdu$cityn[airdu$cityn == "Nasu county Nasu "] <- "Nasu"
airdu$cityn[airdu$cityn == "Igu county Marumori"] <- "IguMarumori"
airdu$cityn[airdu$cityn == "Kitaibaraki city"] <- "Kitaibaraki"
airdu$cityn[airdu$cityn == "Kori "] <- "Kōri"
airdu$cityn[airdu$cityn == "Otama "] <- "Ōtama"
airdu$cityn[airdu$cityn == "Shimogo"] <- "Shimogō"
airdu$cityn[airdu$cityn == "Showa"] <- "Shōwa"
#gsub towns and villages to ""
airdu$cityn <- gsub("town","",airdu$cityn)
airdu$cityn <- gsub("village","",airdu$cityn)
#remove trailing white spaces
airdu$cityn <- trimws(airdu$cityn)
#towns with AnnualExtDose Above 1 
airdut <- subset(airdu,AnnualExtDose > 1) #3704   15
length(unique(airdut$city))

#descripative stats on towns
#percentage annual airdose reduction per 1km2
airdut$doseredp <- ((airdut$undeco.AnnualExtDose - airdut$AnnualExtDose)/(airdut$undeco.AnnualExtDose))*100

jiov <- summarise(group_by(airdut,cityn,n_year),kawt=n(), meanPerDecr = mean(doseredp))
jiov1 <- subset(jiov, !meanPerDecr == 0)
View(jiov1)
ggplot(jiov1, aes(x = cityn, y = percered, fill = n_year)) +
        geom_bar(stat="identity", width = 0.7) +
        labs(x = "Town", y ="Percentage Reduction",title="Percentage Reduction Annual Air Dose Decontaminated", fill = "External Dose/year") +
        theme_minimal(base_size = 14)+
        scale_fill_brewer(palette = "Greens")

g <- ggplot(jiov1, aes(cityn))
# Number of cars in each class:
g + geom_bar()

write.csv(airdut, file = "akitaprese.csv",row.names = FALSE)
airdut <- read.csv("akitaprese.csv")
write.csv(airdu, file = "akitalarge.csv",row.names = FALSE)
write.csv(jiov1, file = "akita//jiov1.csv",row.names = FALSE)

j3 <- subset(jiov1, select=c("cityn","n_year","meanPerDecr"))
j5 <- dcast(j3, cityn~n_year)
j6 <- na.omit(j5)
write.csv(j6, file = "akita//ftown.csv",row.names = FALSE)
names(airdut)[names(airdut) == 'gride'] <- 'gridcode'

aird <- subset(airdut,select=c(1,2,5,6,7,9,10,11,13,14,15,16,17))
#-----------------------------------------------------------------------------------------------------------------------
require(maps)
require(ggplot2)

fuk_towns <- subset(fortify(map_data('fu_admn')),
                    region %in% c(" ", " "))
first_circle <- fortify(subset(jp2, NAME_2=="Hirono" | NAME_2=="Iwaki" | NAME_2=="Kawauchi" | NAME_2=="Tamura" 
                               | NAME_2=="Nihonmatsu" | NAME_2=="Kawamata" | NAME_2=="Date" | NAME_2=="Sōma"| NAME_2=="Minamisōma"))
fuk_towns <- qplot(long, lat, data=first_circle, geom="polygon", group=group)
fuk_towns + coord_fixed() + facet_wrap(~region) # fix aspect ratio　to 1:1

# Using facet_grid instead of facet_wrap and adding space=free:
# gg_state + facet_grid(~region, scales = "free_x", space="free")

# Faceting on towns with ggplot2 
p <- ggplot() +
        geom_polygon(data=first_circle,aes(x = long, y = lat, group = group),color="#999999")+
        #geom_point(data = air12345, aes(x = EastlngDec, y = NorthlatDec))+
        #scale_color_brewer(palette="Reds")+
        coord_map()+
        theme(axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank())
p + facet_grid(~ group,scales = "free", space="free")

#------------------------------------------------------------------------------------------------------------------------
# FUKUSHIMA POPULATION
#------------------------------------------------------------------------------------------------------------------------
fuk_pop <- read.csv("44/fuk.csv") #10,831     5
# change 500m to 1km meshes
fuk_pop$gridcode <- strtrim(fuk_pop$gridcode,8) 
#sum 500m populations to make 1km
fuku <-plyr::ddply(fuk_pop, "gridcode", transform, totalpp=sum(totalpop), males=sum(male), females=sum(female),hshold=sum(household)) #10831     9
fuku1 <- subset(fuku, !duplicated(gridcode)) #3737    9
fuk2 <- subset(fuku1, select=c(1,6,7,8,9)) #3737    5
fuk3 <- subset(fuk2, select=c(1,2))
names(fuk3)
#------------------------------------------------------------------------------------------------------------------------
# FUKUSHIMA LAND USE
#------------------------------------------------------------------------------------------------------------------------
land <- read.csv(file = "landuse/90400000000_07.csv",header = TRUE)

names(land) <- c("gridcode","gridCenterNorthlat","gridCenterEastlng","landusee", 
                 "NE_nLat","NE_eLong","NW_nLat","NW_eLong",
                 "SW_nLat","SW_eLong","SE_nLat","SE_eLong")

# select urban, crops and paddy
land1 <- subset(land, landusee %in% c("Paddy","Crops","Urban"))

land1$gridcode <- gsub("_","",land1$gridcode)

land1$gridcode2 <- strtrim(land1$gridcode,8) #341345     13
#remove duplicate grides
land1 <- land1[!duplicated(land1$gridcode2),] #11571    13

land2 <- subset(land1, select=c("gridcode2","landusee"))
names(land2) <- c("gridcode", "landusee")
# MERGE 
airland <- Reduce(function(...) merge(..., by="gridcode",all=FALSE), list(aird, land2))
airland1 <- subset(airland, !doseredp == 0)


airpop <- Reduce(function(...) merge(..., by="gridcode",all=FALSE), list(aird, fuk3))
airpop1 <- subset(airpop, !doseredp == 0)


