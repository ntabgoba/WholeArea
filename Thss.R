#------------------------------------------------------------------------------------------------------------------------
# 4x4 km mesh Data Set 
#------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(broom)
library(dplyr)
library(jpmesh)
library(reshape2)
library(tidyr)
library(sp)

# ************************************************************************************ Dec 10th 2016
jp2 <- readRDS("landuse/gdam/JPN_adm2.rds")
fu_adm <- jp2[jp2$NAME_1=="Fukushima",]
fu_f <- fortify(fu_adm)

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
air_11_15<- subset(air_11_15, AvgAirDoseRate > 0.04) # 21327     8
#unique grides
length(unique(air_11_15$gride)) #2,759 

# Consistence check in combined dataset
air <- air_11_15 
#cut out year out of date variable
air$n_year <- strftime(air$mdate, "%Y") #21,327    9

#get out gride repeats per year
# air_new <- air %>% distinct(n_year, gride, .keep_all = TRUE) #12,469    11

air$n_year <- as.factor(air$n_year)

# Find grides where measurements are made every
#no_grides.pyear <- with(air_11_15new,aggregate(gride ~ n_year,FUN=function(x){length(unique(x))}))
# grides of each year
air11 <- subset(air, n_year==2011, select=c(gride,mdate,city,AvgAirDoseRate))
air12 <- subset(air, n_year==2012, select=c(gride,mdate,city,AvgAirDoseRate))
air13 <- subset(air, n_year==2013, select=c(gride,mdate,city,AvgAirDoseRate))
air14 <- subset(air, n_year==2014, select=c(gride,mdate,city,AvgAirDoseRate))
air15 <- subset(air, n_year==2015, select=c(gride,mdate,city,AvgAirDoseRate))

#sizes of data collected on each year
cbind(dim(air11),dim(air12),dim(air13),dim(air14),dim(air15))
#apply maker on each year's grides
air11$gride.n <- lapply(air11$gride,grid_maker)
#clean the gride.n columns
air11s <- air11 %>% 
        mutate(gride.n = strsplit(as.character(gride.n), ",")) %>% 
        unnest(gride.n)
#remove punct
air11s$gride.n <-gsub("[ [:punct:]]", "" , air11s$gride.n)
air11s$gride.n <-gsub("list", "" , air11s$gride.n)
length(unique(air11s$gride.n)) #7031
air11s$gride <- NULL
air11t <- aggregate(AvgAirDoseRate ~ gride.n, data=air11s, FUN=function(x) c(maxi=max(x)))
names(air11t)[2] <- "AvgAirDose2011"

#2012
air12$gride.n <- lapply(air12$gride,grid_maker)
#clean the gride.n columns
air12s <- air12 %>% 
        mutate(gride.n = strsplit(as.character(gride.n), ",")) %>% 
        unnest(gride.n)
#remove punct
air12s$gride.n <-gsub("[ [:punct:]]", "" , air12s$gride.n)
air12s$gride.n <-gsub("list", "" , air12s$gride.n)
air12s$gride <- NULL
#air12ss <- aggregate(AvgAirDoseRate ~ gride.n, data=air12s, FUN=function(x) c(mean=mean(x), count=length(x)))
#Take maximum AvgAirDose for any given gride
air12t <- aggregate(AvgAirDoseRate ~ gride.n, data=air12s, FUN=function(x) c(maxi=max(x)))
length(unique(air12s$gride.n)) #6780
names(air12t)[2] <- "AvgAirDose2012"


#2013
air13$gride.n <- lapply(air13$gride,grid_maker)
#clean the gride.n columns
air13s <- air13 %>% 
        mutate(gride.n = strsplit(as.character(gride.n), ",")) %>% 
        unnest(gride.n)
#remove punct
air13s$gride.n <-gsub("[ [:punct:]]", "" , air13s$gride.n)
air13s$gride.n <-gsub("list", "" , air13s$gride.n)
length(unique(air13s$gride.n)) #6,716
air13t <- aggregate(AvgAirDoseRate ~ gride.n, data=air13s, FUN=function(x) c(maxi=max(x)))
names(air13t)[2] <- "AvgAirDose2013"

#2014
air14$gride.n <- lapply(air14$gride,grid_maker)
#clean the gride.n columns
air14s <- air14 %>% 
        mutate(gride.n = strsplit(as.character(gride.n), ",")) %>% 
        unnest(gride.n)
#remove punct
air14s$gride.n <-gsub("[ [:punct:]]", "" , air14s$gride.n)
air14s$gride.n <-gsub("list", "" , air14s$gride.n)
length(unique(air14s$gride.n)) #7171
air14t <- aggregate(AvgAirDoseRate ~ gride.n, data=air14s, FUN=function(x) c(maxi=max(x)))
names(air14t)[2] <- "AvgAirDose2014"

#2015
air15$gride.n <- lapply(air15$gride,grid_maker)
#clean the gride.n columns
air15s <- air15 %>% 
        mutate(gride.n = strsplit(as.character(gride.n), ",")) %>% 
        unnest(gride.n)
#remove punct
air15s$gride.n <-gsub("list", "" , air15s$gride.n)
length(unique(air15s$gride.n)) #7166
air15t <- aggregate(AvgAirDoseRate ~ gride.n, data=air15s, FUN=function(x) c(maxi=max(x)))
names(air15t)[2] <- "AvgAirDose2015"

#join datasets on to grides to get common grides
cbind(dim(air11t),dim(air12t),dim(air13t),dim(air14t),dim(air15t))
airt <- Reduce(function(...) merge(..., by="gride.n",all=TRUE), list(air11t,air12t, air13t, air14t,air15t))
airt <- na.omit(airt)

#grid_make air (Re make grides that match the )
air$gride.m <- lapply(air$gride, grid_maker)
airx <- air%>% 
        mutate(gride.m = strsplit(as.character(gride.m), ",")) %>% 
        unnest(gride.m)
#remove punct
airx$gride.m <-gsub("list", "" , airx$gride.m)

#combine names of towns' grides airdose of each year
air2 <- merge(x = airx, y = airt, by.x = "gride.m", by.y = "gride.n", all.y = TRUE)
#remove duplicate grides
air3 <- air2[!duplicated(air2$gride.m),]
air3$gride <- NULL
air3$n_year <- NULL
air3$AvgAirDoseRate <- NULL
air3$mdate <- NULL
# Assumption, gridmades belong to same town. Only, cordinates change
write.csv(air3, file = "14dec/air.gride.made.csv",row.names = FALSE)
############################################ saved dataset
air3 <- read.csv("14dec/air.gride.made.csv")

air5 <- subset(x = air3, select = c(1,3,7,8,9,10,11))
air6 <- melt(air5, id.vars = c(1,2), measure.vars = c(3,4,5,6,7), variable.name = "Year", value.name = "AvgAirDose")

## Clean the towns names of air6 dataset
tow <- c("Ōtama", "Aizuwakamatsu" , "Date","Kawamata", "Kōri","Kunimi","Fukushima","Futaba","Hirono","Katsurao","Kawauchi","Namie",
          "Naraha","Ōkuma", "Tomioka", "Hanawa","Samegawa","Tanagura","Yamatsuri","Asakawa","Furudono","Hirata","Ishikawa","Tamakawa",
          "Iwaki","Kagamiishi","Ten'ei","Aizubange","Yanaizu","Yugawa","Kitakata","Kōriyama","Hinoemata","Minamiaizu","Shimogō","Tadami",
          "Minamisōma","Motomiya","Nihonmatsu","Izumizaki","Nakajima","Nishigou","Yabuki","Aizumisato","Kaneyama","Mishima","Shōwa",
          "Shirakawa","Sōma","Iitate","Shinchi","Sukagawa","Tamura","Miharu","Ono","Bandai","Inawashiro","Kitashiobara","Nishiaizu")
air_2011 <- read.csv(file = "FukushimaJune2011.csv", header = TRUE) # 45,273 entries
tow1 <- as.vector(unique(sort(air_2011$City)))

# Function to much the names
mgsub <- function(pattern, replacement, x, ...) {
        if (length(pattern)!=length(replacement)) {
                stop("pattern and replacement do not have the same length.")
        }
        result <- x;
        for (i in 1:length(pattern)) {
                result <- gsub(pattern[i], replacement[i], result, ...)
                result1 <- gsub("town","",result)
                result2 <- gsub("village","",result1)
                result3 <- trimws(result2)
        }
        result3
}
##apply function
air6$city <- mgsub(tow1, tow, air6$city)
#remove towns of Miyagi and Ibaraki Prefectures
air7 <- air6[!air6$city =="Igu county Marumori",]
air8 <- air7[!air7$city =="Nasu county Nasu",]
air9 <- air8[!air8$city == "Kitaibaraki city",]
air10 <- air9[!air9$city == "Hitachiota city",]
air10$city[air10$city=="Kori"] <- "Kōri"
length(unique(air10$gride.m))# 6578 * 5 = 32890
air10$Year <- gsub("AvgAirDose","",air10$Year)
air10$date <- 0
air10$date[air10$Year=="2011"] <- "2011-04-12"
air10$date[air10$Year=="2012"] <- "2012-02-21"
air10$date[air10$Year=="2013"] <- "2013-05-13"
air10$date[air10$Year=="2014"] <- "2014-05-13"
air10$date[air10$Year=="2015"] <- "2015-05-29"
air10$date <- as.Date(air10$date)
air10$no.days <- as.integer(difftime(as.POSIXct(air10$date),as.POSIXct("2012-02-21"),units="days"))
air10$AnnualExtDose = (air10$AvgAirDose - 0.04)*(8 + 16*0.4)*365/1000
air10$AnnualExDoseRange = cut(air10$AnnualExtDose, c(0,1,5,10,40))
#makde lat and long from grides
hirwa <- sapply(air10$gride.m, meshcode_to_latlon)
hirwa1 <- as.data.frame(hirwa)
hirwa2 <- as.data.frame(t(hirwa1)) #transpose df
hirwa3 <- subset(hirwa2, select = c(1,2))
hirwa3$gride.m <- air10$gride.m 
#add new lon and lat to df
air10$EastlngDec <- unlist(hirwa3$long_center)
air10$NorthlatDec <- unlist(hirwa3$lat_center)

# plot of all 44 on common grides
p <- ggplot() +
        #geom_point(data = air_2011, aes(x=SW_eLong,y=SW_nLat),size=3,color="grey85")+
        geom_point(data = air9, aes(x = EastlngDec, y = NorthlatDec, color = AnnualExDoseRange,shape=15))+
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
p + facet_wrap(~ Year)


### Annual Ext Dose Area Distribution
airArea <- air8 %>% 
        group_by(Year,AnnualExtDose) %>% 
        summarise(count=n()) %>% 
        mutate(tarea=count,AnnualExDoseRange = cut(AnnualExtDose, c(0,1,5,10,40)))
ggplot(airArea, aes(x = factor(Year), y = tarea, fill = factor(AnnualExDoseRange))) +
        geom_bar(stat="identity", width = 0.7) +
        labs(x = "Year", y = expression(paste("Land Area ", km^{2})),title="Annual External Dose area distribution", fill = "External Dose/year") +
        theme_minimal(base_size = 14)+
        scale_fill_brewer(palette = "Reds")

write.csv(air10, file = "21Dec/air10.csv",row.names = FALSE)
#------------------------------------------------------------------------------------------------------------------------
# AIR DOSE RATE WITHOUT DECONTAMINATION
#------------------------------------------------------------------------------------------------------------------------
### AIR DOSE RATE WITHOUT DECONTAMINATION
# D(t)=D(0)∙[0.69*exp {-( λ134Cs)∙t}+0.27*exp{-(λ137Cs)*t}]  :exp((log(0.5)/2.06)*225/365)
# calculate dates from 2011 Nov 05th, decided to bench mark on 2012-02-21
#Example 0.25uSv/h reduce to 0.248206uSv/h after 11 days
#0.25*(0.69*exp((log(0.5)/2.06)*11/365)+0.31*exp((log(0.5)/30.17)*11/365))
#[1] 0.248206
air3 <- read.csv("14dec/air.gride.made.csv")

#~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Change in measure Airdose reductions
air3a <- air3 %>%
        mutate(red12 = (AvgAirDose2011 - AvgAirDose2012)/AvgAirDose2011,
               red23 = (AvgAirDose2012 - AvgAirDose2013)/AvgAirDose2012,
               red34 = (AvgAirDose2013 - AvgAirDose2014)/AvgAirDose2013,
               red45 = (AvgAirDose2014 - AvgAirDose2015)/AvgAirDose2014)
#mean percentage reductions in decontaminated
cbind(mean(air3a$red12),mean(air3a$red23),mean(air3a$red34),mean(air3a$red45))
# 0.3927644 0.2398724 0.2007111 0.06816164
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Change in measured Airdose reductions
air5 <- subset(x = air3, select = c(1,3,7,8,9,10,11))

## Clean the towns names of air6 dataset
tow <- c("Ōtama", "Aizuwakamatsu" , "Date","Kawamata", "Kōri","Kunimi","Fukushima","Futaba","Hirono","Katsurao","Kawauchi","Namie",
         "Naraha","Ōkuma", "Tomioka", "Hanawa","Samegawa","Tanagura","Yamatsuri","Asakawa","Furudono","Hirata","Ishikawa","Tamakawa",
         "Iwaki","Kagamiishi","Ten'ei","Aizubange","Yanaizu","Yugawa","Kitakata","Kōriyama","Hinoemata","Minamiaizu","Shimogō","Tadami",
         "Minamisōma","Motomiya","Nihonmatsu","Izumizaki","Nakajima","Nishigou","Yabuki","Aizumisato","Kaneyama","Mishima","Shōwa",
         "Shirakawa","Sōma","Iitate","Shinchi","Sukagawa","Tamura","Miharu","Ono","Bandai","Inawashiro","Kitashiobara","Nishiaizu")

air_2011 <- read.csv(file = "FukushimaJune2011.csv", header = TRUE) # 45,273 entries
tow1 <- as.vector(unique(sort(air_2011$City)))

# Function to much the names
mgsub <- function(pattern, replacement, x, ...) {
        if (length(pattern)!=length(replacement)) {
                stop("pattern and replacement do not have the same length.")
        }
        result <- x;
        for (i in 1:length(pattern)) {
                result <- gsub(pattern[i], replacement[i], result, ...)
        }
        result
}
##apply function
air5$city <- mgsub(tow1, tow, air5$city)
air5$city <- gsub("town","",air5$city)
air5$city <- gsub("village","",air5$city)
#remove trailing white spaces
air5$city <- gsub(" ","",air5$city)
#remove towns of Miyagi and Ibaraki Prefectures
air6 <- air5[!(air5$city %in% c("Hitachiotacity","Kitaibarakicity","IgucountyMarumori","WataricountyYamamoto","NasucountyNasu")),]
#equate spelling differences
air6$city[air6$city=="Shimogo"] <- "Shimogō"
air6$city[air6$city=="Showa"] <- "Shōwa"
air6$city[air6$city=="Tenei"] <- "Ten'ei"
air6$city[air6$city=="Nishigo"] <- "Nishigou"
air6$city[air6$city=="Otama" ] <- "Ōtama" 
air6$city[air6$city=="Kori"] <- "Kōri"

length(unique(air6$gride.m))# 6575 * 5 = 32875
#calculate the would be un decontaminated Annual External Doses
air7 <- air6 %>%
        mutate(unAnnualExtDose11 = (AvgAirDose2011 - 0.04)*(8 + 16*0.4)*365/1000,
               unAnnualExtDose12 = (AvgAirDose2012 - 0.04)*(8 + 16*0.4)*365/1000,
               unAnnualExtDose13 = unAnnualExtDose12 * (0.69*exp(-0.336*447/365) + 0.31*exp(-0.023*447/365)),       #0.7586292 coefs
               unAnnualExtDose14 = unAnnualExtDose12 * (0.69*exp(-0.336*812/365) + 0.31*exp(-0.023*812/365)),       #0.6212909
               unAnnualExtDose15 = unAnnualExtDose12 * (0.69*exp(-0.336*1193/365) + 0.31*exp(-0.023*1193/365))      #0.5176418
               )

########
air7a <- melt(air7[,c("gride.m","city","AvgAirDose2011","AvgAirDose2012","AvgAirDose2013","AvgAirDose2014","AvgAirDose2015")],id.vars = c(1,2),variable.name = "Year", value.name = "AvgAirDose")
air7b <- melt(air7[,c("gride.m","city","unAnnualExtDose11","unAnnualExtDose12","unAnnualExtDose13","unAnnualExtDose14","unAnnualExtDose15")],id.vars = c(1,2),variable.name = "Year", value.name = "unAnnualExtDose")
air7a$numb <- 1:32875
air7b$numb <- 1:32875
air7 <- merge(air7a,air7b, by="numb")
air8 <- subset(air7,select = c(2,3,4,5,9))
names(air8) <- c("gride","city","Year","AvgAirDose","unAnnualExtDose")
air8$unAnnualExDoseRange = cut(air8$unAnnualExtDose, c(0,1,5,10,40))
#decontaminated air
air8$Year <- gsub("AvgAirDose","",air8$Year)
air8$date <- 0
air8$date[air8$Year=="2011"] <- "2011-04-12"
air8$date[air8$Year=="2012"] <- "2012-02-21"
air8$date[air8$Year=="2013"] <- "2013-05-13"
air8$date[air8$Year=="2014"] <- "2014-05-13"
air8$date[air8$Year=="2015"] <- "2015-05-29"
air8$date <- as.Date(air8$date)
air8$no.days <- as.integer(difftime(as.POSIXct(air8$date),as.POSIXct("2012-02-21"),units="days"))
#Decontaminated Air External Air Dose
air8$AnnualExtDose = (air8$AvgAirDose - 0.04)*(8 + 16*0.4)*365/1000
########################################################################################
#importing the TEPCO dataset to illustrate the evacuated
air_2011tepco <- read.csv(file = "../CED/10200000002_07.csv", header = TRUE)
names(air_2011tepco) <- c("gridcode","sdate","edate","pref","city","no_samples","AvgAirDoseRate",
                          "NE_nLat","NE_eLong","NW_nLat","NW_eLong",
                          "SW_nLat","SW_eLong","SE_nLat","SE_eLong")
################################################################################################
#makde lat and long from grides
hirwa <- sapply(air8$gride, meshcode_to_latlon)
hirwa1 <- as.data.frame(hirwa)
hirwa2 <- as.data.frame(t(hirwa1)) #transpose df
hirwa3 <- subset(hirwa2, select = c(1,2))
hirwa3$idn <- 1:32875
hirwa3$NorthlatDec <- unlist(hirwa3$lat_center)
hirwa3$EastlngDec <- unlist(hirwa3$long_center)
hirwa3$lat_center <- NULL
hirwa3$long_center <- NULL
air8$idn <- 1:32875
#add new lon and lat to df
air9 <- merge(x = air8, y = hirwa3, by="idn",sort=FALSE)
#air9$no.days[air9$no.days < 0,] <- 0
bure <- c(0,1,5,10,40)
air9$AnnualExDoseRange <- cut(air9$AnnualExtDose,breaks = bure)
air9$unAnnualExDoseRange <- cut(air9$unAnnualExtDose, breaks = bure)

#PLOTS
wudb.airArea <- air13 %>% 
        group_by(Year,unAnnualExtDose) %>% 
        summarise(kawt=n()) %>% 
        mutate(untarea=kawt,unAnnualExDoseRange = cut(unAnnualExtDose, c(0,1,5,10,40)))
ggplot(wudb.airArea, aes(x = factor(Year), y = kawt, fill = factor(unAnnualExDoseRange))) +
        geom_bar(stat="identity", width = 0.7) +
        labs(x = "Year", y = expression(paste("Land Area ", km^{2})),title="Without Decontamination", fill = "External Dose/year") +
        theme_minimal(base_size = 14)+
        scale_fill_brewer(palette = "Greens")

#PLOTS
wudb.airArea1 <- air13 %>% 
        group_by(Year,AnnualExtDose) %>% 
        summarise(kawt1=n()) %>% 
        mutate(untarea=kawt1,AnnualExDoseRange = cut(AnnualExtDose, breaks=c(0,1,5,10,40)))
ggplot(wudb.airArea1, aes(x = factor(Year), y = kawt1, fill = factor(AnnualExDoseRange))) +
        geom_bar(stat="identity", width = 0.7) +
        labs(x = "Year", y = expression(paste("Land Area ", km^{2})),title="With Decontamination", fill = "External Dose/year") +
        theme_minimal(base_size = 14)+
        scale_fill_brewer(palette = "Reds")
#trial Jan 20th
airplot <- subset(air13, select = )
airplot <- melt(air13[,c("Year","unAnnualExtDose","AnnualExtDose")],id.vars = c(2,3),variable.name = "Year", value.name = "ExtAirDose")


#end trial

#wub be map
q <- ggplot() +
        geom_point(data = air_2011tepco, aes(x=SW_eLong,y=SW_nLat),size=3,color="grey85")+
        geom_point(data = air13, aes(x = EastlngDec, y = NorthlatDec, color = unAnnualExDoseRange,shape=15))+
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
              plot.background=element_blank(),
              strip.text = element_text(size=18))
q + facet_wrap(~ Year)

#decontaminated plot
q <- ggplot() +
        geom_point(data = air_2011tepco, aes(x=SW_eLong,y=SW_nLat),size=3,color="grey85")+
        geom_point(data = air13, aes(x = EastlngDec, y = NorthlatDec, color = AnnualExDoseRange,shape=15))+
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
              plot.background=element_blank(),
              strip.text = element_text(size=18))
q + facet_wrap(~ Year)

#descripative stats on towns
#percentage annual airdose reduction per 1km2
air9$AirDoseRedP <- ((air9$unAnnualExtDose - air9$AnnualExtDose)/(air9$unAnnualExtDose))*100
#------------------------------------------------------------------------------------------------------------------------
# AIR DOSE PER TOWN
#------------------------------------------------------------------------------------------------------------------------
towns <- summarise(group_by(air9,city,Year),kawt=n(), meanPerDecr = mean(AirDoseRedP))
towns1 <- subset(towns, !meanPerDecr == 0)
j3 <- subset(towns1, select=c("city","Year","meanPerDecr"))
j4 <- dcast(j3, city~Year) #df of city and meanPerDecr
write.csv(j4, file = "thesisVisuals/ftown.csv",row.names = FALSE)

#look at the negative element
air.ve <- subset(air9,air9$AirDoseRedP < 0)
length(unique(air.ve$gride)) # 3198 that have a negative increase
air.ve1 <- air.ve[air.ve$AnnualExtDose > 1,]
##Check -ve element
q <- ggplot() +
        geom_point(data = air_2011tepco, aes(x=SW_eLong,y=SW_nLat),size=3,color="grey85")+
        geom_point(data = air13, aes(x = EastlngDec, y = NorthlatDec, color = AnnualExDoseRange,shape=15))+
        scale_shape_identity()+
        scale_color_brewer(palette="Purples")+
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
q + facet_wrap(~ Year)

## check -ve element

#PLOTS
ggplot(towns1, aes(x = city, y = meanPerDecr, fill = Year)) +
        geom_bar(stat="identity", width = 0.7) +
        labs(x = "Town", y ="Percentage Reduction",title="Percentage Reduction Annual Air Dose Decontaminated", fill = "External Dose/year") +
        theme_minimal(base_size = 14)+
        scale_fill_brewer(palette = "Greens")

# names(airdut)[names(airdut) == 'gride'] <- 'gridcode'

#------------------------------------------------------------------------------------------------------------------------
# ALTITUTE
#------------------------------------------------------------------------------------------------------------------------
alt <- read.csv(file = "thesisVisuals/faltitude.csv")
names(alt) <- c("gridcode","gridCenterNorthlat","gridCenterEastlng","altitude", 
                 "NE_nLat","NE_eLong","NW_nLat","NW_eLong",
                 "SW_nLat","SW_eLong","SE_nLat","SE_eLong")
#lets turn them to 1km2
alt$gridcode <- gsub(pattern = "_",replacement = "",alt$gridcode)
alt$gridcode <- substr(alt$gridcode,start = 1, stop = 8)
#get maximum height for every 1km
alt1 <- aggregate(altitude ~ gridcode, data=alt, FUN=function(x) c(maxi=max(x)))
names(alt1) <- c("gride","MxAlt1Km")
cbind(dim(air9),dim(alt1))
#32,875 13,880
# 13     2
# add altitude to air9

air10 <- merge(air9,alt1, by = "gride", sort = FALSE)

#------------------------------------------------------------------------------------------------------------------------
# SOIL TYPE
#------------------------------------------------------------------------------------------------------------------------
soil <- read.csv(file = "thesisVisuals/fsoil.csv")
names(soil) <- c("gridcode","gridCenterNorthlat","gridCenterEastlng","sclass", 
                "NE_nLat","NE_eLong","NW_nLat","NW_eLong",
                "SW_nLat","SW_eLong","SE_nLat","SE_eLong")
#lets turn them to 1km2
soil$gridcode <- gsub(pattern = "_",replacement = "",soil$gridcode)
soil$gridcode <- substr(soil$gridcode,start = 1, stop = 8)
#get common soil type for every 1km
#table() the data, sort and then pick the first name
soil1 <- aggregate(sclass ~ gridcode, data=soil, FUN=function(x) names(sort(-table(x)))[1])
names(soil1) <- c("gride","mode.sclass")
cbind(dim(air10),dim(soil1))
#32,335 13,699
#14     2
# add soil to air10
air11 <- merge(air10,soil1, by = "gride", sort = FALSE, all.x = TRUE)

#------------------------------------------------------------------------------------------------------------------------
# FUKUSHIMA LAND USE
#------------------------------------------------------------------------------------------------------------------------
land <- read.csv(file = "landuse/90400000000_07.csv",header = TRUE)

names(land) <- c("gridcode","gridCenterNorthlat","gridCenterEastlng","landusee", 
                 "NE_nLat","NE_eLong","NW_nLat","NW_eLong",
                 "SW_nLat","SW_eLong","SE_nLat","SE_eLong")
land$gridcode <- gsub(pattern = "_",replacement = "",land$gridcode)
land$gridcode <- substr(land$gridcode,start = 1, stop = 8)
#get common landuse type for every 1km
#table() the data, sort and then pick the first name
land1 <- aggregate(landusee ~ gridcode, data=land, FUN=function(x) names(sort(-table(x)))[1])
names(land1) <- c("gride","mode.landuse")
cbind(dim(air11),dim(land1))
# 32335 13885
#   16   2
# add landusee to air11
air12 <- merge(air11,land1, by = "gride", sort = FALSE)
#rm(list=c("air.ve","air.ve1","air10","air11","air9","alt1","land","land1","soil","soil1"))
#------------------------------------------------------------------------------------------------------------------------
# FUKUSHIMA POPULATION
#------------------------------------------------------------------------------------------------------------------------
fuk_pop <- read.csv("44/fuk.csv") #10,831     5
# change 500m to 1km meshes
fuk_pop$gridcode <- substr(fuk_pop$gridcode,start=1,stop=8) 
#sum 500m populations to make 1km
fukp <- aggregate(totalpop ~ gridcode, data=fuk_pop, FUN=sum)
names(fukp) <- c("gride","totalpop")
cbind(dim(air12),dim(fukp))
# 32335 3737
# 17    2
# add landusee to air11
air13 <- merge(air12,fukp, by = "gride", sort = FALSE,all.x=TRUE)
length(air13$totalpop[!is.na(air13$totalpop)])
# sort air13

air13 <- dplyr::arrange(air13,idn)
air13$idn <- NULL

#------------------------------------------------------------------------------------------------------------------------
# DISTANCE TO FDNPP
#------------------------------------------------------------------------------------------------------------------------
# Calculate the distance to Daichi based on the lat and long
# Daichi Location,lat = 37.4211, lng = 141.0328
air13 <- air13[,c(1:10,12,11,13:17)]
air13$daichi.km <- sapply(1:nrow(air13),function(i)
        spDistsN1(as.matrix(air13[i,10:11]),matrix(c(141.0328,37.4211),nrow=1,ncol=2,byrow = TRUE),longlat=T))
write.csv(air13, file = "thesisVisuals/air13.csv",row.names = FALSE)

air13 <- read.csv("thesisVisuals/air13.csv")
bure = c(0,1,5,10,40)
air13$unAnnualExDoseRange <- cut(air13$unAnnualExtDose, breaks = bure)
air13$AnnualExDoseRange <- cut(air13$AnnualExtDose, breaks = bure)
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# END OF DATA WRANGLING  Dec 29th 2016
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

# relation btn un and annual Ex
air1345 <- subset(air13, air13$Year == "2013"|air13$Year == "2014"|air13$Year == "2015")
air.nofore <- subset(air1345, !air1345$mode.landuse == "Deciduous forest")
air.nofoeve <- subset(air.nofore,!air.nofore$mode.landuse == "Evergreen forest")
ggplot(air1345) + 
        geom_point(aes(unAnnualExtDose,AnnualExtDose))+
        facet_wrap(~mode.sclass)
#Large surface area's altitude is btn 300-600m, and 0-300m,600-900



#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#Supervised Learning
library(caret)
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#------------------------------------------------------------------------------------------------------------------------
# CUT THE DATASET INTO YEARLY CHUNKS, from 2013 to 2015
#------------------------------------------------------------------------------------------------------------------------
air13a <- air13[air13$Year == "2013" | air13$Year == "2014" | air13$Year == "2015",]
air13b <- na.omit(subset(air13a,select = c("AnnualExtDose","unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.sclass")))
air13b <- na.omit(subset(air.nam,select = c("Decontaminated.Dose","Undecontaminated.Dose", "Altitude","FDNPP.distance","Land.use","Soil.Type")))

airy11 <- air13[air13$Year == "2011",]
airy11 <- na.omit(subset(airy11,select = c("AnnualExtDose","unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.sclass")))
airy12 <- air13[air13$Year == "2012",]
airy12 <- na.omit(subset(airy12,select = c("AnnualExtDose","unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.sclass")))
airy13 <- air13[air13$Year == "2013",]
airy13 <- na.omit(subset(airy13,select = c("AnnualExtDose","unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.sclass")))
airy14 <- air13[air13$Year == "2014",]
airy14 <- na.omit(subset(airy14,select = c("AnnualExtDose","unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.sclass")))
airy15 <- air13[air13$Year == "2015",]
airy15 <- na.omit(subset(airy15,select = c("AnnualExtDose","unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.sclass")))
cbind(dim(airy11),dim(airy12),dim(airy13),dim(airy14),dim(airy15))

#train on sucessive years. variables being half life, altitude, soil type,popn,land use and daichi dist
base::setdiff(airy11$gride, airy12$gride)

fit11 <- lm(AnnualExtDose ~ unAnnualExtDose + MxAlt1Km + mode.sclass + mode.landuse + totalpop + daichi.km, data = airy11)
summary(fit11)
fit12 <- lm(AnnualExtDose ~ unAnnualExtDose + MxAlt1Km + mode.sclass + mode.landuse + totalpop + daichi.km, data = airy12)
summary(fit12)
fit13 <- lm(AnnualExtDose ~ unAnnualExtDose + MxAlt1Km + mode.sclass + mode.landuse + totalpop + daichi.km, data = airy13)
summary(fit13)
#unAnnualExtDose                7.283e-01  7.355e-03  99.023  < 2e-16 ***
#mode.sclassRocky soil          7.154e-01  4.870e-01   1.469 0.142007
fit14 <- lm(AnnualExtDose ~ unAnnualExtDose + MxAlt1Km + mode.sclass + mode.landuse + totalpop + daichi.km, data = airy13)
summary(fit13)
# 7.283e-01  7.355e-03  99.023  < 2e-16 ***
#7.154e-01  4.870e-01   1.469 0.142007
fit15 <- lm(AnnualExtDose ~ unAnnualExtDose + MxAlt1Km + mode.sclass + mode.landuse + totalpop + daichi.km, data = airy15)
summary(fit15)
#unAnnualExtDose                6.128e-01  9.521e-03  64.366  < 2e-16 ***

# IN NEGATIVE
pre.studiesError13 <- subset(airy13, airy13$AirDoseRedP < 0 | airy13$AirDoseRedP == 0) #2008/6467 Error:0.31
pre.studiesError14 <- subset(airy14, airy14$AirDoseRedP < 0 | airy14$AirDoseRedP == 0) #1255/6467 Error: 0.194
pre.studiesError15 <- subset(airy15, airy15$AirDoseRedP < 0 | airy15$AirDoseRedP == 0) #2084/6467 Error: 0.322
in.negativ <- air13[air13$AirDoseRedP < 0,]
in.negativ13 <- airy13[airy13$AirDoseRedP < 0,] #2008 
in.negativ14 <- airy14[airy14$AirDoseRedP < 0,]  #1255
in.negativ15 <- airy15[airy15$AirDoseRedP < 0,]  #2084
places34 <- base::setdiff(in.negativ13$gride, in.negativ14$gride) #503
places45 <- base::setdiff(in.negativ14$gride, in.negativ15$gride)  #1256
places35 <- base::setdiff(in.negativ13$gride, in.negativ15$gride) #903


#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#Density plots
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#Density plots
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
ggplot(airy11, aes(x = AnnualExtDose)) +
        theme_bw() +
        geom_density(aes(fill = mode.landuse), alpha = 0.5)

ggplot(airy11[airy11$AnnualExtDose < 10,], aes(x=AnnualExtDose))+
        geom_density(aes(colour=mode.landuse, fill=mode.landuse), alpha=0.3)+
        theme_bw() +
        ggtitle("AnnualExtDose Densities per landuse")

#ggtitle("Probability Density Estimates of Annual External Dose per Soil Type")
ggplot(airy11[airy11$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(position = "fill")+
        theme_bw() +
        labs(title="Year 2011",x = "Annual External Dose (mSv/year)", y = "density",fill = "Soil Type") 
#2012
ggplot(airy12[airy12$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(position = "fill")+
        theme_bw() +
        labs(title="Year 2012",x = "Annual External Dose (mSv/year)", y = "density",fill = "Soil Type") 
#2013
ggplot(airy13[airy13$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(position = "fill")+
        theme_bw() +
        labs(title="Year 2013",x = "Annual External Dose (mSv/year)", y = "density",fill = "Soil Type") 
#2014
ggplot(airy14[airy14$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(position = "fill")+
        theme_bw() +
        labs(title="Year 2014",x = "Annual External Dose (mSv/year)", y = "density",fill = "Soil Type") 
#2015
m <- ggplot(airy15[airy15$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(position = "fill")+
        labs(title="Year 2015",x = "Annual External Dose (mSv/year)", y = "density",fill = "Soil Type")
m + scale_fill_manual(values = c("#fb8072","#ffd92f","#b3de69","#8dd3c7","#33a02c","#ffffcc","#fddaec","#a6cee3"))
m + scale_fill_manual(values = c("#b3e2cd","#fdcdac","#cbd5e8","#f4cae4","#e6f5c9","#fff2ae","#f1e2cc","#cccccc"))       
## end per soil type

#reduce soil types to visualizeable
gt <- c("Brown forest soil","Glay soil","Podsol","Rocky soil","Red yellow soil","Lithosol","Kuroboku soil","Peat")
airso <- dplyr::filter(air13,mode.sclass %in% gt)

m <- ggplot(airso[airso$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.sclass))+
        geom_density(position = "fill")+
        labs(x = "Annual External Dose (mSv/year)", y = "density",fill = "Soil Type")+
        theme_bw() +
        facet_wrap(~Year)
m + scale_fill_manual(values = c("#fdb462",
                                 "#ffffb3",
                                 "#bebada",
                                 "#b3de69",
                                 "#8dd3c7",
                                 "#fb8072",
                                 
                                 "#fccde5",
                                 "#fdb462"))

m + scale_fill_manual(values = c("#8dd3c7",
                                 "#ffffb3",
                                 "#bebada",
                                 "#fb8072",
                                 "#80b1d3",
                                 "#fdb462",
                                 "#b3de69",
                                 "#fccde5",
                                 "#d9d9d9",
                                 "#bc80bd",
                                 "#ccebc5",
                                 "#ffed6f",
                                 "#b15928"))
m + scale_fill_manual(values = c("#fb8072","#ffd92f","#b3de69","#8dd3c7","#33a02c","#ffffcc","#fddaec","#a6cee3"))



#ggtitle("Probability Density Estimates of Annual External Dose per Land Use")
ggplot(airy11[airy11$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(position = "fill")+
        theme_bw() +
        labs(title="Year 2011",x = "Annual External Dose (mSv/year)", y = "density",fill = "Land use") 
#2012
ggplot(airy12[airy12$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(position = "fill")+
        theme_bw() +
        labs(title="Year 2012",x = "Annual External Dose (mSv/year)", y = "density",fill = "Land use") 
#2013
ggplot(airy13[airy13$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(position = "fill")+
        theme_bw() +
        labs(title="Year 2013",x = "Annual External Dose (mSv/year)", y = "density",fill = "Land use") 
#2014
ggplot(airy14[airy14$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(position = "fill")+
        theme_bw() +
        labs(title="Year 2014",x = "Annual External Dose (mSv/year)", y = "density",fill = "Soil Type") 
#2015
ggplot(airy15[airy15$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(position = "fill")+
        theme_bw() +
        labs(title="Year 2015",x = "Annual External Dose (mSv/year)", y = "density",fill = "Land use") 
## end per soil type
m <- ggplot(air13[air13$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(position = "fill")+
        theme_bw() +
        labs(x = "Annual External Dose (mSv/year)", y = "density",fill = "Land use")+
        facet_wrap(~Year)


#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#sampling
set.seed(122)
trainSamples <- sample(1:length(air13b$AnnualExtDose),size =length(air13b$AnnualExtDose)*0.6,replace = F )
train13 <- airy12[trainSamples,]
test13 <- airy12[-trainSamples,]

fit2 <- lm(AnnualExtDose ~ unAnnualExtDose + MxAlt1Km + daichi.km + mode.landuse + mode.sclass,data=train13)
#end sampling

# Predict annually 
#Year 2013
fit13 <- lm(AnnualExtDose ~ unAnnualExtDose + MxAlt1Km + daichi.km + mode.landuse + mode.sclass,data=airy13)
fit13df <- broom::tidy(fit13)
write.csv(file = "Thesis.Jan17/lmAll.csv")
#predict new values i.e AnnualExtDose of 2014
un.airy14 <- subset(airy14,select = c("unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.sclass"))
predicted14 <- predict(fit13,newdata=un.airy14)
##Training set and test set errors
# Calculate RMSE on training
sqrt(sum((fit13$fitted - airy13$AnnualExtDose)^2))
# Calculate RMSE on test dataset
sqrt(sum((predicted14 - airy14$AnnualExtDose)^2))


#predict new values i.e AnnualExtDose of 2015
un.airy15 <- subset(airy15,select = c("unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.sclass"))
predicted15 <- predict(fit13,newdata=un.airy15)

#predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")
##Training set and test set errors
# Calculate RMSE on training
sqrt(sum((fit13$fitted - airy13$AnnualExtDose)^2))
# Calculate RMSE on test dataset
sqrt(sum((predicted15 - airy15$AnnualExtDose)^2))


#IF LEVERAGE AND OUTLIERS ARE REMOVED
airy13.nolo <- subset(airy13, airy13$AnnualExtDose < 2)
airy14.nolo <- subset(airy14, airy13$AnnualExtDose < 2)
fit13.nolo <- lm(AnnualExtDose ~ unAnnualExtDose + MxAlt1Km + daichi.km + mode.landuse + mode.sclass,data=airy13.nolo)
fit13.nolo.df <- broom::tidy(fit13.nolo)
View(fit13.nolo.df)
write.csv(fit13.nolo.df,file = "Thesis.Jan17/lm.allpredictors.csv")
#predict on 2014
un.airy14.nolo <- subset(airy14.nolo,select = c("unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.sclass"))
predicted14.nolo <- predict(fit13.nolo,newdata=un.airy14.nolo)

# Calculate RMSE on training
sqrt(sum((fit13.nolo$fitted - airy13.nolo$AnnualExtDose)^2))
# Calculate RMSE on test dataset
sqrt(sum((predicted14.nolo - airy14.nolo$AnnualExtDose)^2))

#Reduce predictors
fit13.nolo1 <- lm(AnnualExtDose ~ unAnnualExtDose + MxAlt1Km + mode.sclass,data=airy13.nolo)

fit13.nolo1.df <- broom::tidy(fit13.nolo1)
View(fit13.nolo1.df)
write.csv(fit13.nolo1.df,file = "Thesis.Jan17/lm.3predictors.csv")
#predict on 2014
un.airy14.nolo1 <- subset(airy14.nolo,select = c("unAnnualExtDose", "MxAlt1Km","mode.sclass"))
predicted14.nolo1 <- predict(fit13.nolo1,newdata=un.airy14.nolo1)

# Calculate RMSE on training
sqrt(sum((fit13.nolo1$fitted - airy13.nolo$AnnualExtDose)^2))
# Calculate RMSE on test dataset
sqrt(sum((predicted14.nolo1 - airy14.nolo$AnnualExtDose)^2))

# End Predict annually 

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#RANDOM FOREST
library(randomForest)
set.seed(1505)
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
trainSamples <- sample(1:length(air13b$Decontaminated.Dose),size =length(air13b$Decontaminated.Dose)*0.6,replace = F )
train13 <- air13b[trainSamples,]
test13 <- air13b[-trainSamples,]

rf.air =randomForest(Decontaminated.Dose~.,data=train13)
rf.air

train.err=double(5)
test.err=double(5)
for(mtry in 1:5){
        fit=randomForest(Decontaminated.Dose~.,data=train13,mtry=mtry,ntree=300)
        train.err[mtry]=fit$mse[300]
        pred=predict(fit,test13)
        test.err[mtry]=with(test13,mean((Decontaminated.Dose-pred)^2))
        cat(mtry," ")
}
matplot(1:mtry,cbind(test.err,train.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("Train","Test"),pch=19,col=c("red","blue"))
title(main = "Graph of Train and Test Mean Squared Errors", xlab="Number of Features")


#
# View the forest results.
print(rf.air) 
# Importance of each predictor.
print(importance(rf.air,type = 2)) 
#Test this randomF on completely diff df of 2011
hahi <- head(air13a)
hahi1 <- na.omit(subset(hahi,select = c("AnnualExtDose","unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.sclass")))
hahi1$AnnualExtDose
# 0.21024 0.21024 0.21024 0.21024 0.21024 0.21024
predict(rf.air, hahi1)
# 0.2388158 0.2289263 0.2366579 0.2311324 0.2240987 0.2200885
hahi1$unAnnualExtDose
#0.1993678 0.1993678 0.1993678 0.1993678 0.1594942 0.1594942

#Train the rf model in areas that saw a rise in Radiations
in.negativ1 <- na.omit(subset(in.negativ,select = c("AnnualExtDose","unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.sclass")))

train.err=double(5)
test.err=double(5)
for(mtry in 1:5){
        fit=randomForest(AnnualExtDose~.,data=train13,mtry=mtry,ntree=300)
        train.err[mtry]=fit$mse[300]
        pred=predict(fit,in.negativ1)
        test.err[mtry]=with(in.negativ1,mean((AnnualExtDose-pred)^2))
        cat(mtry," ")
}
matplot(1:mtry,cbind(test.err,train.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("Train","Test"),pch=19,col=c("red","blue"))
title("Graph of Train and Test Mean Squared Errors")
#Visualize a single tree
library(rpart)
library(party)
library(tree)
# grow tree 
air_tree <- tree(Decontaminated.Dose~Land.use+Soil.Type+Altitude,air13b)
plot(air_tree)
text(air_tree, pretty = 0)
title("Annual External Air Dose Tree ")
# create attractive postcript plot of tree 
post(air_tree, file = "./tree2.ps", 
     title = "Annual External Air Dose Tree ")

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#BOSTING
library(gbm)
set.seed(1986)
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#put good names
air.nam <- rename(air13,
                  Undecontaminated.Dose=unAnnualExtDose,
                  Decontaminated.Dose=AnnualExtDose,
                  Days=no.days,
                  Altitude=MxAlt1Km,
                  Soil.Type=mode.sclass,
                  Land.use=mode.landuse,
                  Population=totalpop,
                  FDNPP.distance=daichi.km)
# end name change 
boost.air=gbm(Decontaminated.Dose~.,data=train13,distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4)
summary(boost.air)
par(mfrow=c(2,2))
plot(boost.air,i="Undecontaminated.Dose")
plot(boost.air,i="Altitude")
plot(boost.air,i= "FDNPP.distance")
plot(boost.air,i="Land.use")
plot(boost.air,i="Soil.Type")


n.trees=seq(from=100,to=10000,by=100)
predmat=predict(boost.air,newdata=test13,n.trees=n.trees)
dim(predmat)
berr=with(test13,apply( (predmat-Decontaminated.Dose)^2,2,mean))
par(mfrow=c(1,1))
plot(n.trees,berr,pch=19,ylab="Mean Squared Error", xlab="Number of Trees",main="Comparing Test Errors of Boosting and Random Forest")
abline(h=min(test.err),col="red",pch=59)
legend("topright",legend=c("RF","GBM"),pch=19,col=c("red","black"))

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#xgboost
library("xgboost")
set.seed(1994)
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


dtrain <- xgb.DMatrix(data = train13$data, label=train13$AnnualExtDose)
dtest <- xgb.DMatrix(data = test13$data, label=test13$AnnualExtDose)

bstDense <- xgboost(data = as.matrix(train13$data), label = train13$AnnualExtDose, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")

#linear
bst <- xgb.train(data=dtrain, booster = "gblinear", max.depth=2, nthread = 2, nround=2, watchlist=watchlist, eval.metric = "error", eval.metric = "logloss", objective = "binary:logistic")



#------------------------------------------------------------------------------------------------------------------------
# AIR DOSE PER TOWN
#------------------------------------------------------------------------------------------------------------------------
### 21st Jan 2017
#Annalyse number of towns with the positive airdose reduction
jio <- air9[air9$AirDoseRedP > 0,]
cbind(dim(jio[jio$Year == "2015",]),dim(jio[jio$Year == "2014",]),dim(jio[jio$Year == "2013",]))
# #Annalyse square km with the negative airdose reduction
gio <- air13[!air13$AirDoseRedP > 0,]
cbind(dim(jio[gio$Year == "2015",]),dim(gio[gio$Year == "2014",]),dim(gio[gio$Year == "2013",]))
#save df made since above
write.csv(air9,file = "thesisVisuals/air9.csv",row.names = FALSE)

# Towns with Annual External Dose > 1mSv/year
air1m <- air13[air13$AnnualExtDose > 1,]
towns1m <- summarise(group_by(air1m,city,Year),kawt=n(), meanPerDecr = mean(AirDoseRedP))
towns1345 <- subset(towns1m, !meanPerDecr == 0)

j3 <- subset(towns1345, select=c("city","Year","meanPerDecr"))
j4 <- na.omit(dcast(j3, city~Year)) #df of city and meanPerDecr
write.csv(j4,file = "thesisVisuals/town.meanR.csv",row.names = FALSE)

##
#Population
#
airpop <- air13[!air13$AirDoseRedP == 0,]
airpop1 <- na.omit(subset(airpop, select = c("Year","AirDoseRedP","totalpop")))
ggplot(airpop1) + 
        geom_point(aes(AirDoseRedP,totalpop))+
        geom_vline(xintercept = 0, colour="green", linetype = "longdash")+
        labs(list(title = "Population Density against Percentage Change of Annual External Air Doses", 
                  x = "Percentage Change of Annual External Dose (mSv/year)", y = expression ("Population density"~(persons/km^2))))+
        facet_wrap(~Year)

##
#Land use
#
airl <- air1m[!air1m$AirDoseRedP == 0,]
ggplot(airl) + 
        geom_point(aes(AirDoseRedP,mode.landuse))+
        geom_vline(xintercept = 0, colour="green", linetype = "longdash")+
        labs(list(title = "Land usage against Percentage Change of Annual External Air Doses (2013 to 2015)", 
                  x = "Percentage Change of Annual External Dose (mSv/y)", y = expression ("Land usage per"~km^2)))
   

# PREDICT FUTURE DOSES, 2021, 2041, 2071,2101
air.future <- air13[air13$Year == "2012" | air13$Year == "2013" | air13$Year == "2014" | air13$Year == "2015",]
air.future$date <- 0
#project dates
air.future$date[air.future$Year == "2012"] <- "2021-04-12"
air.future$date[air.future$Year == "2013"] <- "2041-04-12"
air.future$date[air.future$Year == "2014"] <- "2071-04-12"
air.future$date[air.future$Year == "2015"] <- "2101-04-12"
#project years
air.future$Year[air.future$Year == "2012"] <- "2021"
air.future$Year[air.future$Year == "2013"] <- "2041"
air.future$Year[air.future$Year == "2014"] <- "2071"
air.future$Year[air.future$Year == "2015"] <- "2101"
#nullify variables
air.future$AvgAirDose <- 0
air.future$unAnnualExtDose <- NULL
air.future$unAnnualExDoseRange <- 0
air.future$no.days <- 0
air.future$AnnualExtDose <- 0
air.future$AnnualExDoseRange <- 0
air.future$AirDoseRedP <- 0
#re-add the variables
air.future$date <- as.Date(air.future$date)
air.future$no.days <- as.integer(difftime(as.POSIXct(air.future$date),as.POSIXct("2012-02-21"),units="days"))
#append the benchmarked annual ext dose of 2012
air.bench <- subset(air13, select = c("gride","Year","Decontaminated.Dose"))
air.bench12 <- na.omit(air.bench[air.bench$Year == "2012",])
#after 10 years
air.bench21 <- air.bench12
air.bench21$Year <- "2021"
#after 30years
air.bench41 <- air.bench12 
air.bench41$Year <- "2041"
#after 60years
air.bench71 <- air.bench12 
air.bench71$Year <- "2071"
#after 90years
air.bench101 <- air.bench12 
air.bench101$Year <- "2101"
#combine all the projected df
cbind(names(air.bench21),names(air.bench41),names(air.bench71),names(air.bench101))
air.proj <- do.call("rbind", list(air.bench21, air.bench41, air.bench71,air.bench101))
colnames(air.proj) <- c("gride","Year","Undecontaminated.Dose")
#merge the projected years with static variables
air.future1 <- merge(air.future, air.proj, by.x = c("gride","Year"),by.y = c("gride","Year"),sort = FALSE)
air.future1$Undecontaminated.Dosee <- air.future1$unAnnualExtDose12 * (0.69*exp(-0.336*air.future1$no.days/365) + 0.31*exp(-0.023*air.future1$no.days/365))
air.future2 <- subset(air.future1,select = c(!is.na(MxAlt1Km),!is.na(daichi.km),!is.na(mode.landuse),!is.na(mode.sclass)))

#predict the distribution of future doses
fit.future <- randomForest(AnnualExtDose~.,data=train13,mtry=3,ntree=300)

# get predictors of air.future2 df
air.futureb <- subset(air.future2,select = c("AnnualExtDose","unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.sclass"))
air.future2$AnnualExtDose <- predict(fit.future, air.futureb)

#calculate map ranges
air.future2$AirDoseRedP <- ((air.future2$unAnnualExtDose - air.future2$AnnualExtDose)/(air.future2$unAnnualExtDose))*100
#previous annual ext dose due to background ( 0.04)*(8 + 16*0.4)*365/1000 = 0.21024
brks <- c(0.00,0.21,1.00,3.00,5.00)
air.future2$unAnnualExDoseRange = cut(air.future2$unAnnualExtDose, breaks = brks)
air.future2$AnnualExDoseRange = cut(air.future2$AnnualExtDose, breaks = brks)

#save file
write.csv(air.future2, file = "thesisVisuals/air.future.csv",row.names = FALSE)
#visualize
#decontaminated plot
q <- ggplot() +
        geom_point(data = air_2011tepco, aes(x=SW_eLong,y=SW_nLat),size=3,color="grey85")+
        geom_point(data = air.future2, aes(x = EastlngDec, y = NorthlatDec, color = AnnualExDoseRange,shape=15))+
        scale_shape_identity()+
        scale_color_brewer(palette="Blues")+
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
              plot.background=element_blank(),
              strip.text = element_text(size=18))
q + facet_wrap(~ Year)

#quantified
airArea.future <- air.future2 %>% 
        group_by(Year,AnnualExtDose) %>% 
        summarise(kawt=n()) %>% 
        mutate(untarea=kawt, AnnualExDoseRange = cut(AnnualExtDose, breaks = brks))
ggplot(airArea.future, aes(x = factor(Year), y = kawt, fill = factor(AnnualExDoseRange))) +
        geom_bar(stat="identity", width = 0.7) +
        labs(x = "Year", y = expression(paste("Land Area ", km^{2})),title="Decontaminated area based on current trend", fill = "External Dose/year") +
        theme_minimal(base_size = 14)+
        scale_fill_brewer(palette = "Blues")

#location maps
q <- ggplot() +
        geom_polygon(data=fu_f,aes(x = long, y = lat, group = group),fill="yellowgreen")+
        geom_point(data = air_2011tepco, aes(x=SW_eLong,y=SW_nLat),size=3,color="lightpink")+
        coord_map()+
        annotate("text", x = 141.0328, y = 37.4211, label = "x",color="red", size=4)+
        labs(main = "Fukushima Prefecture")+
        theme_opts
q 

# Predicted vs Observed plots
# https://www.r-bloggers.com/part-4a-modelling-predicting-the-amount-of-rain/
library(tidyr)
all.predictions <- gather(all.predictions,key = model,value = predictions,2:6)
ggplot(data = all.predictions,aes(x = actual, y = predictions)) + 
        geom_point(colour = "blue") + 
        geom_abline(intercept = 0, slope = 1, colour = "red") +
        geom_vline(xintercept = 23, colour = "green", linetype = "dashed") +
        facet_wrap(~ model,ncol = 2) + 
        coord_cartesian(xlim = c(0,70),ylim = c(0,70)) +
        ggtitle("Predicted vs. Actual, by model")
