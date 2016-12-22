#------------------------------------------------------------------------------------------------------------------------
# 4x4 km mesh Data Set 
#------------------------------------------------------------------------------------------------------------------------
library(leaflet)
library(dplyr)
library(jpmesh)
library(ggplot2)
library(reshape2)
library(tidyr)



# ************************************************************************************ Dec 10th 2016
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

# Number of grides where measurements are taken per year
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
air15s$gride.n <-gsub("[ [:punct:]]", "" , air15s$gride.n)
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
airx$gride.m <-gsub("[ [:punct:]]", "" , airx$gride.m)
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
air10$no.days <- as.numeric(difftime(as.POSIXct(air10$date),as.POSIXct("2012-02-21"),units="days"))
air10$AnnualExtDose = (air10$AvgAirDose - 0.04)*(8 + 16*0.4)*365/1000
air10$AnnualExDoseRange = cut(air10$AnnualExtDose, c(0,1,5,10,40))
#makde lat and long from grides
hirwa <- sapply(air101$gride.m, meshcode_to_latlon)
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
        geom_point(data = air10, aes(x = EastlngDec, y = NorthlatDec, color = AnnualExDoseRange,shape=15))+
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
airArea <- air10 %>% 
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

length(unique(air6$gride))# 6575 * 5 = 32875
#calcuate the would be un decontaminated Annual External Doses
air99 <- air9 %>%
        mutate(unAnnualExtDose11 = (AvgAirDose2011 - 0.04)*(8 + 16*0.4)*365/1000,
               unAnnualExtDose12 = (AvgAirDose2012 - 0.04)*(8 + 16*0.4)*365/1000,
               unAnnualExtDose13 = unAnnualExtDose12 * (0.69*exp(-0.336*447.375/365) + 0.31*exp(-0.023*447.375/365)),
               unAnnualExtDose14 = unAnnualExtDose12 * (0.69*exp(-0.336*812.375/365) + 0.31*exp(-0.023*812.375/365)),
               unAnnualExtDose15 = unAnnualExtDose12 * (0.69*exp(-0.336*1193.375/365) + 0.31*exp(-0.023*1193.375/365))
               )
########
dm1 <- melt(d[,c("Type","I.alt","idx06","idx07","idx08")], id=c("Type","I.alt"))
dm2 <- melt(d[,c("Type","I.alt","farve1","farve2")], id=c("Type","I.alt"))
colnames(dm2) <- c("Type", "I.alt", "variable2", "value2")
dm <- merge(dm1, dm2)
air991 <- melt(air99[,c("gride.m","city","AvgAirDose2011","AvgAirDose2012","AvgAirDose2013","AvgAirDose2014","AvgAirDose2015")],id.vars = c(1,2),variable.name = "Year", value.name = "AvgAirDose")
air992 <- melt(air99[,c("gride.m","city","unAnnualExtDose11","unAnnualExtDose12","unAnnualExtDose13","unAnnualExtDose14","unAnnualExtDose15")],id.vars = c(1,2),variable.name = "Year", value.name = "unAnnualExtDose")
air991$numb <- 1:32875
air992$numb <- 1:32875
air990 <- merge(air991,air992, by="numb")
air999 <- subset(air990,select = c(2,3,4,5,9))
names(air999) <- c("gride","city","Year","AvgAirDose","unAnnualExtDose")
air999$unAnnualExDoseRange = cut(air999$unAnnualExtDose, c(0,1,5,10,40))
#decontaminated air
air999$Year <- gsub("AvgAirDose","",air999$Year)
air999$date <- 0
air999$date[air999$Year=="2011"] <- "2011-04-12"
air999$date[air999$Year=="2012"] <- "2012-02-21"
air999$date[air999$Year=="2013"] <- "2013-05-13"
air999$date[air999$Year=="2014"] <- "2014-05-13"
air999$date[air999$Year=="2015"] <- "2015-05-29"
air999$date <- as.Date(air999$date)
air999$no.days <- as.numeric(difftime(as.POSIXct(air999$date),as.POSIXct("2012-02-21"),units="days"))
air999$AnnualExtDose = (air999$AvgAirDose - 0.04)*(8 + 16*0.4)*365/1000
air999$AnnualExDoseRange = cut(air999$AnnualExtDose, c(0,1,5,10,40))


########################################################################################
################################################################################################

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
        result <- x;
        for (i in 1:length(pattern)) {
                result <- gsub(pattern[i], replacement[i], result, ...)
                result1 <- gsub("town","",result)
                result2 <- gsub("village","",result1)
                result3 <- trimws(result2)
        }
        result3
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
airdut <- read.csv("akita//akitaprese.csv")
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

#plots
ggplot(data = airpop1) + 
        geom_point(mapping = aes(x = totalpp, y = doseredp))+
        geom_smooth(mapping = aes(x = totalpp, y = doseredp),se = FALSE)+
        labs(x = expression(paste("Population Density per ", km^{2})),y = "Mean Percentage Decrease (mSv/y)",title="Percentage Annual External Dose Rate Reduction in areas with >1mSv/y") +
        facet_wrap(~n_year)
#plots landuse and dose
airland2 <- subset(airland1, select=c(11,13,14))
airland3 <- dcast(airland2, cityn~landusee)
airland4 <- summarise(group_by(airland2,cityn,landusee),meanPerDecr = mean(doseredp))
airland5 <- dcast(airland4, cityn~landusee)
write.csv(airland5, file = "airland5.csv",row.names = FALSE)

ggplot(data = airland1,mapping = aes(x = landusee, y = doseredp)) + 
        geom_bar(stat="identity", width = 0.7)
