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

# Rectangle of fukushima pref
# SW:37.022015, 137.783550
# SE: 36.522711, 141.096468

# NE: 38.057683, 141.104679
# NW: 38.260231, 139.420979
plot(jp2, col = 'forestgreen', border = "lightgrey", xlim = c(139.420979,141.104679),ylim=c(38.057683,38.260231))
unique(jp2$NAME_1) #list of prefectures
unique(jp2$NAME_0) # list of countries
fu_adm <- jp2[jp2$NAME_1=="Fukushima",]
fu_cit <- fu_adm$NAME_2   # 60 cities,vilages,et in Fukushima pref

plot(fu_adm, border = 'gray', col = 'light gray') # plot of fuku pref & levl 2
args(points)

fu_hirata <- fu_adm[fu_adm$NAME_2 == "Hirata"] 
plot(fu_hirata)
#distribute data
regionalValues <- runif(60)  # Simulate a value for each region between 0 and 1
plot(fu_adm, col = magenta(regionalValues), border = 0)
class(regionalValues)

#middle jp
fuk_region<-subset(jp2,NAME_1=="Miyagi" | NAME_1=="Yamagata" | NAME_1=="Ibaraki"| NAME_1=="Gunma"| NAME_1=="Tochigi"| NAME_1=="Niigata" | NAME_1=="Fukushima")

library(ggplot2)
library(dplyr)
library(rgeos)
head(jp2@data,n=3)
#subset fukushima
fu_adm <- jp2[jp2$NAME_1=="Fukushima",]
plot(fu_adm)
#subset first circle
first_circle <- subset(jp2, NAME_2=="Hirono" | NAME_2=="Iwaki" | NAME_2=="Kawauchi" | NAME_2=="Tamura" 
                       | NAME_2=="Nihonmatsu" | NAME_2=="Kawamata" | NAME_2=="Date" | NAME_2=="Sōma"| NAME_2=="Minamisōma")
plot(first_circle, col='red', add = TRUE)

# 
fu_adm@data <- left_join(fu_adm@data, ag_extdose)
library(tmap)
qtm(shp = fu_adm, fill = "AnnualExtDose", fill.palette = "Reds")


tm_shape(fu_adm) +
        tm_fill("AnnualExtDose", thres.poly = 0) +
        tm_facets("NAME_2", free.coords=TRUE, drop.shapes=TRUE) +
        tm_layout(legend.show = FALSE, title.position = c("center", "center"), title.size = 20)

#subset on a few cities
lower_anextdose <- subset(fu_adm, AnnualExtDose > 1 & AnnualExtDose < 10)

qtm(shp = lower_anextdose, fill = "AnnualExtDose", fill.palette = "Reds")


