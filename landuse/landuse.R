#land use
# http://nlftp.mlit.go.jp/ksj-e/index.html
http://nlftp.mlit.go.jp/ksj-e/gml/data/L03-a-09_5540-jgd_GML.zip
# The World Geodetic System (WGS) is a standard for use in cartography, geodesy, and navigation including GPS. 
#It comprises a standard coordinate system for the Earth, a standard spheroidal reference surface 
#(the datum or reference ellipsoid) for raw altitude data, and a gravitational equipotential surface 
# (the geoid) that defines the nominal sea level.
library(dplyr)
require(XML)
landuse <- xmlParse(file = "L03-18M-07-01/KS-META-L03-18M_07.xml")
class(landuse)
xmltop <- xmlRoot(landuse) # gives content of root
class(xmltop)
xmlName(xmltop)
xmlSize(xmltop)
xmlName(xmltop)[[1]]
xmltop[[1]]
xmltop[[2]]
xmlSize(xmltop[[1]])
xmlSApply(xmltop[[1]], xmlName)
xmlSApply(xmltop[[1]], xmlAttrs)
xmlSApply(xmltop[[1]], xmlSize)


land.df <-  xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue))
landdf <- data.frame(t(land.df),row.names = NULL)
landdf[1:5,1:4]


# Read .dat files

data <- read.table(file = "L03-18M-07-01/L03-18M-07-tky.dat", header = TRUE, skip = 3)
dim(data)

readLines("L03-18M-07-01/L03-18M-07-tky.dat", n = 10)

data1 <- read.delim("L03-18M-07-01/L03-18M-07-tky.dat",sep = "",header = TRUE)


#meuse data base ex
library(sp)
data(meuse)
head(meuse[,1:5]) #first 5 columns
coordinates(meuse) <- 1:2 #convert to spDF object using first 2cols
meuse[meuse$lead>500,1:5] #high lead, does as df but for spDF
meuse[meuse$lead<40,1:6]
# plot method of spDF
par(mfrow=c(1,2), mar=c(2,2,2,2))
plot(meuse, pch=20, main="Full Dataset", axes=TRUE)
plot(meuse,
     bg=rev(heat.colors(5))[cut(meuse$lead,breaks=c(0,100,200,300,400,Inf),labels=FALSE)],
     col="grey",main="lead Distribution",pch=21, axes=TRUE)
library(rgdal)
