#land use
# http://nlftp.mlit.go.jp/ksj-e/index.html
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

