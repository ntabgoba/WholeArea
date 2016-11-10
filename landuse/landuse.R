#land use
# http://nlftp.mlit.go.jp/ksj-e/index.html
# The World Geodetic System (WGS) is a standard for use in cartography, geodesy, and navigation including GPS. 
#It comprises a standard coordinate system for the Earth, a standard spheroidal reference surface 
#(the datum or reference ellipsoid) for raw altitude data, and a gravitational equipotential surface 
# (the geoid) that defines the nominal sea level.

require(XML)
landuse <- xmlParse(file = "L03-18M-07-01/KS-META-L03-18M_07.xml")

landuse_data <- xmlToList(landuse)
length(landuse_use)
L03-18M-07-tky.dat

readLines("L03-18M-07-01/L03-18M-07-tky.dat",n = 10)
