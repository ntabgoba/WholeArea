require(rgdal)
require(leaflet)
land <- readOGR(dsn="fland",layer="L03-a-09_5540")
class(land)
slotNames(land)
plot(land)
names(land@data)
 
head(land)
names(land) <- c("Mesh","Field","Other_agricultural_land", "Forest","Wastelands",
                 "Land_for_Building","Road","Railway","Other_Land","RiverLake",
                 "Beach","Seaarea","Golfcourse")

### read in csv from jaea

land <- read.csv(file = "90400000000_07.csv",header = TRUE)

names(land) <- c("gridcode","gridCenterNorthlat","gridCenterEastlng","landusee", 
                 "NE_nLat","NE_eLong","NW_nLat","NW_eLong",
                 "SW_nLat","SW_eLong","SE_nLat","SE_eLong")


iro <- colorFactor(
        palette = "YlOrRd",
        domain = land$landusee
)

# Link of Daichi
land_plot <- leaflet() %>%
        addTiles()%>%
        addRectangles(data = land,lng1 = ~SW_eLong, lat1 = ~SW_nLat,
                      lng2 = ~NE_eLong, lat2 = ~NE_nLat,
                      color = ~iro(land$landusee))%>%
        addLegend("bottomright", pal = iro, values = land$landusee,
                  title = "Landuse",
                  opacity = 1)%>%
        addPopups(lat = 37.4211, lng = 141.0328,popup = "FDNPP") 
land_plot