## working on landuse of fukushima prefecture on lng/lat dataset from jaea
require(rgdal)
require(leaflet)

names(land) <- c("Mesh","Field","Other_agricultural_land", "Forest","Wastelands",
                 "Land_for_Building","Road","Railway","Other_Land","RiverLake",
                 "Beach","Seaarea","Golfcourse")

### read in csv from jaea

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


# Functionals adv-r
random <- function(f) f(runif(1e3))
random(mean)
random(mean)
random(sum)
# split apply combine 
lapply2 <- function(x,f, ...){
        out <- vector("list", length(x)) # create list of length x
        for(i in seq_along(x)){
                out[[i]] <- f(x[[i]], ...)
        }
        out
}

# Create some random data
l <- replicate(20, runif(sample(1:10, 1)), simplify = FALSE)

# With a for loop
out <- vector("list", length(l))
for (i in seq_along(l)) {
        out[[i]] <- length(l[[i]])
}
unlist(out)
#>  [1]  8  9  4  4  9  8  6  9  3  5  5  2  9  6  3  9 10  1  9  8

# With lapply
order(unlist(lapply(l, length)))
#>  [1]  8  9  4  4  9  8  6  9  3  5  5  2  9  6  3  9 10  1  9  8

order(unlist(l))





