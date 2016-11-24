# Populate 3 other 1km2 meshes basing on 4km2 samples
#Find freq of each mesh m, i.e has a1,a2,a3,a4,a5,a6
# Get last 2 values of m to get mn, var7, var8
# Step1:me = Increment var8 and append it to mn
# Step2:mn=  Increment var7 and insert btn mn and var1
# Step: mne= Increment Var7 and Var8
# Randomly Assign a,example; me = a4, mn = a2, mne = a5

library(stringr)
#
grid_maker <- function (gride)
{
        gridecode <- as.character(gride)
        
        if (length(grep("^[0-9]{8}", gridecode)) == 1) {
                mi <- substr(gridecode, start = 1, stop = 6)
                v7 <- substr(gridecode, start = 7, stop = 7)
                v8 <- substr(gridecode, start = 8, stop = 8)

grid_maker(55402569)      

        if (length(grep("^[0-9]{6}", mesh)) == 1) { 
                mesh5 <- as.numeric(substring(mesh, 5, 5))
                mesh6 <- as.numeric(substring(mesh, 6, 6))
                lat_width  <- lat_width / 8;
                long_width <- long_width / 8;
        }
        
        if (length(grep("^[0-9]{8}", mesh)) == 1) { 
                mesh7 <- as.numeric(substring(mesh, 7, 7))
                mesh8 <- as.numeric(substring(mesh, 8, 8))
                lat_width  <- lat_width / 10;
                long_width <- long_width / 10;
        }
        
        
        lat  <- mesh12 * 2 / 3;          
        long <- mesh34 + 100;
        
        if (exists("mesh5") && exists("mesh6")) {  
                lat  <- lat  + (mesh5 * 2 / 3) / 8;
                long <- long +  mesh6 / 8;
        }
        if (exists("mesh7") && exists("mesh8")) {  
                lat  <- lat  + (mesh7 * 2 / 3) / 8 / 10;
                long <- long +  mesh8 / 8 / 10;
        }
        
        if (loc == "C") {   
                lat  <-  lat  + lat_width  / 2;
                long <-  long + long_width / 2;
        }
        if (length(grep("N", loc)) == 1) {  
                lat  <- lat  + lat_width;
        }
        if (length(grep("E", loc) == 1)) {  
                long <- long +long_width;
        }
        
        lat  <- sprintf("%.8f", lat); 
        long <- sprintf("%.8f", long);
        
        x <- list(as.numeric(lat), as.numeric(long))
        names(x) <- c("lat", "long")
        
        return (x)
}
# j <- mesh_latlong(mesh = 564000463 , loc = "C")
# class(j);print(j);length(j)
grides <- fuk_pop[,1]
head(grides);class(grides);length(grides)
#create the lat/long
mylist <- list()
for (i in 1:length(grides) ){
        lis <- mesh_latlong(mesh = grides[i], loc = "C")
        mylist[[i]] <- lis
}

res <- as.data.frame(mylist)
# View(res)
df <- data.frame(matrix(unlist(res), nrow=10831, byrow=T))
df <- ldply (res, data.frame)

#library(data.table)
df <- as.data.frame(rbindlist(mylist, fill=TRUE))
df[,"gridcode"] <- grides 
View(df)
# merge gridcode and lat/lon datasets of Fuk population
fuk_ll <- merge(fuk_pop, df, by.x = "gridcode", by.y = "gridcode", all = TRUE)
write.csv(fuk_pop, file="fuk_pop.csv",row.names = FALSE)
# load new dataset
fuk_pop <- read.csv("fuk_pop.csv")
no_unique <- length(unique(fuk_pop$gridcode))