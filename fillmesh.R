# Populate 3 other 1km2 meshes basing on 4km2 samples
#Find freq of each mesh m, i.e has a1,a2,a3,a4,a5,a6
# Get last 2 values of m to get mn, var7, var8
# Step1:me = Increment var8 and append it to mn
# Step2:mn=  Increment var7 and insert btn mn and var1
# Step: mne= Increment Var7 and Var8
# Randomly Assign a,example; me = a4, mn = a2, mne = a5

library(stringr)
library(dplyr)
#
grid_maker <- function (grides)
{
        gridecode <- as.character(grides)
        
        if (length(grep("^[0-9]{8}", gridecode)) == 1) {
                mi <- substr(gridecode, start = 1, stop = 6)
                v7 <- substr(gridecode, start = 7, stop = 7)
                v8 <- substr(gridecode, start = 8, stop = 8)
                v77 <- as.numeric(v7) + 1
                v88 <- as.numeric(v8) + 1
                if (v77 != 10 & v88 != 10){
                        me <- paste0(mi,v7,v88)
                        mn <- paste0(mi,v77,v8)
                        mne <- paste0(mi,v77,v88)
                }
                if(v88 == 10) {
                        me <- NA
                        mn <- paste0(mi,v77,v8)
                        mne <- NA
                }
                if(v77 == 10) {
                        me <- paste0(mi,v7,v88)
                        mn <- NA
                        mne <- NA
                }
                newgrides <- list(as.character(grides),as.character(me), as.character(mn),as.character(mne))
                return (newgrides)
                }
}

### randomize assignment of air dose to new made grides
# group current grides with airdoses
# if a gride exists 2 or 3 times, subsitute its airdose values with its adjacent newly made grides

grdair <- air_11_15 %>% 
        group_by(AvgAirDoseRate,gride)

mylist <- list()
for (i in 1:length(grides) ){
        lis <- mesh_latlong(mesh = grides[i], loc = "C")
        mylist[[i]] <- lis
}
        summarise(count=n()) %>% 
        mutate(tarea=count*4,AnnualExDoseRange = cut(AnnualExtDose, c(0,1,5,10,40)))


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