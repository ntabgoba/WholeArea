# Populate 3 other 1km2 meshes basing on 4km2 samples
#Find freq of each mesh m, i.e has a1,a2,a3,a4,a5,a6
# Get last 2 values of m to get mn, var7, var8
# Step1:me = Increment var8 and append it to mn
# Step2:mn=  Increment var7 and insert btn mn and var1
# Step: mne= Increment Var7 and Var8
# Randomly Assign a,example; me = a4, mn = a2, mne = a5

library(stringr)
library(dplyr)
#5640 2597
#1234 5678
grid_maker <- function (grides)
{
        gridecode <- as.character(grides)
        
        if (length(grep("^[0-9]{8}", gridecode)) == 1) {
                mi <- substr(gridecode, start = 1, stop = 4)
                v5 <- substr(gridecode, start = 5, stop = 5)
                v6 <- substr(gridecode, start = 6, stop = 6)
                v7 <- substr(gridecode, start = 7, stop = 7)
                v8 <- substr(gridecode, start = 8, stop = 8)
                v55 <- as.numeric(v5) + 1
                v66 <- as.numeric(v6) + 1
                v77 <- as.numeric(v7) + 1
                v88 <- as.numeric(v8) + 1
                v0 <- 0
                if (v77 != 10 & v88 != 10){
                        me <- paste0(mi,v5,v6,v7,v88)
                        mn <- paste0(mi,v5,v6,v77,v8)
                        mne <- paste0(mi,v5,v6,v77,v88)
                }
                if(v88 == 10 & v77 != 10 ) {
                        me <- paste0(mi,v5,v66,v7,v0)
                        mn <- paste0(mi,v5,v6,v77,v8)
                        mne <- paste0(mi,v5,v66,v77,v0)
                }
                if(v77 == 10 & v88 != 10 ) {
                        me <- paste0(mi,v5,v6,v7,v88)
                        mn <- paste0(mi,v55,v6,v0,v8)
                        mne <- paste0(mi,v55,v6,v0,v88)
                }
                if(v77 == 10 & v88 == 10 ) {
                        me <- paste0(mi,v5,v66,v7,v0)
                        mn <- paste0(mi,v55,v6,v0,v8)
                        mne <- paste0(mi,v55,v66,v0,v0)
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
grdair_orderedg <- grdair[order(grdair$gride),]

grdair_gfreq <- summarise(group_by(grdair_orderedg,gride),length(gride))
View(grdair_gfreq)
names(grdair_gfreq) <- c("gride","lngth")

# vectorise the columns
gri <- grdair_gfreq[,1]
lngt <- grdair_gfreq[,2]

# loop through each of the above to create a new list
mylist <- list()
for (i in 1:length(gri)){
        lis <- grid_maker(grides = gri[i])
        mylist[[i]] <- lis
        }     
                

gri <- as.list(gri)
#jump
kags <- lapply(gri, grid_maker) 
length(kags)
kags


jio <- grid_maker(airdut$gride)
length(jio)
jio
