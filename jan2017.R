#------------------------------------------------------------------------------------------------------------------------
# 4x4 km mesh Data Set 
#------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(broom)
library(dplyr)
library(jpmesh)
library(reshape2)
library(tidyr)
library(sp)

# ************************************************************************************ Dec 10th 2016
jp2 <- readRDS("landuse/gdam/JPN_adm2.rds")
fu_adm <- jp2[jp2$NAME_1=="Fukushima",]
fu_f <- fortify(fu_adm)

#------------------------------------------------------------------------------------------------------------------------

########################################################################################
#importing the TEPCO dataset to illustrate the evacuated
air_2011tepco <- read.csv(file = "../CED/10200000002_07.csv", header = TRUE)
names(air_2011tepco) <- c("gridcode","sdate","edate","pref","city","no_samples","AvgAirDoseRate",
                          "NE_nLat","NE_eLong","NW_nLat","NW_eLong",
                          "SW_nLat","SW_eLong","SE_nLat","SE_eLong")
################################################################################################
#------------------------------------------------------------------------------------------------------------------------
# AIR DOSE PER TOWN
#------------------------------------------------------------------------------------------------------------------------





write.csv(air13, file = "thesisVisuals/air13.csv",row.names = FALSE)
air13 <- read.csv("thesisVisuals/air13.csv")
bure = c(0,1,5,10,40)
air13$unAnnualExDoseRange <- cut(air13$unAnnualExtDose, breaks = bure)
air13$AnnualExDoseRange <- cut(air13$AnnualExtDose, breaks = bure)
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# END OF DATA WRANGLING  Dec 29th 2016
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
airy11 <- air13b[air13b$Year == "2011",]
airy11 <- na.omit(subset(airy11,select = c("AnnualExtDose","unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.sclass")))
airy12 <- air13b[air13b$Year == "2012",]
airy12 <- na.omit(subset(airy12,select = c("AnnualExtDose","unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.sclass")))
airy13 <- air13b[air13b$Year == "2013",]
airy13 <- na.omit(subset(airy13,select = c("AnnualExtDose","unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.sclass")))
airy14 <- air13b[air13b$Year == "2014",]
airy14 <- na.omit(subset(airy14,select = c("AnnualExtDose","unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.sclass")))
airy15 <- air13b[air13b$Year == "2015",]
airy15 <- na.omit(subset(airy15,select = c("AnnualExtDose","unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.sclass")))

#xxxxxxxxxxxxxxxxxxxxxxxxx air13
#PLOTS
wudb.airArea <- air13 %>% 
        group_by(Year,unAnnualExtDose) %>% 
        summarise(kawt=n()) %>% 
        mutate(untarea=kawt,unAnnualExDoseRange = cut(unAnnualExtDose, c(0,1,5,10,40)))
ggplot(wudb.airArea, aes(x = factor(Year), y = kawt, fill = factor(unAnnualExDoseRange))) +
        geom_bar(stat="identity", width = 0.7) +
        labs(x = "Year", y = expression(paste("Land Area ", km^{2})),title="Without Decontamination", fill = "External Dose(mSv/year)") +
        theme_minimal(base_size = 14)+
        scale_fill_brewer(palette = "Greens")

#PLOTS
wudb.airArea1 <- air13 %>% 
        group_by(Year,AnnualExtDose) %>% 
        summarise(kawt1=n()) %>% 
        mutate(untarea=kawt1,AnnualExDoseRange = cut(AnnualExtDose, breaks=c(0,1,5,10,40)))
ggplot(wudb.airArea1, aes(x = factor(Year), y = kawt1, fill = factor(AnnualExDoseRange))) +
        geom_bar(stat="identity", width = 0.7) +
        labs(x = "Year", y = expression(paste("Land Area ", km^{2})),title="With Decontamination", fill = "External Dose(mSv/year)") +
        theme_minimal(base_size = 14)+
        scale_fill_brewer(palette = "Reds")

#trial Jan 20th
airplot <- subset(air13, select = )
airplot <- melt(air13[,c("Year","unAnnualExtDose","AnnualExtDose")],id.vars = c(2,3),variable.name = "Year", value.name = "ExtAirDose")


#end trial

#wub be map
q <- ggplot() +
        geom_point(data = air_2011tepco, aes(x=SW_eLong,y=SW_nLat),size=3,color="grey85")+
        geom_point(data = air13, aes(x = EastlngDec, y = NorthlatDec, color = Annual_External_Dose,shape=15))+
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
              plot.background=element_blank(),
              strip.text = element_text(size=18))
q + facet_wrap(~ Year)

#decontaminated plot
q <- ggplot() +
        geom_point(data = air_2011tepco, aes(x=SW_eLong,y=SW_nLat),size=3,color="grey85")+
        geom_point(data = air13, aes(x = EastlngDec, y = NorthlatDec, color = Annual_External_Dose,shape=15))+
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
              plot.background=element_blank(),
              strip.text = element_text(size=18))
q + facet_wrap(~ Year)






#unAnnualExtDose                7.283e-01  7.355e-03  99.023  < 2e-16 ***
#mode.landuseRocky soil          7.154e-01  4.870e-01   1.469 0.142007
fit14 <- lm(AnnualExtDose ~ unAnnualExtDose + MxAlt1Km + mode.landuse + mode.landuse + totalpop + daichi.km, data = airy13)
summary(fit13)
# 7.283e-01  7.355e-03  99.023  < 2e-16 ***
#7.154e-01  4.870e-01   1.469 0.142007
fit15 <- lm(AnnualExtDose ~ unAnnualExtDose + MxAlt1Km + mode.landuse + mode.landuse + totalpop + daichi.km, data = airy15)
summary(fit15)
#unAnnualExtDose                6.128e-01  9.521e-03  64.366  < 2e-16 ***

# IN NEGATIVE


#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#Density plots
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
ggplot(airy11, aes(x = AnnualExtDose)) +
        theme_bw() +
        geom_density(aes(fill = mode.landuse), alpha = 0.5)

ggplot(airy11[airy11$AnnualExtDose < 10,], aes(x=AnnualExtDose))+
        geom_density(aes(colour=mode.landuse, fill=mode.landuse), alpha=0.3)+
        theme_bw() +
        ggtitle("AnnualExtDose Densities per landuse")

#ggtitle("Probability Density Estimates of Annual External Dose per Soil Type")
ggplot(airy11[airy11$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(aes(y=..scaled..))+
        theme_bw() +
        labs(title="Year 2011",x = "Annual External Dose (mSv/year)", y = "density",fill = "Soil Type") 
#2012
ggplot(airy12[airy12$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(aes(y=..scaled..))+
        theme_bw() +
        labs(title="Year 2012",x = "Annual External Dose (mSv/year)", y = "density",fill = "Soil Type") 
#2013
ggplot(airy13[airy13$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(aes(y=..scaled..))+
        theme_bw() +
        labs(title="Year 2013",x = "Annual External Dose (mSv/year)", y = "density",fill = "Soil Type") 
#2014
ggplot(airy14[airy14$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(aes(y=..scaled..))+
        theme_bw() +
        labs(title="Year 2014",x = "Annual External Dose (mSv/year)", y = "density",fill = "Soil Type") 
#2015
ggplot(airy15[airy15$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(aes(y=..scaled..))+
        theme_bw() +
        labs(title="Year 2015",x = "Annual External Dose (mSv/year)", y = "density",fill = "Soil Type") 
## end per soil type

#ggtitle("Probability Density Estimates of Annual External Dose per Land Use")
ggplot(airy11[airy11$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(position = "fill")+
        theme_bw() +
        labs(title="Year 2011",x = "Annual External Dose (mSv/year)", y = "density",fill = "Land use") 
#2012
ggplot(airy12[airy12$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(position = "fill")+
        theme_bw() +
        labs(title="Year 2012",x = "Annual External Dose (mSv/year)", y = "density",fill = "Land use") 
#2013
ggplot(airy13[airy13$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(position = "fill")+
        theme_bw() +
        labs(title="Year 2013",x = "Annual External Dose (mSv/year)", y = "density",fill = "Land use") 
#2014
ggplot(airy14[airy14$AnnualExtDose < 6,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(position = "fill")+
        theme_bw() +
        labs(title="Year 2014",x = "Annual External Dose (mSv/year)", y = "density",fill = "Soil Type") +
        xlim(0, 6)
#2015
ggplot(airy15[airy15$AnnualExtDose < 5,], aes(x=AnnualExtDose,fill=mode.landuse))+
        geom_density(position = "fill")+
        theme_bw() +
        labs(title="Year 2015",x = "Annual External Dose (mSv/year)", y = "density",fill = "Land use") 
## end per soil type

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

#sampling
set.seed(122)
trainSamples <- sample(1:length(air13b$AnnualExtDose),size =length(air13b$AnnualExtDose)*0.6,replace = F )
train13 <- airy12[trainSamples,]
test13 <- airy12[-trainSamples,]

fit2 <- lm(AnnualExtDose ~ unAnnualExtDose + MxAlt1Km + daichi.km + mode.landuse + mode.landuse,data=train13)
#end sampling

# Predict annually 
#Year 2013
fit13 <- lm(AnnualExtDose ~ unAnnualExtDose + MxAlt1Km + daichi.km + mode.landuse + mode.landuse,data=airy13)
fit13df <- broom::tidy(fit13)
write.csv(file = "Thesis.Jan17/lmAll.csv")
#predict new values i.e AnnualExtDose of 2014
un.airy14 <- subset(airy14,select = c("unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.landuse"))
predicted14 <- predict(fit13,newdata=un.airy14)
##Training set and test set errors
# Calculate RMSE on training
sqrt(sum((fit13$fitted - airy13$AnnualExtDose)^2))
# Calculate RMSE on test dataset
sqrt(sum((predicted14 - airy14$AnnualExtDose)^2))


#predict new values i.e AnnualExtDose of 2015
un.airy15 <- subset(airy15,select = c("unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.landuse"))
predicted15 <- predict(fit13,newdata=un.airy15)

#predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")
##Training set and test set errors
# Calculate RMSE on training
sqrt(sum((fit13$fitted - airy13$AnnualExtDose)^2))
# Calculate RMSE on test dataset
sqrt(sum((predicted15 - airy15$AnnualExtDose)^2))


#IF LEVERAGE AND OUTLIERS ARE REMOVED
airy13.nolo <- subset(airy13, airy13$AnnualExtDose < 2)
airy14.nolo <- subset(airy14, airy13$AnnualExtDose < 2)
fit13.nolo <- lm(AnnualExtDose ~ unAnnualExtDose + MxAlt1Km + daichi.km + mode.landuse + mode.landuse,data=airy13.nolo)
fit13.nolo.df <- broom::tidy(fit13.nolo)
View(fit13.nolo.df)
write.csv(fit13.nolo.df,file = "Thesis.Jan17/lm.allpredictors.csv")
#predict on 2014
un.airy14.nolo <- subset(airy14.nolo,select = c("unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.landuse"))
predicted14.nolo <- predict(fit13.nolo,newdata=un.airy14.nolo)

# Calculate RMSE on training
sqrt(sum((fit13.nolo$fitted - airy13.nolo$AnnualExtDose)^2))
# Calculate RMSE on test dataset
sqrt(sum((predicted14.nolo - airy14.nolo$AnnualExtDose)^2))

#Reduce predictors
fit13.nolo1 <- lm(AnnualExtDose ~ unAnnualExtDose + MxAlt1Km + mode.landuse,data=airy13.nolo)

fit13.nolo1.df <- broom::tidy(fit13.nolo1)
View(fit13.nolo1.df)
write.csv(fit13.nolo1.df,file = "Thesis.Jan17/lm.3predictors.csv")
#predict on 2014
un.airy14.nolo1 <- subset(airy14.nolo,select = c("unAnnualExtDose", "MxAlt1Km","mode.landuse"))
predicted14.nolo1 <- predict(fit13.nolo1,newdata=un.airy14.nolo1)

# Calculate RMSE on training
sqrt(sum((fit13.nolo1$fitted - airy13.nolo$AnnualExtDose)^2))
# Calculate RMSE on test dataset
sqrt(sum((predicted14.nolo1 - airy14.nolo$AnnualExtDose)^2))

# End Predict annually 

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#RANDOM FOREST
library(randomForest)
set.seed(1505)
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
trainSamples <- sample(1:length(air13b$AnnualExtDose),size =length(air13b$AnnualExtDose)*0.6,replace = F )
train13 <- air13b[trainSamples,]
test13 <- air13b[-trainSamples,]

rf.air =randomForest(AnnualExtDose~.,data=train13)
rf.air

train.err=double(5)
test.err=double(5)
for(mtry in 1:5){
        fit=randomForest(AnnualExtDose~.,data=train13,mtry=mtry,ntree=300)
        train.err[mtry]=fit$mse[300]
        pred=predict(fit,test13)
        test.err[mtry]=with(test13,mean((AnnualExtDose-pred)^2))
        cat(mtry," ")
}
matplot(1:mtry,cbind(test.err,train.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("Train","Test"),pch=19,col=c("red","blue"))
title("Graph of Train and Test Mean Squared Errors")

#
# View the forest results.
print(rf.air) 
# Importance of each predictor.
print(importance(rf.air,type = 2)) 
#Test this randomF on completely diff df of 2011
hahi <- head(air13a)
hahi1 <- na.omit(subset(hahi,select = c("AnnualExtDose","unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.landuse")))
hahi1$AnnualExtDose
# 0.21024 0.21024 0.21024 0.21024 0.21024 0.21024
predict(rf.air, hahi1)
# 0.2388158 0.2289263 0.2366579 0.2311324 0.2240987 0.2200885
hahi1$unAnnualExtDose
#0.1993678 0.1993678 0.1993678 0.1993678 0.1594942 0.1594942

#Train the rf model in areas that saw a rise in Radiations
in.negativ1 <- na.omit(subset(in.negativ,select = c("AnnualExtDose","unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.landuse")))

train.err=double(5)
test.err=double(5)
for(mtry in 1:5){
        fit=randomForest(AnnualExtDose~.,data=train13,mtry=mtry,ntree=300)
        train.err[mtry]=fit$mse[300]
        pred=predict(fit,in.negativ1)
        test.err[mtry]=with(in.negativ1,mean((AnnualExtDose-pred)^2))
        cat(mtry," ")
}
matplot(1:mtry,cbind(test.err,train.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("Train","Test"),pch=19,col=c("red","blue"))
title("Graph of Train and Test Mean Squared Errors")
#Visualize a single tree
library(rpart)
# grow tree 
air_tree <- tree(AnnualExtDose~.,air13b)
plot(air_tree)
text(air_tree, pretty = 0)
title("Annual External Air Dose Tree ")
# create attractive postcript plot of tree 
post(air_tree, file = "./tree2.ps", 
     title = "Annual External Air Dose Tree ")

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#BOSTING
library(gbm)
set.seed(1986)
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

boost.air=gbm(AnnualExtDose~.,data=train13,distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4)
summary(boost.air)
par(mfrow=c(2,2))
plot(boost.air,i="unAnnualExtDose")
plot(boost.air,i="MxAlt1Km")
plot(boost.air,i= "daichi.km")
plot(boost.air,i="mode.landuse")
plot(boost.air,i="mode.landuse")


n.trees=seq(from=100,to=10000,by=100)
predmat=predict(boost.air,newdata=test13,n.trees=n.trees)
dim(predmat)
berr=with(test13,apply( (predmat-AnnualExtDose)^2,2,mean))
par(mfrow=c())
plot(n.trees,berr,pch=19,ylab="Mean Squared Error", xlab="Number of Trees",main="Comparing Test Errors of Boosting and Random Forest")
abline(h=mean(test.err),col="red")


#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#xgboost
library("xgboost")
set.seed(1994)
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


dtrain <- xgb.DMatrix(data = train13$data, label=train13$AnnualExtDose)
dtest <- xgb.DMatrix(data = test13$data, label=test13$AnnualExtDose)

bstDense <- xgboost(data = as.matrix(train13$data), label = train13$AnnualExtDose, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")

#linear
bst <- xgb.train(data=dtrain, booster = "gblinear", max.depth=2, nthread = 2, nround=2, watchlist=watchlist, eval.metric = "error", eval.metric = "logloss", objective = "binary:logistic")



#------------------------------------------------------------------------------------------------------------------------
# AIR DOSE PER TOWN
#------------------------------------------------------------------------------------------------------------------------
### 21st Jan 2017
#Annalyse number of towns with the positive airdose reduction
jio <- air9[air9$AirDoseRedP > 0,]
cbind(dim(jio[jio$Year == "2015",]),dim(jio[jio$Year == "2014",]),dim(jio[jio$Year == "2013",]))
# #Annalyse square km with the negative airdose reduction
gio <- air13[!air13$AirDoseRedP > 0,]
cbind(dim(jio[gio$Year == "2015",]),dim(gio[gio$Year == "2014",]),dim(gio[gio$Year == "2013",]))
#save df made since above
write.csv(air9,file = "thesisVisuals/air9.csv",row.names = FALSE)

# Towns with Annual External Dose > 1mSv/year
air1m <- air13[air13$AnnualExtDose > 1,]
towns1m <- summarise(group_by(air1m,city,Year),kawt=n(), meanPerDecr = mean(AirDoseRedP))
towns1345 <- subset(towns1m, !meanPerDecr == 0)

j3 <- subset(towns1345, select=c("city","Year","meanPerDecr"))
j4 <- na.omit(dcast(j3, city~Year)) #df of city and meanPerDecr
write.csv(j4,file = "thesisVisuals/town.meanR.csv",row.names = FALSE)

##
#Population
#
airpop <- air13[!air13$AirDoseRedP == 0,]
airpop1 <- na.omit(subset(airpop, select = c("Year","AirDoseRedP","totalpop")))
ggplot(airpop1) + 
        geom_point(aes(AirDoseRedP,totalpop))+
        geom_vline(xintercept = 0, colour="green", linetype = "longdash")+
        labs(list(title = "Population Density against Percentage Change of Annual External Air Doses", 
                  x = "Percentage Change of Annual External Dose (mSv/year)", y = expression ("Population density"~(persons/km^2))))+
        facet_wrap(~Year)

##
#Land use
#
airl <- air1m[!air1m$AirDoseRedP == 0,]
ggplot(airl) + 
        geom_point(aes(AirDoseRedP,mode.landuse))+
        geom_vline(xintercept = 0, colour="green", linetype = "longdash")+
        labs(list(title = "Land usage against Percentage Change of Annual External Air Doses (2013 to 2015)", 
                  x = "Percentage Change of Annual External Dose (mSv/y)", y = expression ("Land usage per"~km^2)))
   

# PREDICT FUTURE DOSES, 2021, 2041, 2071,2101
air.future <- air13[air13$Year == "2012" | air13$Year == "2013" | air13$Year == "2014" | air13$Year == "2015",]
air.future$date <- 0
#project dates
air.future$date[air.future$Year == "2012"] <- "2021-04-12"
air.future$date[air.future$Year == "2013"] <- "2041-04-12"
air.future$date[air.future$Year == "2014"] <- "2071-04-12"
air.future$date[air.future$Year == "2015"] <- "2101-04-12"
#project years
air.future$Year[air.future$Year == "2012"] <- "2021"
air.future$Year[air.future$Year == "2013"] <- "2041"
air.future$Year[air.future$Year == "2014"] <- "2071"
air.future$Year[air.future$Year == "2015"] <- "2101"
#nullify variables
air.future$AvgAirDose <- 0
air.future$unAnnualExtDose <- NULL
air.future$unAnnualExDoseRange <- 0
air.future$no.days <- 0
air.future$AnnualExtDose <- 0
air.future$AnnualExDoseRange <- 0
air.future$AirDoseRedP <- 0
#re-add the variables
air.future$date <- as.Date(air.future$date)
air.future$no.days <- as.integer(difftime(as.POSIXct(air.future$date),as.POSIXct("2012-02-21"),units="days"))
#append the benchmarked annual ext dose of 2012
air.bench <- subset(air13, select = c("gride","Year","AnnualExtDose"))
air.bench12 <- na.omit(air.bench[air.bench$Year == "2012",])
#after 10 years
air.bench21 <- air.bench12
air.bench21$Year <- "2021"
#after 30years
air.bench41 <- air.bench12 
air.bench41$Year <- "2041"
#after 60years
air.bench71 <- air.bench12 
air.bench71$Year <- "2071"
#after 90years
air.bench101 <- air.bench12 
air.bench101$Year <- "2101"
#combine all the projected df
cbind(names(air.bench21),names(air.bench41),names(air.bench71),names(air.bench101))
air.proj <- do.call("rbind", list(air.bench21, air.bench41, air.bench71,air.bench101))
colnames(air.proj) <- c("gride","Year","unAnnualExtDose12")
#merge the projected years with static variables
air.future1 <- merge(air.future, air.proj, by.x = c("gride","Year"),by.y = c("gride","Year"),sort = FALSE)
air.future1$unAnnualExtDose <- air.future1$unAnnualExtDose12 * (0.69*exp(-0.336*air.future1$no.days/365) + 0.31*exp(-0.023*air.future1$no.days/365))
air.future2 <- subset(air.future1,select = c(!is.na(MxAlt1Km),!is.na(daichi.km),!is.na(mode.landuse),!is.na(mode.landuse)))

#predict the distribution of future doses
fit.future <- randomForest(AnnualExtDose~.,data=train13,mtry=3,ntree=300)

# get predictors of air.future2 df
air.futureb <- subset(air.future2,select = c("AnnualExtDose","unAnnualExtDose", "MxAlt1Km","daichi.km","mode.landuse","mode.landuse"))
air.future2$AnnualExtDose <- predict(fit.future, air.futureb)

#calculate map ranges
air.future2$AirDoseRedP <- ((air.future2$unAnnualExtDose - air.future2$AnnualExtDose)/(air.future2$unAnnualExtDose))*100
#previous annual ext dose due to background ( 0.04)*(8 + 16*0.4)*365/1000 = 0.21024
brks <- c(0.00,0.21,1.00,3.00,5.00)
air.future2$unAnnualExDoseRange = cut(air.future2$unAnnualExtDose, breaks = brks)
air.future2$AnnualExDoseRange = cut(air.future2$AnnualExtDose, breaks = brks)

#save file
write.csv(air.future2, file = "thesisVisuals/air.future.csv",row.names = FALSE)
#visualize
#decontaminated plot
q <- ggplot() +
        geom_point(data = air_2011tepco, aes(x=SW_eLong,y=SW_nLat),size=3,color="grey85")+
        geom_point(data = air.future2, aes(x = EastlngDec, y = NorthlatDec, color = AnnualExDoseRange,shape=15))+
        scale_shape_identity()+
        scale_color_brewer(palette="Blues")+
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
              plot.background=element_blank(),
              strip.text = element_text(size=18))
q + facet_wrap(~ Year)

#quantified
airArea.future <- air.future2 %>% 
        group_by(Year,AnnualExtDose) %>% 
        summarise(kawt=n()) %>% 
        mutate(untarea=kawt, AnnualExDoseRange = cut(AnnualExtDose, breaks = brks))
ggplot(airArea.future, aes(x = factor(Year), y = kawt, fill = factor(AnnualExDoseRange))) +
        geom_bar(stat="identity", width = 0.7) +
        labs(x = "Year", y = expression(paste("Land Area ", km^{2})),title="Decontaminated area based on current trend", fill = "External Dose/year") +
        theme_minimal(base_size = 14)+
        scale_fill_brewer(palette = "Blues")

#location maps
z <- ggplot() +
        geom_polygon(data=fu_f,aes(x = long, y = lat, group = group),fill="yellowgreen")+
        geom_point(data = air_2011tepco, aes(x=SW_eLong,y=SW_nLat),size=4,color="lightpink")+
        geom_point(data = air13, aes(x = EastlngDec, y = NorthlatDec),size=2,color="yellowgreen")+
        geom_polygon(data=fu_f,aes(x = long, y = lat, group = group),color="black",fill="NA")+
        coord_map()+
        annotate("text", x = 141.0328, y = 37.4211, label = "x",color="red", size=4)+
        labs(main = "Fukushima Prefecture")
z 

# Predicted vs Observed plots
# https://www.r-bloggers.com/part-4a-modelling-predicting-the-amount-of-rain/
library(tidyr)
all.predictions <- gather(all.predictions,key = model,value = predictions,2:6)
ggplot(data = all.predictions,aes(x = actual, y = predictions)) + 
        geom_point(colour = "blue") + 
        geom_abline(intercept = 0, slope = 1, colour = "red") +
        geom_vline(xintercept = 23, colour = "green", linetype = "dashed") +
        facet_wrap(~ model,ncol = 2) + 
        coord_cartesian(xlim = c(0,70),ylim = c(0,70)) +
        ggtitle("Predicted vs. Actual, by model")
