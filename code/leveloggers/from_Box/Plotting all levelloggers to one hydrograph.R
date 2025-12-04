### Read in Precipitation data
#Make date into date format for NOAA 2020 and NOAA 2021 data
setwd("C:/Users/rickard/Documents/NCOS Hydrology/2021 merged levellogger")
library(ggplot2)
library(grid)
library(gridExtra)
NOAA<-read.csv("NOAA_2021wy_Formatted.csv")
tail(NOAA)
NOAA$Date<-as.Date(NOAA$Date,format="%m/%d/%Y")
NOAAdaily<-aggregate(Precip_mm~Date, NOAA, FUN=sum)
NOAAdaily$Precip_Inch<-NOAAdaily$Precip_mm*0.0393701
NOAAdaily$Date<-as.POSIXct(NOAAdaily$Date,format="%Y-%m-%d")
NOAAdaily<-NOAAdaily[NOAAdaily$Precip_mm>=0,]
tail(NOAAdaily)
###############################################################################
## Aggregate downloaded files to get daily average#############################
###############################################################################
Whittier2021wy<-read.csv("Whittier_Pond_levelLogger_2021.csv")
WhittierDaily<-aggregate(cbind(LEVEL,TEMPERATURE,WSE_ft)~Date,Whittier2021wy,FUN=mean)
WhittierDaily<-WhittierDaily[WhittierDaily$Date>="2020-10-01"& WhittierDaily$Date<="2021-10-01",]
WhittierDaily$Date<-as.POSIXct(WhittierDaily$Date,format="%Y-%m-%d")

Westarm_DevSlough2021wy<-read.csv("Westarm_DevCreek_levelLogger_2021.csv")
Westarm_DevSloughDaily<-aggregate(cbind(LEVEL,TEMPERATURE,WSE_ft)~Date,Westarm_DevSlough2021wy,FUN=mean)
Westarm_DevSloughDaily<-Westarm_DevSloughDaily[Westarm_DevSloughDaily$Date>="2020-10-01"& Westarm_DevSloughDaily$Date<="2021-10-01",]
Westarm_DevSloughDaily$Date<-as.POSIXct(Westarm_DevSloughDaily$Date,format="%Y-%m-%d")
tail(Westarm_DevSlough2021wy)

Phelps2021wy<-read.csv("Phelps_Bridge_levelLogger_2021.csv")
PhelpsDaily<-aggregate(cbind(LEVEL,TEMPERATURE,WSE_ft)~Date,Phelps2021wy,FUN=mean)
PhelpsDaily<-PhelpsDaily[PhelpsDaily$Date>="2020-10-01"& PhelpsDaily$Date<="2021-10-01",]
PhelpsDaily$Date<-as.POSIXct(PhelpsDaily$Date,format="%Y-%m-%d")

Devereux<-read.csv("Devereux_YSI2021wy_Master.csv")
Devereux<-Devereux[!is.na(Devereux$Date.x),]
Devereux$Date.x<-as.Date(Devereux$Date.x,format="%m/%d/%Y")
DevereuxDaily<-aggregate(Depth.ft~Date.x,Devereux,FUN=mean)
colnames(DevereuxDaily)<-c("Date","LEVEL")
DevereuxDaily$TEMPERATURE<-0
DevereuxDaily$WSE_ft<-DevereuxDaily$LEVEL*1.18
DevereuxDaily$Type<-"Devereux Slough"
DevereuxDaily$Date<-as.POSIXct(DevereuxDaily$Date,format="%Y-%m-%d")
tail(DevereuxDaily)
#Now making a hydrograph with all levelloggers and the water surface elevation
#each dataset needs to have a new column called "Type" with the location name 

PhelpsDaily$Type<-"Phelps Bridge"
Westarm_DevSloughDaily$Type<-"West Arm-Devereux Creek"
WhittierDaily$Type<-"Whittier Pond"
merDF  <- rbind(PhelpsDaily,Westarm_DevSloughDaily,DevereuxDaily)

#Plotting in Base r so rain is same plot
NOAAdaily
coeff=0.5
ggplot(merDF, aes(x=Date)) +
  geom_line( aes(y=WSE_ft,color=Type), size=2) +
  geom_bar( data=NOAAdaily, aes(y=Precip_Inch/coeff), stat="identity", size=.1, fill=temperatureColor, color="black",alpha=0.4) +
  scale_y_continuous( name = "Water Elevation (feet)",
                      sec.axis = sec_axis(~.*coeff, name="Precipitation (Inches)"))+ylab("Date")+
  ggtitle("Precipitation and Hydrology in NCOS Tributaries and Wetland- 2021 Water Year")+theme_bw()+ 
  theme(axis.title.y = element_text(size=15,margin = margin(t = 0, r = 10, b = 0, l = 20)), axis.title.y.right = element_text(size=15,margin = margin(t = 0, r = 0, b = 0, l = 20)),
        axis.title.x=element_text(size=15), axis.text.x=element_text(size=15),
        axis.text.y = element_text(size=15),title =element_text(size=20, face='bold'),
        strip.text.y = element_text(size = 20),
        legend.text = element_text(size=15),
        legend.position = c(0.9, 0.4))

