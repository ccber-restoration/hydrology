#Download data
#Dates are in different forats so we need to change to date format individually
library(dplyr)
a<-read.csv("Venoco_Bridge_20200917_1102_Compensated.csv",skip=13)
a$Date<-as.Date(a$Date, format="%m/%d/%Y")
b<-read.csv("Venoco_Bridge_20201102_1214_Compensated.csv",skip=13)
b$Date<-as.Date(b$Date, format="%Y-%m-%d")
c<-read.csv("Venoco_Bridge_20201218_1227_Compensated.csv",skip=13)
c$Date<-as.Date(c$Date, format="%Y-%m-%d")
d<-read.csv("Venoco_Bridge_20201227_20210104_Compensated.csv",skip=13)
d$Date<-as.Date(d$Date, format="%Y-%m-%d")
e<-read.csv("Venoco_Bridge_20210104_0125_Compensated.csv",skip=13)
e$Date<-as.Date(e$Date, format="%m/%d/%Y")
f<-read.csv("Venoco_Bridge_20210125_0209_Compensated.csv",skip=13)
f$Date<-as.Date(f$Date, format="%Y-%m-%d")
g<-read.csv("Venoco_Bridge_20210209_0304_Compensated.csv",skip=13)
g$Date<-as.Date(g$Date, format="%Y-%m-%d")
h<-read.csv("VenocoBridge_20210304_0316_Compensated.csv",skip=13)
h$Date<-as.Date(h$Date, format="%Y-%m-%d")
#Missing 3/17-5/12
i<-read.csv("Venoco_Bridge_20210512_0617_Compensated.csv",skip=13)
i$Date<-as.Date(i$Date, format="%m/%d/%Y")
j<-read.csv("Venoco_Bridge_20210617_0818_Compensated.csv",skip=13)
j$Date<-as.Date(j$Date, format="%m/%d/%Y")
k<-read.csv("Venoco_Bridge_20201214_1218_Compensated.csv",skip=13)
k$Date<-as.Date(k$Date, format="%Y-%m-%d")


#Bind all data together to make one long data frame with all of the years dates
Master<-bind_rows(a,b,e,i,j)
str(Master)
Master2<-bind_rows(b,c,d,f,g,h,k)
Master$Date<-as.Date(Master$Date, format="%m/%d/%Y")
Mastermaster<-rbind(Master,Master2)

#Check if there are missing dates
wy2021 <- seq(as.Date("2020/10/1"),as.Date("2021/9/30"),"days") 
wy2021<-as.data.frame(wy2021)
colnames(wy2021)<-"Date"
Missingdays<-merge(wy2021,Mastermaster,by="Date",all.x=TRUE)
unique(Mastermaster$Date)
new_DF <- Missingdays[is.na(Missingdays$LEVEL),]
new_DF

## Make a ndf that has the elevation for all sites
df<-c("Devereux Slough","East Channel Bridge","Phelps Creek- Marymount Bridge","Veneco Bridge-North Side",
      "Devereux Creek","Western Seasonal Pond", "Whittier Pond","Whittier Stormdrain")
df<-as.data.frame(df)
df$Logger_Elevation_ft<-c(1.18,3.96,9.99,2.84,8.41,6.20,5.04,10.41)    
df
## Make a new column for WSE that accounts for elevation
Mastermaster$WSE_ft<-Mastermaster$LEVEL+2.84
Mastermaster

write.csv(Mastermaster,"C:/Users/rickard/Documents/NCOS Hydrology/Veneco Bridge Level logger 2021/Veneco_Bridge_levelLogger_2021.csv")
