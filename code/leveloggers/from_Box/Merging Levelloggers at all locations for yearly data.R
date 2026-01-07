## Phelps 
#Download data
#Dates are in different forats so we need to change to date format individually
library(dplyr)
a<-read.csv("Phelps_Creek_MarymountBridge_20200917_1102_Compensated.csv",skip=11)
a$Date<-as.Date(a$Date, format="%Y-%m-%d")
b<-read.csv("Phelps_Creek_MarymountBridge_20201102_1214_Compensated.csv",skip=11)
b$Date<-as.Date(b$Date, format="%Y-%m-%d")
c<-read.csv("Phelps_Creek_MarymountBridge_20201214_1227_Compensated.csv",skip=11)
c$Date<-as.Date(c$Date, format="%Y-%m-%d")
d<-read.csv("Phelps_Creek_MarymountBridge_20201227_20210104_Compensated.csv",skip=11)
d$Date<-as.Date(d$Date, format="%Y-%m-%d")
e<-read.csv("Phelps_Creek_MarymountBridge_20210104_0125_Compensated.csv",skip=11)
e$Date<-as.Date(e$Date, format="%m/%d/%Y")
f<-read.csv("Phelps_Creek_MarymountBridge_20210125_0209_Compensated.csv",skip=11)
f$Date<-as.Date(f$Date, format="%m/%d/%Y")
g<-read.csv("Phelps_Creek_MarymountBridge_20210209_0304_Compensated.csv",skip=11)
g$Date<-as.Date(g$Date, format="%m/%d/%Y")
h<-read.csv("Phelps_Creek_MarymountBridge_20210304_0316_Compensated.csv",skip=11)
h$Date<-as.Date(h$Date, format="%m/%d/%Y")
i<-read.csv("PhelpsCreek_MarymountBridge_20210316_0512_Compensated_2.csv",skip=11)
i$Date<-as.Date(i$Date, format="%m/%d/%Y")
options(digits=8)
i$LEVEL<-as.numeric(i$LEVEL)
j<-read.csv("PhelpsCreek_MarymountBridge_20210512_0617_Compensated.csv",skip=11)
j$Date<-as.Date(j$Date, format="%m/%d/%Y")
k<-read.csv("PhelpsCreek_MarymountBridge_20210617_0818_Compensated.csv",skip=11)
k$Date<-as.Date(k$Date, format="%m/%d/%Y")

#Bind all data together to make one long data frame with all of the years dates
str(a)
str(b)
str(c)
str(d)
str(e)
str(f)
str(g)
str(h)
str(i)
str(j)
str(k)
Phelps<-bind_rows(a,b,c,d,e,f,g,h,i,j,k)
str(Phelps)
#Check if there are missing dates
wy2021 <- seq(as.Date("2020/10/1"),as.Date("2021/9/30"),"days") 
wy2021<-as.data.frame(wy2021)
colnames(wy2021)<-"Date"
str(wy2021)
Missingdays<-merge(wy2021,Phelps,by="Date",all.x=TRUE)
unique(Phelps$Date)
new_DF <- Missingdays[is.na(Missingdays$LEVEL),]
unique(new_DF$Date)
#2021-01-30 to 2021-05-11
## Make a ndf that has the elevation for all sites
df<-c("Devereux Slough","East Channel Bridge","Phelps Creek- Marymount Bridge","Veneco Bridge-North Side",
      "Devereux Creek","Western Seasonal Pond", "Whittier Pond","Whittier Stormdrain")
df<-as.data.frame(df)
df$Logger_Elevation_ft<-c(1.18,3.96,9.99,2.84,8.41,6.20,5.04,10.41)    
df
## Make a new column for WSE that accounts for elevation
Phelps$WSE_ft<-Phelps$LEVEL+9.99
Phelps

write.csv(Phelps,"C:/Users/rickard/Documents/NCOS Hydrology/Veneco Bridge Level logger 2021/Phelps_Bridge_levelLogger_2021.csv")

#####################################################################################
##################Devereux Slough####################################################
#####################################################################################
setwd("C:/Users/rickard/Documents/NCOS Hydrology/West arm Devereux Creek")
#Download data
#Dates are in different forats so we need to change to date format individually
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
a<-read.csv("Westarm_DevCreek_20200917_1102_Compensated.csv",skip=11)
a$Date<-as.Date(a$Date, format="%m/%d/%Y")
b<-read.csv("WestArm_DevCreek_20201102_1214_Compensated.csv",skip=11)
b$Date<-as.Date(b$Date, format="%Y-%m-%d")
c<-read.csv("WestArm_DevCreek_20201214_1227_Compensated.csv",skip=11)
c$Date<-as.Date(c$Date, format="%Y-%m-%d")
d<-read.csv("WestArm_DevCreek_20201227_20210104_Compensated.csv",skip=11)
d$Date<-as.Date(d$Date, format="%Y-%m-%d")
e<-read.csv("WestArm_DevereuxCreek_20210104_0125_Compensated.csv",skip=11)
e$Date<-as.Date(e$Date, format="%m/%d/%Y")
f<-read.csv("WestArm_DevCreek_20210125_0209_Compensated.csv",skip=11)
f$Date<-as.Date(f$Date, format="%Y-%m-%d")
g<-read.csv("WestArm_DevCreek_20210209_0304_Compensated.csv",skip=11)
g$Date<-as.Date(g$Date, format="%Y-%m-%d")
h<-read.csv("WestArm_DevCreek_20210304_0316_compensated.csv",skip=11)
h$Date<-as.Date(h$Date, format="%Y-%m-%d")
i<-read.csv("WestArm_DevereuxCreek_20210316_0617_Compensated.csv",skip=11)
i$Date<-as.Date(i$Date, format="%m/%d/%Y")
options(digits=8)
i$LEVEL<-as.numeric(i$LEVEL)
j<-read.csv("WestArm_DevereuxCreek_20210617_0818_Compensated.csv",skip=11)
j$Date<-as.Date(j$Date, format="%m/%d/%Y")


#Bind all data together to make one long data frame with all of the years dates
str(a)
str(b)
str(c)
str(d)
str(e)
str(f)
str(g)
str(h)
str(i)
str(j)
str(k)
DevSlough<-bind_rows(a,b,c,d,e,f,g,h,i,j)
str(DevSlough)

#Check if there are missing dates
wy2021 <- seq(as.Date("2020/10/1"),as.Date("2021/9/30"),"days") 
wy2021<-as.data.frame(wy2021)
colnames(wy2021)<-"Date"
Missingdays<-merge(wy2021,DevSlough,by="Date",all.x=TRUE)
unique(DevSlough$Date)
new_DF <- Missingdays[is.na(Missingdays$LEVEL),]
unique(new_DF$Date)

## Make a df that has the elevation for all sites
df<-c("Devereux Slough","East Channel Bridge","Phelps Creek- Marymount Bridge","Veneco Bridge-North Side",
      "Devereux Creek","Western Seasonal Pond", "Whittier Pond","Whittier Stormdrain")
df<-as.data.frame(df)
df$Logger_Elevation_ft<-c(1.18,3.96,9.99,2.84,8.41,6.20,5.04,10.41)    
df
## Make a new column for WSE that accounts for elevation
DevSlough$WSE_ft<-DevSlough$LEVEL+8.41
DevSlough

write.csv(DevSlough,"C:/Users/rickard/Documents/NCOS Hydrology/Veneco Bridge Level logger 2021/Westarm_levelLogger_2021.csv")
