setwd("F:/RREAS/MURRES")
library(plyr)

All_Obs_Data<-read.csv("RREAS_FI_SBAS_obs_private_for_Justin.csv", header=TRUE)
Sightings_Data<-read.csv("RREAS_Seabirds_Trackline.csv", header=TRUE)
setwd("F:/RREAS/SOSH")
SOSH<-All_Obs_Data[All_Obs_Data$species=="SOSH" & (All_Obs_Data$behavior==2 | All_Obs_Data$behavior==3),]
SOSH_Summed<-ddply(SOSH, .(gis_key), summarize, Total_SOSH=sum(count))

SOSH_Core<-merge(Sightings_Data[2:nrow(Sightings_Data),], SOSH_Summed, by="gis_key", all.x=TRUE)

#SPH_Core<-SPH_Core[SPH_Core$core==1,]
SOSH_Core<-SOSH_Core[SOSH_Core$latitude>=36 & SOSH_Core$latitude<=39,]
SOSH_Core$Total_SOSH[is.na(SOSH_Core$Total_SOSH)]<-0

###########making grid of SOSH observations to match with ROMS data#########
SOSH_Core$latitude<-as.numeric(SOSH_Core$latitude)
SOSH_Core$longitude<-as.numeric(SOSH_Core$longitude)

max(SOSH_Core$latitude)
min(SOSH_Core$latitude)
max(SOSH_Core$longitude)
min(SOSH_Core$longitude)
library(raster)
SOSH_Core$cutLat<-cut(SOSH_Core$latitude,breaks=seq(36,39, by=0.1))
SOSH_Core$cutLon<-cut(SOSH_Core$longitude,breaks=seq(-125,-121, by=0.1))
SOSH_Grid_Sum<-ddply(SOSH_Core, .(datedate, cutLat, cutLon), summarize, SOSH_Nu=sum(Total_SOSH))
SOSH_Grid_Sum$Lat<-unlist(strsplit(as.character(SOSH_Grid_Sum$cutLat),","))[c(T,F)]
SOSH_Grid_Sum$Lon<-unlist(strsplit(as.character(SOSH_Grid_Sum$cutLon),","))[c(T,F)]

SOSH_Grid_Sum$Lat<-as.numeric(sub("\\(","",SOSH_Grid_Sum$Lat))
SOSH_Grid_Sum$Lon<-as.numeric(sub("\\(","",SOSH_Grid_Sum$Lon))

summary(SOSH_Grid_Sum$SOSH_Nu[SOSH_Grid_Sum$SOSH_Nu>0])
quantile(SOSH_Grid_Sum$SOSH_Nu[SOSH_Grid_Sum$SOSH_Nu>0],.9)

SOSH_Grid_Sum$year<-as.numeric(substr(SOSH_Grid_Sum$datedate, nchar(SOSH_Grid_Sum$datedate)-3, nchar(SOSH_Grid_Sum$datedate)))

Obs_by_year<-ddply(SOSH_Grid_Sum, .(year), summarize, Sample_Total_SOSH=length(cutLat))

hist(Obs_by_year$Sample_Total_SOSH, breaks=seq(0,300, by=30))

png("Observations_by_Year_36_39.png", height=5, width=6, units="in", res=200)
plot(Obs_by_year$year, Obs_by_year$Sample_Total_SOSH,type="l", lty=1, lwd=3, xlab=c("Year"), ylab="Obs. Total_SOSH")
par(new=T)
plot(Obs_by_year$year, Obs_by_year$Sample_Total_SOSH,pch=19,cex=1.25, xlab=c("Year"), ylab="Obs. Total_SOSH")
dev.off()

SOSH_Grid_Sum$Unq_Loc<-paste(SOSH_Grid_Sum$cutLon,SOSH_Grid_Sum$cutLat )
Obs_by_Space<-ddply(SOSH_Grid_Sum, .(Unq_Loc), summarize, Sample_Total_SOSH=length(year))

hist(Obs_by_Space$Sample_Total_SOSH)

SOSH_Space_Occurrence<-merge(SOSH_Grid_Sum, Obs_by_Space, by="Unq_Loc")

SOSH_Space_Occurrence_Comm<-SOSH_Space_Occurrence[SOSH_Space_Occurrence$Sample_Total_SOSH>=5,]
Obs_by_year<-ddply(SOSH_Space_Occurrence_Comm, .(year), summarize, Sample_Total_SOSH=length(cutLat))

hist(Obs_by_year$Sample_Total_SOSH, breaks=seq(0,300, by=30))

png("Observations_by_Year_Red.png", height=5, width=6, units="in", res=200)
plot(Obs_by_year$year, Obs_by_year$Sample_Total_SOSH,type="l", lty=1, lwd=3, xlab=c("Year"), ylab="Obs. Total_SOSH", main="Reduced")
par(new=T)
plot(Obs_by_year$year, Obs_by_year$Sample_Total_SOSH,pch=19,cex=1.25, xlab=c("Year"), ylab="Obs. Total_SOSH")
dev.off()



SOSH_ROMS_Raster<-raster(xmn=-125, xmx=-121, ymn=36, ymx=39, res=c(0.1,0.1))




library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(viridis)
library(ggplot2)
library(raster)
library(ggplot2)
library(plyr)
setwd("F:/RREAS/Predictors/Static")
Depth1<-raster("z_1.grd")
Depth<-as.data.frame(Depth1,xy=TRUE)
colnames(Depth)<-c("Longitude","Latitude", "Depth")
#read in the Total_SOSHry coastline you are interested in 
library(ggnewscale)

###########figure 1###################
setwd("F:/RREAS/SOSH")
world<-ne_countries(scale="medium", returnclass = "sf")
png("SOSH_Grid_5_Obs.png", width=6, height=8, units="in", res=300)
ggplot(data=world) + geom_raster(data = Depth,aes(x=Longitude, y = Latitude, fill=Depth),linejoin = "mitre") + scale_fill_gradient(low = "cornflowerblue", high = "white",limits=c(-5000,0))+new_scale_fill()+
  coord_fixed(ratio = 1)+geom_raster(data=SOSH_Space_Occurrence_Comm, aes(x=Lon, y=Lat), fill="red")+
  theme_bw()+geom_segment(aes(x = -124.7, y = 36, xend = -124.7, yend = 39), size=1.2,linetype=4)+geom_segment(aes(x = -124.7, y = 36, xend = -120, yend = 36), size=1.2,linetype=4)+geom_segment(aes(x = -124.7, y = 39, xend = -120, yend = 39), size=1.2,linetype=4)+
   labs(fill = "Depth (m)")+geom_sf()+coord_sf(xlim=c(-125.5,-116.5), ylim=c(34, 42))+
  theme(legend.title=element_text(size=16),legend.text=element_text(size=14),legend.direction = "vertical", legend.box = "vertical")+ggtitle("5 obs. Threshold")
dev.off()


SOSH_Space_Occurrence_Comm<-SOSH_Space_Occurrence[SOSH_Space_Occurrence$Sample_Total_SOSH>=5,]
SOSH_Space_Occurrence_Comm$Agg[SOSH_Space_Occurrence_Comm$SOSH_Nu>=48]<-1
SOSH_Space_Occurrence_Comm$Agg[SOSH_Space_Occurrence_Comm$SOSH_Nu<48]<-0

SOSH_Spatial_Mean<-ddply(SOSH_Space_Occurrence_Comm,.(Lat, Lon), summarize, Mean_Total_SOSH=mean(SOSH_Nu), Mean_Agg=mean(Agg))

setwd("F:/RREAS/SOSH")
world<-ne_countries(scale="medium", returnclass = "sf")
png("SOSH_Total_SOSH_Grid.png", width=6, height=8, units="in", res=300)
ggplot(data=world) + geom_raster(data = Depth,aes(x=Longitude, y = Latitude, fill=Depth),linejoin = "mitre") + scale_fill_gradient(low = "cornflowerblue", high = "white",limits=c(-5000,0))+new_scale_fill()+
  coord_fixed(ratio = 1)+geom_raster(data=SOSH_Spatial_Mean, aes(x=Lon, y=Lat, fill=Mean_Total_SOSH))+ scale_fill_gradientn(colours=c("#0000FFFF","#F1ECE4","#FF0000FF"))+
  theme_bw()+geom_segment(aes(x = -124.7, y = 36, xend = -124.7, yend = 39), size=1.2,linetype=4)+geom_segment(aes(x = -124.7, y = 36, xend = -120, yend = 36), size=1.2,linetype=4)+geom_segment(aes(x = -124.7, y = 39, xend = -120, yend = 39), size=1.2,linetype=4)+
  labs(fill = "Mean SOSH Total_SOSH")+geom_sf()+coord_sf(xlim=c(-125.5,-116.5), ylim=c(34, 42))+
  theme(legend.title=element_text(size=16),legend.text=element_text(size=14),legend.direction = "vertical", legend.box = "vertical")+ggtitle("5 obs. Threshold")
dev.off()


png("SOSH_Agg_Grid.png", width=6, height=8, units="in", res=300)
ggplot(data=world) + geom_raster(data = Depth,aes(x=Longitude, y = Latitude, fill=Depth),linejoin = "mitre") + scale_fill_gradient(low = "cornflowerblue", high = "white",limits=c(-5000,0))+new_scale_fill()+
  coord_fixed(ratio = 1)+geom_raster(data=SOSH_Spatial_Mean, aes(x=Lon, y=Lat, fill=Mean_Agg))+ scale_fill_gradientn(colours=c("#0000FFFF","#F1ECE4","#FF0000FF"))+
  theme_bw()+geom_segment(aes(x = -124.7, y = 36, xend = -124.7, yend = 39), size=1.2,linetype=4)+geom_segment(aes(x = -124.7, y = 36, xend = -120, yend = 36), size=1.2,linetype=4)+geom_segment(aes(x = -124.7, y = 39, xend = -120, yend = 39), size=1.2,linetype=4)+
  labs(fill = "Mean Freq. Agg")+geom_sf()+coord_sf(xlim=c(-125.5,-116.5), ylim=c(34, 42))+
  theme(legend.title=element_text(size=16),legend.text=element_text(size=14),legend.direction = "vertical", legend.box = "vertical")+ggtitle("5 obs. Threshold")
dev.off()

#prep the SOSH data for downstream data matching#

SOSH_ROMS_Data<-SOSH_Space_Occurrence_Comm[SOSH_Space_Occurrence_Comm$year>=1998 &SOSH_Space_Occurrence_Comm$year!=2000, ]
SOSH_ROMS_Data$Date<-as.Date(SOSH_ROMS_Data$datedate,
                         format = "%m/%d/%Y")
library(dplyr)
library(lubridate)
library(plyr)
SOSH_ROMS_Data=SOSH_ROMS_Data %>%
  dplyr::mutate(Year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date),
                DOY=lubridate::yday(Date))



########match ssh##############
detach(package:tidyr,unload=TRUE)
setwd('F:/RREAS/Predictors/ROMS/SSH')
library(ncdf4)
SSH_ROMs_2010=nc_open('wcra31_ssh_daily_1980_2010.nc')
SSH_Daily_2010<-ncvar_get(SSH_ROMs_2010, "ssh")
Lat<-ncvar_get(SSH_ROMs_2010, "lat")
Lon<-ncvar_get(SSH_ROMs_2010, "lon")
Year_2010<-ncvar_get(SSH_ROMs_2010, "year")
Month_2010<-ncvar_get(SSH_ROMs_2010, "month")
Day_2010<-ncvar_get(SSH_ROMs_2010, "day")


SSH_ROMs_2017=nc_open('wcnrt_ssh_daily_20110102_20170419.nc')

library(abind)
SSH_Daily_2017<-ncvar_get(SSH_ROMs_2017, "ssh")
Year_2017<-ncvar_get(SSH_ROMs_2017, "year")
Month_2017<-ncvar_get(SSH_ROMs_2017, "month")
Day_2017<-ncvar_get(SSH_ROMs_2017, "day")

SSH_ROMs_2018=nc_open('wcnrt_ssh_daily_20170420_20180731.nc')

library(abind)
SSH_Daily_2018<-ncvar_get(SSH_ROMs_2018, "ssh")
Year_2018<-ncvar_get(SSH_ROMs_2018, "year")
Month_2018<-ncvar_get(SSH_ROMs_2018, "month")
Day_2018<-ncvar_get(SSH_ROMs_2018, "day")

SSH_ROMs_2019=nc_open('wcnrt_ssh_daily_20180801_20190815.nc')

library(abind)
SSH_Daily_2019<-ncvar_get(SSH_ROMs_2019, "ssh")
Year_2019<-ncvar_get(SSH_ROMs_2019, "year")
Month_2019<-ncvar_get(SSH_ROMs_2019, "month")
Day_2019<-ncvar_get(SSH_ROMs_2019, "day")

Month<-c(Month_2010, Month_2017, Month_2018, Month_2019)
Year<-c(Year_2010, Year_2017, Year_2018, Year_2019)
Day<-c(Day_2010, Day_2017, Day_2018, Day_2019)

SSH_Daily<-abind(SSH_Daily_2010, SSH_Daily_2017, SSH_Daily_2018, SSH_Daily_2019)

SOSH_ROMS_Data$Unique_Date<-paste(SOSH_ROMS_Data$year, SOSH_ROMS_Data$month, SOSH_ROMS_Data$day)

Station_Coords<-as.matrix(SOSH_ROMS_Data[,c(7,6)])
Station_Coords<-as.data.frame(Station_Coords)
SSH_Val<-matrix(, nrow = nrow(SOSH_ROMS_Data), ncol = 2)

Unique_Date<-paste(Year, Month, Day)

library(sp)
pts<-SpatialPoints(Station_Coords, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
pts@coords[, c(1,2)]
for (i in 1: nrow(SOSH_ROMS_Data)){
  if (SOSH_ROMS_Data$year[i]<=2019){
    J<-which(Unique_Date==SOSH_ROMS_Data$Unique_Date[i])
    SSH_Ex<-t(SSH_Daily[,,J])
    SSH_Ex<- SSH_Ex[ nrow(SSH_Ex):1, ]
    SSH_Rast<-raster(
      SSH_Ex,
      xmn=min(Lon), xmx=max(Lon),
      ymn=min(Lat), ymx=max(Lat), 
      crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
    )
    Pts_Adj<-spTransform(pts[i,],crs(SSH_Rast))
    SSH_SD_Rast<-focal(SSH_Rast,w=matrix(1,nrow=3,ncol = 3),fun=sd,na.rm=T)
    SSH_Val[i,1]<-extract(SSH_Rast, Pts_Adj)
    SSH_Val[i,2]<-extract(SSH_SD_Rast, Pts_Adj)}
  else if (SOSH_ROMS_Data$year[i]==2021){
    SSH_Rast<-raster(paste("F:/RREAS/Predictors/ROMS_Output/ROMS_Output/", SOSH_ROMS_Data$Date[i],"/ssh.grd",sep=""))
    Pts_Adj<-spTransform(pts[i,],crs(SSH_Rast))
    SSH_SD_Rast<-focal(SSH_Rast,w=matrix(1,nrow=3,ncol = 3),fun=sd,na.rm=T)
    SSH_Val[i,1]<-extract(SSH_Rast, Pts_Adj)
    SSH_Val[i,2]<-extract(SSH_SD_Rast, Pts_Adj)}
}


rm(SSH_Daily, SSH_Daily_2010, SSH_Daily_2017, SSH_Daily_2018, SSH_Daily_2019)

#########match SST##################

setwd('F:/RREAS/Predictors/ROMS/SST')
library(ncdf4)
SST_ROMs_2010=nc_open('wcra31_sst_daily_1980_2010.nc')
SST_Daily_2010<-ncvar_get(SST_ROMs_2010, "sst")
Lat<-ncvar_get(SST_ROMs_2010, "lat")
Lon<-ncvar_get(SST_ROMs_2010, "lon")
Year_2010<-ncvar_get(SST_ROMs_2010, "year")
Month_2010<-ncvar_get(SST_ROMs_2010, "month")
Day_2010<-ncvar_get(SST_ROMs_2010, "day")


SST_ROMs_2017=nc_open('wcnrt_sst_daily_20110102_20170419.nc')

library(abind)
SST_Daily_2017<-ncvar_get(SST_ROMs_2017, "sst")
Year_2017<-ncvar_get(SST_ROMs_2017, "year")
Month_2017<-ncvar_get(SST_ROMs_2017, "month")
Day_2017<-ncvar_get(SST_ROMs_2017, "day")

SST_ROMs_2018=nc_open('wcnrt_sst_daily_20170420_20180731.nc')

library(abind)
SST_Daily_2018<-ncvar_get(SST_ROMs_2018, "sst")
Year_2018<-ncvar_get(SST_ROMs_2018, "year")
Month_2018<-ncvar_get(SST_ROMs_2018, "month")
Day_2018<-ncvar_get(SST_ROMs_2018, "day")

SST_ROMs_2019=nc_open('wcnrt_sst_daily_20180801_20190815.nc')

library(abind)
SST_Daily_2019<-ncvar_get(SST_ROMs_2019, "sst")
Year_2019<-ncvar_get(SST_ROMs_2019, "year")
Month_2019<-ncvar_get(SST_ROMs_2019, "month")
Day_2019<-ncvar_get(SST_ROMs_2019, "day")


Month<-c(Month_2010, Month_2017, Month_2018, Month_2019)
Year<-c(Year_2010, Year_2017, Year_2018, Year_2019)
Day<-c(Day_2010, Day_2017, Day_2018, Day_2019)

SST_Daily<-abind(SST_Daily_2010, SST_Daily_2017, SST_Daily_2018, SST_Daily_2019)
SST_Val<-matrix(, nrow = nrow(SOSH_ROMS_Data), ncol = 3)

Unique_Date<-paste(Year, Month, Day)

library(sp)
pts<-SpatialPoints(Station_Coords, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
pts@coords[, c(1,2)]
for (i in 1:nrow(SOSH_ROMS_Data) ){
  if (SOSH_ROMS_Data$year[i]<=2019){
    J<-which(Unique_Date==SOSH_ROMS_Data$Unique_Date[i])
    SST_Ex<-t(SST_Daily[,,J])
    SST_Ex<- SST_Ex[ nrow(SST_Ex):1, ]
    SST_Rast<-raster(
      SST_Ex,
      xmn=min(Lon), xmx=max(Lon),
      ymn=min(Lat), ymx=max(Lat), 
      crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
    )
    Pts_Adj<-spTransform(pts[i,],crs(SST_Rast))
    SST_SD_Rast<-aggregate(SST_Rast,fact= 2, fun=sd)
    SST_Val[i,1]<-extract(SST_Rast, Pts_Adj)
    SST_Val[i,2]<-extract(SST_SD_Rast, Pts_Adj)}
  else if (SOSH_ROMS_Data$year[i]==2021){
    SST_Rast<-raster(paste("F:/RREAS/Predictors/ROMS_Output/ROMS_Output/", SOSH_ROMS_Data$Date[i],"/sst.grd",sep=""))
    Pts_Adj<-spTransform(pts[i,],crs( SST_Rast))
    SST_SD_Rast<-focal( SST_Rast,w=matrix(1,nrow=3,ncol = 3),fun=sd,na.rm=T)
    SST_Val[i,1]<-extract( SST_Rast, Pts_Adj)
    SST_Val[i,2]<-extract( SST_SD_Rast, Pts_Adj)}
  SST_Val[i,3]<- (SST_Val[i,1]-cellStats(SST_Rast, "min"))/(cellStats(SST_Rast,"max")-cellStats(SST_Rast, "min"))
  
}

rm(SST_Daily, SST_Daily_2010, SST_Daily_2017, SST_Daily_2018, SST_Daily_2019)
##########wind stress curl#######################

######wind Curl######

setwd('F:/RREAS/Predictors/ROMS/Wind/Curl')
library(ncdf4)
Curl_ROMs_2010=nc_open('wcra31_curl_daily_1980_2010.nc')
Curl_Daily_2010<-ncvar_get(Curl_ROMs_2010, "curl")
Lat<-ncvar_get(Curl_ROMs_2010, "lat")
Lon<-ncvar_get(Curl_ROMs_2010, "lon")
Year_2010<-ncvar_get(Curl_ROMs_2010, "year")
Month_2010<-ncvar_get(Curl_ROMs_2010, "month")
Day_2010<-ncvar_get(Curl_ROMs_2010, "day")


Curl_ROMs_2017=nc_open('wcnrt_curl_daily_20110102_20170419.nc')

library(abind)
Curl_Daily_2017<-ncvar_get(Curl_ROMs_2017, "curl")
Year_2017<-ncvar_get(Curl_ROMs_2017, "year")
Month_2017<-ncvar_get(Curl_ROMs_2017, "month")
Day_2017<-ncvar_get(Curl_ROMs_2017, "day")

Curl_ROMs_2018=nc_open('wcnrt_curl_daily_20170420_20180731.nc')

library(abind)
Curl_Daily_2018<-ncvar_get(Curl_ROMs_2018, "curl")
Year_2018<-ncvar_get(Curl_ROMs_2018, "year")
Month_2018<-ncvar_get(Curl_ROMs_2018, "month")
Day_2018<-ncvar_get(Curl_ROMs_2018, "day")

Curl_ROMs_2019=nc_open('wcnrt_curl_daily_20180801_20190815.nc')

library(abind)
Curl_Daily_2019<-ncvar_get(Curl_ROMs_2019, "curl")
Year_2019<-ncvar_get(Curl_ROMs_2019, "year")
Month_2019<-ncvar_get(Curl_ROMs_2019, "month")
Day_2019<-ncvar_get(Curl_ROMs_2019, "day")

Month<-c(Month_2010, Month_2017, Month_2018, Month_2019)
Year<-c(Year_2010, Year_2017, Year_2018, Year_2019)
Day<-c(Day_2010, Day_2017, Day_2018, Day_2019)

Curl_Daily<-abind(Curl_Daily_2010, Curl_Daily_2017, Curl_Daily_2018, Curl_Daily_2019)
Curl_Val<-matrix(, nrow = nrow(SOSH_ROMS_Data), ncol = 1)

Unique_Date<-paste(Year, Month, Day)

library(sp)
pts<-SpatialPoints(Station_Coords, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
pts@coords[, c(1,2)]
for (i in 1: nrow(SOSH_ROMS_Data)){
  if (SOSH_ROMS_Data$year[i]<=2019){
    J<-which(Unique_Date==SOSH_ROMS_Data$Unique_Date[i])
    Curl_Ex<-t(Curl_Daily[,,J])
    Curl_Ex<- Curl_Ex[ nrow(Curl_Ex):1, ]
    Curl_Rast<-raster(
      Curl_Ex,
      xmn=min(Lon), xmx=max(Lon),
      ymn=min(Lat), ymx=max(Lat), 
      crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
    )
    Curl_Rast_1<-focal( Curl_Rast,w=matrix(1,nrow=5,ncol = 5),fun=mean,na.rm=T)
    Pts_Adj<-spTransform(pts[i,],crs(Curl_Rast_1))
    Curl_Val[i,1]<-extract(Curl_Rast_1, Pts_Adj)}
  else if (SOSH_ROMS_Data$year[i]==2021){
    Curl_Rast<-raster(paste("F:/RREAS/Predictors/ROMS_Output/ROMS_Output/", SOSH_ROMS_Data$Date[i],"/curl.grd",sep=""))
    Curl_Rast_1<-focal( Curl_Rast,w=matrix(1,nrow=5,ncol = 5),fun=mean,na.rm=T)
    Pts_Adj<-spTransform(pts[i,],crs( Curl_Rast_1))
    Curl_Val[i,1]<-extract( Curl_Rast_1, Pts_Adj)
  }}
plot(Curl_Rast_1)
rm(Curl_Daily, Curl_Daily_2010, Curl_Daily_2017, Curl_Daily_2018, Curl_Daily_2019)



#############U current###################
setwd('F:/RREAS/Predictors/ROMS/Currents')
library(ncdf4)
U_ROMs_2010=nc_open('wcra31_su_daily_1980_2010.nc')
U_Daily_2010<-ncvar_get(U_ROMs_2010, "su")
Lat<-ncvar_get(U_ROMs_2010, "lat")
Lon<-ncvar_get(U_ROMs_2010, "lon")
Year_2010<-ncvar_get(U_ROMs_2010, "year")
Month_2010<-ncvar_get(U_ROMs_2010, "month")
Day_2010<-ncvar_get(U_ROMs_2010, "day")


U_ROMs_2017=nc_open('wcnrt_su_daily_20110102_20170419.nc')

library(abind)
U_Daily_2017<-ncvar_get(U_ROMs_2017, "su")
Year_2017<-ncvar_get(U_ROMs_2017, "year")
Month_2017<-ncvar_get(U_ROMs_2017, "month")
Day_2017<-ncvar_get(U_ROMs_2017, "day")

U_ROMs_2018=nc_open('wcnrt_su_daily_20170420_20180731.nc')

library(abind)
U_Daily_2018<-ncvar_get(U_ROMs_2018, "su")
Year_2018<-ncvar_get(U_ROMs_2018, "year")
Month_2018<-ncvar_get(U_ROMs_2018, "month")
Day_2018<-ncvar_get(U_ROMs_2018, "day")

U_ROMs_2019=nc_open('wcnrt_su_daily_20180801_20190815.nc')

library(abind)
U_Daily_2019<-ncvar_get(U_ROMs_2019, "su")
Year_2019<-ncvar_get(U_ROMs_2019, "year")
Month_2019<-ncvar_get(U_ROMs_2019, "month")
Day_2019<-ncvar_get(U_ROMs_2019, "day")

Month<-c(Month_2010, Month_2017, Month_2018, Month_2019)
Year<-c(Year_2010, Year_2017, Year_2018, Year_2019)
Day<-c(Day_2010, Day_2017, Day_2018, Day_2019)

U_Daily<-abind(U_Daily_2010, U_Daily_2017, U_Daily_2018, U_Daily_2019)
U_Val<-matrix(, nrow = nrow(SOSH_ROMS_Data), ncol = 1)

Unique_Date<-paste(Year, Month, Day)

library(sp)
pts<-SpatialPoints(Station_Coords, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
pts@coords[, c(1,2)]
for (i in 1: nrow(SOSH_ROMS_Data)){
  if (SOSH_ROMS_Data$year[i]<=2019){
    J<-which(Unique_Date==SOSH_ROMS_Data$Unique_Date[i])
    U_Ex<-t(U_Daily[,,J])
    U_Ex<- U_Ex[ nrow(U_Ex):1, ]
    U_Rast<-raster(
      U_Ex,
      xmn=min(Lon), xmx=max(Lon),
      ymn=min(Lat), ymx=max(Lat), 
      crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
    )
    Pts_Adj<-spTransform(pts[i,],crs(U_Rast))
    U_Val[i,1]<-extract(U_Rast, Pts_Adj)}
  else if (SOSH_ROMS_Data$year[i]==2021){
    U_Rast<-raster(paste("F:/RREAS/Predictors/ROMS_Output/ROMS_Output/", SOSH_ROMS_Data$Date[i],"/su.grd",sep=""))
    Pts_Adj<-spTransform(pts[i,],crs(  U_Rast))
    U_Val[i,1]<-extract( U_Rast, Pts_Adj)
  }}  

plot(U_Rast)

rm(U_Daily, U_Daily_2010, U_Daily_2017, U_Daily_2018, U_Daily_2019)
#############V current###################
library(ncdf4)
V_ROMs_2010=nc_open('wcra31_sv_daily_1980_2010.nc')
V_Daily_2010<-ncvar_get(V_ROMs_2010, "sv")
Lat<-ncvar_get(V_ROMs_2010, "lat")
Lon<-ncvar_get(V_ROMs_2010, "lon")
Year_2010<-ncvar_get(V_ROMs_2010, "year")
Month_2010<-ncvar_get(V_ROMs_2010, "month")
Day_2010<-ncvar_get(V_ROMs_2010, "day")


V_ROMs_2017=nc_open('wcnrt_sv_daily_20110102_20170419.nc')

library(abind)
V_Daily_2017<-ncvar_get(V_ROMs_2017, "sv")
Year_2017<-ncvar_get(V_ROMs_2017, "year")
Month_2017<-ncvar_get(V_ROMs_2017, "month")
Day_2017<-ncvar_get(V_ROMs_2017, "day")

V_ROMs_2018=nc_open('wcnrt_sv_daily_20170420_20180731.nc')

library(abind)
V_Daily_2018<-ncvar_get(V_ROMs_2018, "sv")
Year_2018<-ncvar_get(V_ROMs_2018, "year")
Month_2018<-ncvar_get(V_ROMs_2018, "month")
Day_2018<-ncvar_get(V_ROMs_2018, "day")

V_ROMs_2019=nc_open('wcnrt_sv_daily_20180801_20190815.nc')

library(abind)
V_Daily_2019<-ncvar_get(V_ROMs_2019, "sv")
Year_2019<-ncvar_get(V_ROMs_2019, "year")
Month_2019<-ncvar_get(V_ROMs_2019, "month")
Day_2019<-ncvar_get(V_ROMs_2019, "day")



Month<-c(Month_2010, Month_2017, Month_2018, Month_2019)
Year<-c(Year_2010, Year_2017, Year_2018, Year_2019)
Day<-c(Day_2010, Day_2017, Day_2018, Day_2019)

V_Daily<-abind(V_Daily_2010, V_Daily_2017, V_Daily_2018, V_Daily_2019)
V_Val<-matrix(, nrow = nrow(SOSH_ROMS_Data), ncol = 1)

Unique_Date<-paste(Year, Month, Day)

library(sp)
pts<-SpatialPoints(Station_Coords, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
pts@coords[, c(1,2)]
for (i in 1: nrow(SOSH_ROMS_Data)){
  if (SOSH_ROMS_Data$year[i]<=2019){
    J<-which(Unique_Date==SOSH_ROMS_Data$Unique_Date[i])
    V_Ex<-t(V_Daily[,,J])
    V_Ex<- V_Ex[ nrow(V_Ex):1, ]
    V_Rast<-raster(
      V_Ex,
      xmn=min(Lon), xmx=max(Lon),
      ymn=min(Lat), ymx=max(Lat), 
      crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
    )
    Pts_Adj<-spTransform(pts[i,],crs(V_Rast))
    V_Val[i,1]<-extract(V_Rast, Pts_Adj)
  }
  else if (SOSH_ROMS_Data$year[i]==2021){
    #there are many missing v vals, so i am using this as an alternative
    V_Rast<-raster(paste("F:/RREAS/Predictors/ROMS_Output/ROMS_Output/", SOSH_ROMS_Data$Date[i],"/sv.grd",sep=""))
    Pts_Adj<-spTransform(pts[i,],crs(  V_Rast))
    V_Val[i,1]<-extract( V_Rast, Pts_Adj)
  }}  

plot(V_Rast)

rm(V_Daily, V_Daily_2010, V_Daily_2017, V_Daily_2018, V_Daily_2019)
############### static vars ###################
setwd("F:/RREAS/Predictors/Static")
Depth_Grid<-raster("z_1.grd")
Rugosity_Grid<-raster("z_sd.grd")
Distance_to_Shore_Grid<-raster("Distance_from_Shore.grd")
Distance_to_Land_Grid<-raster("Distance_from_Land.grd")
setwd("F:/RREAS/Predictors/Static")
library(raster)
bath<-raster("z_1.grd")



#Create raster with distances from each raster cell to point on coast
#takes as input a template raster with the resolution and extent that you want your distance raster to be in
dists = distanceFromPoints(bath, coastpts)
plot(dists) #check it works
Distance_from_Colony<-dists



pts<-spTransform(pts,crs(Depth_Grid))


Depth_Val<-extract(Depth_Grid, pts)
Rugosity_Val<-extract(Rugosity_Grid, pts)
Dist_Shore_Val<-extract(Distance_to_Shore_Grid, pts)
Dist_Land_Val<-extract(Distance_to_Land_Grid, pts)
Dist_Colony<-extract(Distance_from_Colony,pts)
##########chlorophyll#########
SOSH_ROMS_Data$Date_2<-gsub("-", "",substr(SOSH_ROMS_Data$Date,1,10))

setwd("F:/RREAS/Predictors/CHL/8_day")

CHL_Files1<-list.files(pattern='*GSM-SWF_CHL1_8D_00.nc')
CHL_Files2<-list.files(pattern='*GSM-MERSWF_CHL1_8D_00.nc')
CHL_Files3<-list.files(pattern='*GSM-MERMODSWF_CHL1_8D_00.nc')
CHL_Files4<-list.files(pattern='*GSM-MERMOD_CHL1_8D_00.nc')
CHL_Files5<-list.files(pattern='*GSM-MERMODVIR_CHL1_8D_00.nc')
CHL_Files6<-list.files(pattern='*GSM-MODVIR_CHL1_8D_00.nc')

CHL_Files<-as.data.frame(c(CHL_Files1, CHL_Files2, CHL_Files3,CHL_Files4, CHL_Files5, CHL_Files6))

colnames(CHL_Files)<-"FileName"
CHL_Files$Start<-as.numeric(substr(CHL_Files$FileName, 5,12))
CHL_Files$End<-as.numeric(substr(CHL_Files$FileName, 14,21))




CHL_Val<-matrix(, nrow = nrow(SOSH_ROMS_Data), ncol = 1)

pts<-SpatialPoints(Station_Coords, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
pts@coords[, c(1,2)]
for (i in 1: nrow(SOSH_ROMS_Data)){
  Day_of_Collection<-SOSH_ROMS_Data$Date_2[i]
  J<-which(CHL_Files$Start<=Day_of_Collection & CHL_Files$End>=Day_of_Collection)
  CHL<-nc_open(CHL_Files$FileName[J])
  CHL<-nc_open(CHL_Files$FileName[J])
  Lat<-ncvar_get(CHL, "lat")
  Lon<-ncvar_get(CHL, "lon")
  CHL<-ncvar_get(CHL, "CHL1_mean")
  CHL_Ex<-t(CHL)
  CHL_Rast<-raster(
    CHL_Ex,
    xmn=min(Lon), xmx=max(Lon),
    ymn=min(Lat), ymx=max(Lat), 
    crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
  )
  Pts_Adj<-spTransform(pts[i,],crs(CHL_Rast))
  CHL_Val[i,1]<-extract(CHL_Rast, Pts_Adj, method='bilinear')
}
plot(CHL_Rast)

RREAS_Pred1<-cbind(SOSH_ROMS_Data,CHL_Val)
RREAS_Pred_NA<-RREAS_Pred1[is.na(RREAS_Pred1$CHL_Val),]


setwd("F:/RREAS/Predictors/CHL/Monthly")

CHL_Files1<-list.files(pattern='*GSM-SWF_CHL1_MO_00.nc')
CHL_Files2<-list.files(pattern='*GSM-MERSWF_CHL1_MO_00.nc')
CHL_Files3<-list.files(pattern='*GSM-MERMODSWF_CHL1_MO_00.nc')
CHL_Files4<-list.files(pattern='*GSM-MERMOD_CHL1_MO_00.nc')
CHL_Files5<-list.files(pattern='*GSM-MERMODVIR_CHL1_MO_00.nc')
CHL_Files6<-list.files(pattern='*GSM-MODVIR_CHL1_MO_00.nc')

CHL_Files<-as.data.frame(c(CHL_Files1, CHL_Files2, CHL_Files3,CHL_Files4, CHL_Files5, CHL_Files6))

colnames(CHL_Files)<-"FileName"
CHL_Files$Start<-as.numeric(substr(CHL_Files$FileName, 5,12))
CHL_Files$End<-as.numeric(substr(CHL_Files$FileName, 14,21))




Station_Coords<-as.matrix(RREAS_Pred_NA[,c(7,6)])
Station_Coords<-as.data.frame(Station_Coords)
CHL_Val_2<-matrix(, nrow = nrow(RREAS_Pred_NA), ncol = 1)

pts<-SpatialPoints(Station_Coords, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
pts@coords[, c(1,2)]
for (i in 1: nrow(RREAS_Pred_NA)){
  Day_of_Collection<-RREAS_Pred_NA$Date_2[i]
  J<-which(CHL_Files$Start<=Day_of_Collection & CHL_Files$End>=Day_of_Collection)
  CHL<-nc_open(CHL_Files$FileName[J])
  CHL<-nc_open(CHL_Files$FileName[J])
  Lat<-ncvar_get(CHL, "lat")
  Lon<-ncvar_get(CHL, "lon")
  CHL<-ncvar_get(CHL, "CHL1_mean")
  CHL_Ex<-t(CHL)
  CHL_Rast<-raster(
    CHL_Ex,
    xmn=min(Lon), xmx=max(Lon),
    ymn=min(Lat), ymx=max(Lat), 
    crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
  )
  Pts_Adj<-spTransform(pts[i,],crs(CHL_Rast))
  CHL_Val_2[i,1]<-extract(CHL_Rast, Pts_Adj, method='bilinear')
}

RREAS_Pred_Monthly<-cbind(RREAS_Pred_NA,CHL_Val_2)

RREAS_Pred_Monthly$CHL_Val<-RREAS_Pred_Monthly$CHL_Val_2

RREAS_Pred_Monthly_Trim<-RREAS_Pred_Monthly[,1:18]
RREAS_Pred_no_NA<-RREAS_Pred1[!is.na(RREAS_Pred1$CHL_Val),]
RREAS_CHL_Pred<-rbind(RREAS_Pred_no_NA, RREAS_Pred_Monthly_Trim)
colnames(SST_Val)<-c("SST","SST_sd","SST_Anom")
colnames(SSH_Val)<-c("SSH","SSH_sd")


SOSH_Pred_2021<-cbind(SOSH_ROMS_Data,SSH_Val,SST_Val,Curl_Val, U_Val, V_Val, Depth_Val,Rugosity_Val,Dist_Shore_Val, Dist_Land_Val)

Full_SOSH_Predictors_CHL_2021<-merge(SOSH_Pred_2021, RREAS_CHL_Pred, by=c("Lat","Lon","Date"))
Full_SOSH_Predictors<-Full_SOSH_Predictors_CHL_2021[,c(1:28,44)]

setwd("F:/RREAS/SOSH")
write.csv(Full_SOSH_Predictors, "Full_RREAS_Predictors_SOSH.csv")

##########look at collinear predictors##############
setwd("F:/RREAS/WEGU")
Full_SOSH_Predictors2<-read.csv("Full_RREAS_Predictors_WEGU.csv", header=TRUE)
Full_SOSH_Predictors2$Lat_Deg<-round(Full_SOSH_Predictors2$Lat)
setwd("F:/RREAS/Predictors/CUTI_BEUTI")
CUTI_DF<-read.csv("erdCUTIdaily_97ea_5144_c2e1.csv", header=TRUE)
CUTI_DF$Date<-as.Date(CUTI_DF$time..UTC.)
colnames(CUTI_DF)<-c("Time","Lat_Deg","CUTI","Date")
Full_SOSH_Predictors2$Date<-as.Date(Full_SOSH_Predictors2$Date)
Full_SOSH_Predictors3<-merge(Full_SOSH_Predictors2, CUTI_DF, by=c("Date","Lat_Deg"))
setwd("F:/RREAS/Predictors/Static")
Station_Coords<-as.matrix(Full_SOSH_Predictors3[,colnames(Full_SOSH_Predictors3) %in% c("Lon","Lat")])
Station_Coords<-rev(as.data.frame(Station_Coords))

library(sp)
pts<-SpatialPoints(Station_Coords, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
pts@coords[, c(1,2)]

library(raster)
bath<-raster("z_1.grd")
setwd("F:/RREAS/COMU")
coastpts <- read.csv('COMU_Colony_Locations.csv', head=T)
coordinates(coastpts) <- ~Lon + Lat
proj4string(coastpts)<-CRS("+proj=longlat +datum=WGS84")

#Create raster with distances from each raster cell to point on coast
#takes as input a template raster with the resolution and extent that you want your distance raster to be in
dists = distanceFromPoints(bath, coastpts)
plot(dists) #check it works
Distance_from_Colony<-dists
Dist_Colony<-extract(Distance_from_Colony,pts)

Full_SOSH_Predictors4<-cbind(Full_SOSH_Predictors3, Dist_Colony)

Full_SOSH_Predictors1<-merge(SOSH_ROMS_Data, Full_SOSH_Predictors4, by=c("Lon","Lat","Date"))
Full_SOSH_Predictors<-Full_SOSH_Predictors1[-c(17:31,44,46)]
setwd("F:/RREAS/SOSH")

#SOSH_Population<-read.csv("SOSH_Pop_Estimate.csv", header=TRUE)
Full_SOSH_Predictors$TKE<-log(0.5*((Full_SOSH_Predictors$U_Val^2+Full_SOSH_Predictors$V_Val^2)^0.5))
SOSH_Predictors<-Full_SOSH_Predictors[,17:30]

setwd("F:/RREAS/SOSH")
library(PerformanceAnalytics)
png("SOSH_Predictors_220715.png", height=10, width=12,units="in", res=400 )
chart.Correlation(SOSH_Predictors)
dev.off()

SOSH_For_Model<-Full_SOSH_Predictors[,c(1,2,3,8,9,17:22,25:30)]
names(SOSH_For_Model)[4]<-"SOSH_Count"
names(SOSH_For_Model)[5]<-"year"

SOSH_For_Model$PA[SOSH_For_Model$SOSH_Count>0]<-1
SOSH_For_Model$PA[SOSH_For_Model$SOSH_Count==0]<-0
quantile(SOSH_For_Model$SOSH_Count[SOSH_For_Model$SOSH_Count>0],.9)
SOSH_For_Model$Agg[SOSH_For_Model$SOSH_Count>=192]<-1
SOSH_For_Model$Agg[SOSH_For_Model$SOSH_Count<192]<-0

quantile(SOSH_For_Model$SOSH_Count[SOSH_For_Model$SOSH_Count>0],.9)

# 90 % is 192 birds

SOSH_For_Model_Trimmed<-SOSH_For_Model
SOSH_For_Model_Pres<-SOSH_For_Model_Trimmed[SOSH_For_Model_Trimmed$PA==1,]
library(dismo)
library(gbm)
###############PA BRT##################

BRT_SOSH_Estimate<-fit.brt.n50_eval_Fixed(SOSH_For_Model_Trimmed, gbm.x = 6:17, gbm.y = 18,lr=0.005, tc=3,50)
save(BRT_SOSH_Estimate, file="BRT_PA_SOSH_50_Iterations_220803_Fixed.RData")

SOSH_For_Model_Pres<-SOSH_For_Model_Trimmed[SOSH_For_Model_Trimmed$PA==1,]


#be sure to make this not fixed when possible
BRT_SOSH_Agg_Estimate<-fit.brt.n50_eval_Agg_Fixed(SOSH_For_Model_Pres, gbm.x = 6:17, gbm.y = 19,lr=0.005, tc=3,50)

save(BRT_SOSH_Agg_Estimate, file="BRT_Agg_SOSH_50_Iterations_220803_Fixed.RData")

Model_PA_Eval_SOSH<-matrix(,50,2)
Model_Evals_SOSH<-unlist(unlist(BRT_SOSH_Estimate[[2]]))

for (i in 1:50){
  Model_PA_Eval_SOSH[i,1]<-Model_Evals_SOSH[[i]]@auc
  Model_PA_Eval_SOSH[i,2]<-max(Model_Evals_SOSH[[i]]@TPR+Model_Evals_SOSH[[i]]@TNR-1)
}

mean( Model_PA_Eval_SOSH[,1])
mean( Model_PA_Eval_SOSH[,2])
summary( Model_PA_Eval_SOSH[,1])
summary( Model_PA_Eval_SOSH[,2])
Model_Agg_Eval_SOSH<-matrix(,50,2)
Model_EvalsAgg_SOSH<-unlist(unlist(BRT_SOSH_Agg_Estimate[[2]]))
for (i in 1:50){
  Model_Agg_Eval_SOSH[i,1]<-Model_EvalsAgg_SOSH[[i]]@auc
  Model_Agg_Eval_SOSH[i,2]<-max(Model_EvalsAgg_SOSH[[i]]@TPR+Model_EvalsAgg_SOSH[[i]]@TNR-1)}
mean( Model_Agg_Eval_SOSH[,1])
mean( Model_Agg_Eval_SOSH[,2])
summary( Model_Agg_Eval_SOSH[,1])
summary( Model_Agg_Eval_SOSH[,2])
#save.image("F:/RREAS/SOSH/SOSH_BRT_Models_GBM_Fixed_220803.RData")
load("F:/RREAS/SOSH/SOSH_BRT_Models_GBM_Fixed_220803.RData")

library(lubridate)
#need to remove years without aggregations for this to work
PA_LOYOCV_SOSH<-LOO_eval_heather_Fixed(SOSH_For_Model_Trimmed, gbm.x = 6:17, gbm.y = 18,lr=0.005, tc=3, family="bernoulli", response="PA")
save(PA_LOYOCV_SOSH, file="PA_LOYOCV_SOSH_220803_Fixed.RData")
Agg_LOYOCV_SOSH<-LOO_eval_heather_Fixed(SOSH_For_Model_Pres, gbm.x = 6:17, gbm.y = 19,lr=0.005, tc=3, family="bernoulli", response="Agg")
save(Agg_LOYOCV_SOSH, file="Agg_LOYOCV_SOSH_220803_Fixed.RData")
#load("F:/RREAS/SOSH/SOSH_BRT_Models.RData")
summary(PA_LOYOCV_SOSH$AUC)
summary(PA_LOYOCV_SOSH$TSS)

plot(PA_LOYOCV_SOSH$Year, PA_LOYOCV_SOSH$AUC)
plot(PA_LOYOCV_SOSH$Year, PA_LOYOCV_SOSH$TSS)

plot(Agg_LOYOCV_SOSH$Year, Agg_LOYOCV_SOSH$AUC)

plot(Agg_LOYOCV_SOSH$Year, Agg_LOYOCV_SOSH$TSS)

summary(Agg_LOYOCV_SOSH$AUC)
summary(Agg_LOYOCV_SOSH$TSS)
###########look at residuals

PA_Residuals_SOSH<-matrix(nrow=nrow(SOSH_For_Model_Trimmed), ncol=50)
Agg_Residuals_SOSH<-matrix(nrow=nrow(SOSH_For_Model_Trimmed), ncol=50)

BRT_PA_Models_SOSH<-BRT_SOSH_Estimate[[1]]
BRT_Agg_Models_SOSH<-BRT_SOSH_Agg_Estimate[[1]]

for (i in 1:50){
  Mod_PA_SOSH<-BRT_PA_Models_SOSH[i]
Mod_Agg_SOSH<-BRT_Agg_Models_SOSH[i]
    PA_Residuals_SOSH[,i]<-predict.gbm(Mod_PA_SOSH[[1]],SOSH_For_Model_Trimmed, n.trees=Mod_PA_SOSH[[1]]$gbm.call$best.trees, type="response")
    Agg_Residuals_SOSH[,i]<-predict.gbm(Mod_Agg_SOSH[[1]],SOSH_For_Model_Trimmed, n.trees=Mod_Agg_SOSH[[1]]$gbm.call$best.trees, type="response")
    }
View (PA_Residuals)
PA_Resid_SOSH_Means<-rowMeans(PA_Residuals_SOSH)
Agg_Resid_SOSH_Means<-rowMeans(Agg_Residuals_SOSH)

SOSH_For_Model_Trimmed$PA_Resids<-SOSH_For_Model_Trimmed$PA-PA_Resid_SOSH_Means
SOSH_For_Model_Trimmed$Agg_Resids_SOSH<-SOSH_For_Model_Trimmed$Agg-Agg_Resid_SOSH_Means
SOSH_For_Model_Trimmed$Hurdle_Resids<-SOSH_For_Model_Trimmed$Agg-(PA_Resid_SOSH_Means*Agg_Resid_SOSH_Means)

boxplot(PA_Resids~year, data=SOSH_For_Model_Trimmed)
boxplot(Agg_Resids_SOSH~year, data=SOSH_For_Model_Trimmed, ylim=c(-0.25,0.1))
boxplot(Hurdle_Resids~year, data=SOSH_For_Model_Trimmed, ylim=c(-0.2,0.2))



####look at partial plots for PA model####
var_tested<-c("SST","SST_Anom","Depth_Val", "Rugosity_Val","CHL_Val","SSH","SSH_sd","SST_sd","Dist_Shore_Val","TKE","Curl_Val","CUTI")
percent_contrib<-NULL#list()
iters=50
part_plot<-list()
part_plot<-list()
percent_contrib<-NULL#list()
for(q in 1:iters){                                #this was 50 
  mod<-BRT_SOSH_Estimate_PA_Models[q][[1]]  ###
  part_plot1<-data.frame(row.names=1:100)
  for(x in 1:length(var_tested)){ ###
    pp<-plot(mod ,var_tested[x],return.grid=T) ###
    part_plot1<-cbind(part_plot1, pp) ###
  } ###
  part_plot[[q]]<-part_plot1 ###
  
  sum1<-summary(BRT_SOSH_Estimate_PA_Models[q][[1]]  , plot=F )
  sum2<-sum1[order(sum1[,1], levels = var_tested),]
  percent_contrib<-cbind(percent_contrib, sum2[,2])
  rownames(percent_contrib)<-sum1[order(sum1[,1], levels = var_tested),1]
}
All_percent_contribution<-cbind(rownames(percent_contrib), paste(round(rowMeans(percent_contrib),2), round(rowSds(percent_contrib),2), sep=" ± "))
Combined_All_percent_contribution<-All_percent_contribution

library(matrixStats)
library(fmsb)
Mean_PA_Contributions<-as.data.frame(t(rowMeans(percent_contrib)))
PA_Predictors_Plot<- rbind(rep(max(Mean_PA_Contributions),11) , rep(0,11) , Mean_PA_Contributions)
PA_Predictors_Plot[]<-sapply(PA_Predictors_Plot, as.numeric)
par(mfrow=c(1,1))

png("PA_Radar_Chart_SOSH_Fixed.png", height=6, width=6, units="in",res=300)
radarchart(PA_Predictors_Plot,  pfcol=rgb(0.0,0.3,0.5,0.5), pcol=rgb(0.0,0.3,0.5,0.5), title="Presence/Absence" )
dev.off()


png("PA_partial_plots_SOSH_Fixed.png", height=9,width=6, res=300, units="in")
par(mfrow=c(4,3))
mn_part_plot<-list()  
for(y in 1:length(var_tested)){
  id<-which(colnames(part_plot[[1]])==var_tested[y])
  all1<-NULL
  all2<-NULL
  for(z in 1:iters){											 #this was 50 
    all1<-rbind(all1, cbind(c(part_plot[[z]][,id])))
    all2<-rbind(all2, cbind(c(part_plot[[z]][,id+1])))
  }
  all3<-cbind(all1, all2)
  all1<-all3[order(all3[,1]),]
  
  plot(all1, xlab=var_tested[y], col="white", ylab=paste("f(",var_tested[y], ")", sep=""),cex.axis=1.2, cex.lab=1.2) #, ylim=c(-8,2))
  plx<-predict(loess(all1[,2] ~ all1[,1], span = 0.3), se=T)
  mn_part_plot[[y]]<- cbind(all1[,1], plx$fit)      
  lines(all1[,1],plx$fit)
  lines(all1[,1],plx$fit - qt(0.975,plx$df)*plx$se, lty=2)#0.975
  lines(all1[,1],plx$fit + qt(0.975,plx$df)*plx$se, lty=2)
  legend("bottomright", paste(All_percent_contribution[which(All_percent_contribution[,1]==var_tested[y]),2],"%", sep=" "), bty="n", cex=1.4)
}
dev.off()


####look at partial plots for aggregations####
percent_contrib<-NULL#list()
iters=50
part_plot<-list()
part_plot<-list()
percent_contrib<-NULL#list()
for(q in 1:iters){                                #this was 50 
  mod<-BRT_SOSH_Estimate_Agg_Models[q][[1]]  ###
  part_plot1<-data.frame(row.names=1:100)
  for(x in 1:length(var_tested)){ ###
    pp<-plot(mod ,var_tested[x],return.grid=T) ###
    part_plot1<-cbind(part_plot1, pp) ###
  } ###
  part_plot[[q]]<-part_plot1 ###
  
  sum1<-summary(BRT_SOSH_Estimate_Agg_Models[q][[1]]  , plot=F )
  sum2<-sum1[order(sum1[,1], levels = var_tested),]
  percent_contrib<-cbind(percent_contrib, sum2[,2])
  rownames(percent_contrib)<-sum1[order(sum1[,1], levels = var_tested),1]
}
All_percent_contribution<-cbind(rownames(percent_contrib), paste(round(rowMeans(percent_contrib),2), round(rowSds(percent_contrib),2), sep=" ± "))
Combined_All_percent_contribution<-All_percent_contribution

library(matrixStats)

Mean_Agg_Contributions<-as.data.frame(t(rowMeans(percent_contrib)))
Agg_Predictors_Plot<- rbind(rep(max(Mean_Agg_Contributions),11) , rep(0,11) , Mean_Agg_Contributions)
Agg_Predictors_Plot[]<-sapply(Agg_Predictors_Plot, as.numeric)
par(mfrow=c(1,1))

png("Agg_Radar_Chart_SOSH_Fixed.png", height=6, width=6, units="in",res=300)
radarchart(Agg_Predictors_Plot,  pfcol=rgb(0.0,0.3,0.5,0.5), pcol=rgb(0.0,0.3,0.5,0.5), title="Aggregation" )
dev.off()




png("Agg_partial_plots_SOSH_Fixed.png", height=9,width=6, res=300, units="in")
par(mfrow=c(4,3))
mn_part_plot<-list()  
for(y in 1:length(var_tested)){
  id<-which(colnames(part_plot[[1]])==var_tested[y])
  all1<-NULL
  all2<-NULL
  for(z in 1:iters){											 #this was 50 
    all1<-rbind(all1, cbind(c(part_plot[[z]][,id])))
    all2<-rbind(all2, cbind(c(part_plot[[z]][,id+1])))
  }
  all3<-cbind(all1, all2)
  all1<-all3[order(all3[,1]),]
  
  plot(all1, xlab=var_tested[y], col="white", ylab=paste("f(",var_tested[y], ")", sep=""),cex.axis=1.2, cex.lab=1.2) #, ylim=c(-8,2))
  plx<-predict(loess(all1[,2] ~ all1[,1], span = 0.3), se=T)
  mn_part_plot[[y]]<- cbind(all1[,1], plx$fit)      
  lines(all1[,1],plx$fit)
  lines(all1[,1],plx$fit - qt(0.975,plx$df)*plx$se, lty=2)#0.975
  lines(all1[,1],plx$fit + qt(0.975,plx$df)*plx$se, lty=2)
  legend("bottomright", paste(All_percent_contribution[which(All_percent_contribution[,1]==var_tested[y]),2],"%", sep=" "), bty="n", cex=1.4)
}
dev.off()

Predictions_PA=matrix(, nrow=nrow(SOSH_For_Model_Trimmed), ncol=50)
Predictions_Agg=matrix(, nrow=nrow(SOSH_For_Model_Trimmed), ncol=50)

for (i in 1: 50){
  Mod_PA<-BRT_SOSH_Estimate_PA_Models[i]
  Mod_Agg<-BRT_SOSH_Estimate_Agg_Models[i]
Predictions_PA[,i]<-predict.gbm(Mod_PA[[1]],SOSH_For_Model_Trimmed, n.trees=Mod_PA[[1]]$gbm.call$best.trees, type="response")

  Predictions_Agg[,i]<-predict.gbm(Mod_Agg[[1]],SOSH_For_Model_Trimmed, n.trees=Mod_Agg[[1]]$gbm.call$best.trees, type="response")

}

Predictions_hurdle=rowMeans(Predictions_PA)*rowMeans(Predictions_Agg)
Predictions_hurdle_all<-Predictions_PA*Predictions_Agg


Hurdle_Model_Metrics<-matrix(,nrow=50, ncol=2)

for (i in 1: 50){
d <- cbind(SOSH_For_Model_Trimmed$Agg, Predictions_hurdle_all[,i])
pres <- d[d[,1]==1, 2]
abs <- d[d[,1]==0, 2]
e <- evaluate(p=pres, a=abs)
Hurdle_Model_Metrics[i,1]<-e@auc
Hurdle_Model_Metrics[i,2]<-max(e@TPR+e@TNR-1)
}


summary(Hurdle_Model_Metrics[,1])
summary(Hurdle_Model_Metrics[,2])


#SOSH_Error_Mapping<-ddply(SOSH_Train, .(Lon, Lat), summarize, Mean_Res=mean(PA_Res, na.rm=T))

setwd("F:/RREAS/SOSH")
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(viridis)
library(ggplot2)
library(raster)
library(ggplot2)
library(plyr)
library(ggnewscale)

setwd("F:/RREAS/Predictors/Static")
Depth1<-raster("z_1.grd")
Depth<-as.data.frame(Depth1,xy=TRUE)
colnames(Depth)<-c("Longitude","Latitude", "Depth")
#read in the Total_SOSHry coastline you are interested in 
library(ggnewscale)
setwd("F:/RREAS/SOSH")
world<-ne_countries(scale="medium", returnclass = "sf")
png("SOSH_PA_Residuals_BRT.png", height=6, width=6, units="in", res=200)
ggplot(data=world) + geom_raster(data = Depth,aes(x=Longitude, y = Latitude, fill=Depth),linejoin = "mitre") + scale_fill_gradient(low = "cornflowerblue", high = "white",limits=c(-5000,0))+new_scale_fill()+
  coord_fixed(ratio = 1)+geom_raster(data=SOSH_For_Model_Trimmed, aes(x=Lon+0.05, y=Lat+0.05, fill=PA_Resids))+ scale_fill_gradientn(colours=c("#0000FFFF","#F1ECE4","#FF0000FF"), limits=c(-1,1))+
  theme_bw()+geom_segment(aes(x = -124.7, y = 36, xend = -124.7, yend = 39), size=1.2,linetype=4)+geom_segment(aes(x = -124.7, y = 36, xend = -120, yend = 36), size=1.2,linetype=4)+geom_segment(aes(x = -124.7, y = 39, xend = -120, yend = 39), size=1.2,linetype=4)+
  labs(fill = "Residuals")+geom_sf()+coord_sf(xlim=c(-125,-120), ylim=c(36, 39))+
  theme(legend.title=element_text(size=16),legend.text=element_text(size=14),legend.direction = "vertical", legend.box = "vertical")+ggtitle("P/A Residuals")
dev.off()

png("SOSH_Agg_Residuals_BRT.png", height=6, width=6, units="in", res=200)
ggplot(data=world) + geom_raster(data = Depth,aes(x=Longitude, y = Latitude, fill=Depth),linejoin = "mitre") + scale_fill_gradient(low = "cornflowerblue", high = "white",limits=c(-5000,0))+new_scale_fill()+
  coord_fixed(ratio = 1)+geom_raster(data=SOSH_For_Model_Trimmed, aes(x=Lon+0.05, y=Lat+0.05, fill=Agg_Resids_SOSH))+ scale_fill_gradientn(colours=c("#0000FFFF","#F1ECE4","#FF0000FF"), limits=c(-1,1))+
  theme_bw()+geom_segment(aes(x = -124.7, y = 36, xend = -124.7, yend = 39), size=1.2,linetype=4)+geom_segment(aes(x = -124.7, y = 36, xend = -120, yend = 36), size=1.2,linetype=4)+geom_segment(aes(x = -124.7, y = 39, xend = -120, yend = 39), size=1.2,linetype=4)+
  labs(fill = "Residuals")+geom_sf()+coord_sf(xlim=c(-125,-120), ylim=c(36, 39))+
  theme(legend.title=element_text(size=16),legend.text=element_text(size=14),legend.direction = "vertical", legend.box = "vertical")+ggtitle("Agg. Residuals")
dev.off()







