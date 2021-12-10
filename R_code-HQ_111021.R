rm(list=ls(all=TRUE)) #Clear the environment.
#set up the working file.
setwd("C:/Users/hniqd/OneDrive/Documents/TP2020-07-03YaoChen/20200904_data_visulization")
library(RColorBrewer)
library(zoo)
library(dplyr)
library(reshape2)
library(R.matlab)
library(readxl)
library(lubridate)
library(ggmap)
library(matrixStats)

#Read model data
mod_dt <- read_xls("C:/Users/hniqd/OneDrive/Documents/TP2020-07-03YaoChen/20200904_data_visulization/data_O18.xls")

#Extract summer months in model data 
#Work on the column names
mod_dt_summer <- as.data.frame(matrix(1,ncol=48,nrow=9009))
mod_dt_summer[ , 1:3] <- mod_dt[,1:3]
mod_dt_summer[ , seq(4,46, by =3 )] <- mod_dt[grepl("Jun", names(mod_dt))]
mod_dt_summer[ , seq(5,47, by =3 )] <- mod_dt[grepl("Jul", names(mod_dt))]
mod_dt_summer[ , seq(6,48, by =3 )] <- mod_dt[grepl("Aug", names(mod_dt))]
names(mod_dt_summer)[1:3] <- c("BoxID", "Lon", "Lat")
names(mod_dt_summer)[4:48] <- paste(rep(1997:2011, each = 3), rep(c("Jun", "Jul", "Aug"), 15))
mod_data <- sapply(mod_dt_summer, as.numeric)
mod_data[mod_data == -1000] <- NA
col_names_mod<- colnames(mod_data)
#write.csv(mod_data,"mod_data_summer.csv")

#Project to the desired area
#Read the 2016-09-24 reconstructed data to get lat and lon coordinates
tpdat1=read.csv("C:/Users/hniqd/OneDrive/Documents/TP2020-07-03YaoChen/InputData/reconout160924r.csv", header=TRUE)
tpdatlatlon=tpdat1[,2:3] #909 obs. of 2 variables
colnames(tpdatlatlon) <- c("Lon","Lat")
tpdat=data.frame(tpdatlatlon)

#Extract the corresponding rows from the model data to match the Lon-Lat of interested.
mod_data<- data.frame(mod_data)
colnames(mod_data)<- col_names_mod
mod_data1<- merge(mod_data, tpdat)# 909 obs. of 48 variables
attach(mod_data1)
mod_data2<- mod_data1[order(Lat, Lon), ]
detach(mod_data1)
mod_data2$BoxID<- 1:909
row.names(mod_data2)<- NULL
#mod_data2 909 obs. of 48 variables with the same BoxID as the previous model data.
names(mod_data2)
#"Lon" "Lat" "BoxID" "1997 Jun" ...

#Remove NA and leave the data with values.
#which(is.na(mod_data2))
#apply(is.na(mod_data2), 2, which) 
mod_rm<- na.omit(mod_data2) # 856 obs. of 48 varaibles
dim(mod_rm)
#[1] 856  48

mod_o18<- mod_rm[, 4:48]# only O18 values, 845 obs.of 45 variables
dim(mod_o18)
#[1] 856  45

#Compute standardized anomalies
clim_mod<- rowMeans(as.matrix(mod_o18), na.rm=TRUE)
length(clim_mod)
#[1] 856
sd_mod<- rowSds(as.matrix(mod_o18), na.rm=TRUE)
length(sd_mod)
#[1] 856
mod_std<- (mod_o18 - clim_mod) / sd_mod
dim(mod_std)
#[1] 856  45

#Plot the model data anomalies used for generating EOFs
#For July 1997
library(ggplot2)
library(ggmap)
#boundary coordinates of Tibetan Plateau
myLocation <- c(65, 25, 105, 45)
#lon-lat of lowerleft and lon-lat of upperright
#maptype = c("terrain", "toner", "watercolor")
maptype = c("roadmap", "terrain", "satellite", "hybrid")
rm(list=c("myMap"))
myMap = get_map(location = myLocation, source="google", maptype="terrain", crop=TRUE)
tp=ggmap(myMap)
ggmap(myMap)

#get lat and lon coordinates
tpdatlatlon=mod_rm[,1:2]
colnames(tpdatlatlon) <- c("Lon", "Lat")
tpdat=data.frame(tpdatlatlon)
moddf=data.frame(mod_std)
#Plot the July 1997 model data
O18=moddf$X1997.Jul
ggmap(myMap) + geom_point(data=tpdatlatlon, mapping=aes(x=Lon, y=Lat, colour=O18), size=2) +
  scale_colour_gradient2(limits=c(-3.5,3.5),low="blue",mid="white", 
                         midpoint=0, high = "red", space="rgb")+
  ggtitle("Model O18 Anomalies: July 1997") +
  theme(plot.title = element_text(hjust = 0.5))

#Compute EOF using SVD
jjasvd=svd(mod_std)
eig=(jjasvd$d)^2/45
eig4=sum(eig[1:4])/sum(eig)
eig4
#[1] 0.8328875
eig3=sum(eig[1:3])/sum(eig)
eig3
#[1] 0.7928953

#Plot the eigenvalues vs mode number
par(mar=c(4,4.5,0.5,4.5))
modn=1:45
plot(modn,100*eig/sum(eig), type='o',lwd=2.5,col='red', 
     xlab="", ylab="",cex.axis=1.5, cex.lab=1.5)
mtext("Eigenvalues [%]",side=2,line=3, cex=1.5, col="red")
mtext("EOF Mode Number",side=1,line=3, cex=1.5, col="black")
axis(2, col="red", col.ticks="red", col.axis="red", cex.axis=1.5)
par(new=TRUE)
varexp=100*cumsum(eig)/sum(eig)
plot(modn,varexp,type="o",col="blue",
     ylim=c(0,100),
     lwd=2.5,axes=FALSE,xlab="",ylab="", cex.axis=1.5)
axis(4, col="blue", col.ticks="blue", col.axis="blue", cex.axis=1.5)
mtext("Percent Cumulative Eigenvalue [%]",side=4,line=3, cex=1.5, col="blue")
dev.off
#Plot the eigenvalues vs mode number
par(mar=c(4,4,3,4))
modn=1:45
plot(modn,100*eig/sum(eig), type="o", xlab="EOF Mode Number",
     ylab="Eigenvalues [%]",
     main="Scree Plot and Cumuilative Variance Explained")
#legend(10, 400, lty=1, lwd=3.0, 
#       legend=c("Eigenvalues"),bty="n",text.font=2, cex=1.0)
par(new=TRUE)
varexp=100*cumsum(eig)/sum(eig)
plot(modn,varexp, type="o",col="red",lwd=1.5, 
     axes=FALSE, xlab="",ylab="")
#legend(2, 80, lty=1, lwd=3.0, col="red",text.col="red",
#       legend=c("Percent Cumulative Eigenvalues"),bty="n",text.font=2, cex=1.0)
axis(4, col="red",col.ticks="red", col.axis="red", )
mtext("Percent Cumulative Eigenvalue [%]", side=4, line=3, col = "red", cex.axis=1.5)

#Mark lat and lon data as the first two columns of the EOF data 
eofm=jjasvd$u #EOF vectors
dim(eofm)
#[1] 856  45
eofr=cbind(mod_rm[,1:2], eofm)
dim(eofr)
#[1] 856  47 # 47 columns=lat, lon, plus 45 EOF vectors
#Save 4 EOFs and 6 EOFs
colnames(eofm, do.NULL = FALSE)
eofm4=eofr[, 1:6]
eofm6=eofr[,1:8]
colnames(eofm4)<- c("Lon", "Lat", "E1","E2","E3","E4")
colnames(eofm6) <- c("Lon", "Lat", "E1","E2","E3","E4","E5","E6")
eofm4f=data.frame(eofm4)
eofm6f=data.frame(eofm6) #The first six EOFs in data frame


#Plot the EOFs
#rm(list=ls())
library(ggplot2)
library(ggmap)
#sq_map <- get_map(location = sbbox, maptype = "satellite", source = "google")
#boundary coordinates of Tibetan Plateau
myLocation <- c(65, 25, 105, 45)
#lon-lat of lowerleft and lon-lat of upperright
#maptype = c("terrain", "toner", "watercolor")
maptype = c("roadmap", "terrain", "satellite", "hybrid")
rm(list=c("myMap"))
myMap = get_map(location = myLocation, source="google", maptype="terrain", crop=TRUE)
tp=ggmap(myMap)
ggmap(myMap)
#Read the EOF data for TP
tpdat=data.frame(eofm6)
#plot the first six EOFs and save the figures
setwd("C:/Users/hniqd/OneDrive/Documents/TP2020-07-03YaoChen/2017-10-28Computing/EOFs")
for(i in 1:6){
  scale=tpdat[,i+2]
  #ggplot of the first six EOFs
  p<- ggmap(myMap) + geom_point(data=tpdat, mapping=aes(x=Lon, y=Lat, colour=scale), size=2) +
    scale_colour_gradient2(limits=c(-0.11,0.11),low="blue", mid="white", midpoint=0, high = "red", space="rgb") +
    ggtitle(paste("EOF",i, sep="")) +
    theme(plot.title = element_text(hjust = 0.5),legend.key.height = unit(1, "cm"), legend.key.width = unit(0.5, "cm"))+ 
    labs(x="Longitude", y="Latitude")
  png(paste("Pattern of EOF",i, ".png", sep = ""), width=600, height=400, res=120)
  print(p)
  dev.off()
}

#Plot EOF2
scale=tpdat$E2
ggmap(myMap) + geom_point(data=tpdat, mapping=aes(x=Lon, y=Lat, colour=scale), size=2) +
  scale_colour_gradient2(limits=c(-0.11,0.11),low="blue", mid="white", midpoint=0, high = "red", space="rgb")+
  ggtitle("EOF2") +
  theme(plot.title = element_text(hjust = 0.5),legend.key.height = unit(0.7, "cm"), legend.key.width = unit(0.5, "cm"))
#Plot EOF3
O18=tpdat$E3
ggmap(myMap) + geom_point(data=tpdat, mapping=aes(x=Lon, y=Lat, colour=O18), size=2) +
  scale_colour_gradient2(limits=c(-0.11,0.11),low="blue", mid="white", midpoint=0, high = "red", space="rgb")+
  ggtitle("EOF3") +
  theme(plot.title = element_text(hjust = 0.5))

#Plot PCs
#Plot the first three PCs
setwd("C:/Users/hniqd/OneDrive/Documents/TP2020-07-03YaoChen/2017-10-28Computing/PCs")
time1 = seq(1997,2011, len = 45)
dev.off()
for(i in 1:3){
  pc = jjasvd$v[,i]
  png(paste("PC", i, ".png", sep = ""), width = 700, height = 400, res = 120)
  p<-  plot(time1, pc, type = "o", 
            main = paste("Principal Component #",i, sep = ""),
            xlab = "Time", ylab = "PC values",
            ylim = c(-0.45,0.4), lwd = 1.5)
  
  print(p)
  dev.off()
}

for(i in 4:6){
  pc = jjasvd$v[,i]
  png(paste("PC", i, ".png", sep = ""), width = 700, height = 400, res = 120)
  p<-  plot(time1, pc, type = "o", 
            main = paste("Principal Component #",i, sep = ""),
            xlab = "Time", ylab = "PC values",
            ylim = c(-0.45,0.4), lwd = 1.5)
  
  print(p)
  dev.off()
}

time1=seq(1997,2011, len=45)
pc1=jjasvd$v[,1]
plot(time1, pc1, type="o", 
     main="Principal Component #1", 
     xlab="Time", ylab="PC values", ylim=c(-0.45,0.4), lwd=1.5)

pc2=jjasvd$v[,2]
plot(time1, pc2, type="o", col="red",
     main="Principal Component #2", 
     xlab="Time", ylab="PC values",
     ylim=c(-0.4,0.4), lwd=1.5)

pc3=jjasvd$v[,3]
plot(time1, pc3, type="o", col="blue",
     main="Principal Component #3", 
     xlab="Time", ylab="PC values",
     ylim=c(-0.45,0.40), lwd=1.5)

#first Three PCs in one plot
time1=seq(1997,2011, len=45)
pc1=jjasvd$v[,1]
pc2=jjasvd$v[,2]
pc3=jjasvd$v[,3]
plot(time1, -pc1, type="l", col="black",
     main="The First Three Principal Components", 
     xlab="Time", ylab="PC values",
     ylim=c(-0.8,0.4), lwd=2.0)
lines(time1,pc2,col="red", type="l", lty=2, lwd=2.0 )
lines(time1,pc3,col="blue",lwd=2.0,type="l", lty=3)
legend("bottomleft", legend=c("pc1", "pc2", "pc3"),
       col=c("black", "red", "blue"), lty=1:3, bty = "n",text.font = 1,
       cex = 0.4)
#PC4-PC6 in one plot
time1=seq(1997,2011, len=45)
pc4=jjasvd$v[,4]
pc5=jjasvd$v[,5]
pc6=jjasvd$v[,6]
plot(time1, pc4, type="l", col="black",
     main="PC 4 to PC 6", 
     xlab="Time", ylab="PC values",
     ylim=c(-0.8,0.4), lwd=2.0)
lines(time1,pc5,col="red", type="l", lty=2, lwd=2.0 )
lines(time1,pc6,col="blue",lwd=2.0,type="l", lty=3)
legend("bottomleft", legend=c("pc4", "pc5", "pc6"),
       col=c("black", "red", "blue"), lty=1:3, bty = "n",text.font = 1,
       cex = 0.4)

#Outliers Detection
dato=read.csv("C:/Users/hniqd/OneDrive/Documents/TP2020-07-03YaoChen/InputData/stations17-data-JJA-1997-2006-09-23.csv",header=TRUE)
colnames(dato)[2:4]<- c("BoxID", "Lat", "Lon")
colnames(dato)[5:34]<- colnames(mod_rm)[4:33]
IDloc<- dato[, 1:4]
stnyr=dim(dato)
stnyr
#[1] 17 34 #17 grid boxes with stations, 
#10 years (1997-2006), JJA =>30 months
#First 4 columns: Stn name, stn ID, lat, lon
dato_O18<- dato[, 5:34]
dim(dato_O18)
#[1] 17 30
sum(!is.na(dato_O18))
#[1] 204 nonNA values
datoutmin<- matrix(NA, nrow=17, ncol = 30)
datoutmin2<- matrix(NA, nrow=17, ncol = 30)
datoutmax<- matrix(NA, nrow=17, ncol = 30)
min1<- c()
min2<- c()
max1<- c()
for (i in 1:30){
  sd1<- sd(as.matrix(dato_O18), na.rm=TRUE)
  min1[i]<- mean(dato_O18[, i], na.rm=TRUE)-3*sd1
  min2[i]<- mean(dato_O18[, i], na.rm=TRUE)-2*sd1
  max1[i]<- mean(dato_O18[, i], na.rm=TRUE)+3*sd1
  datoutmin[which(dato_O18[, i]>min1[i]), i]<- dato_O18[which(dato_O18[, i]>min1[i]), i]
  datoutmin2[which(dato_O18[, i]>min2[i]), i]<- dato_O18[which(dato_O18[, i]>min2[i]), i]
  datoutmax[which(dato_O18[, i]<max1[i]), i]<- dato_O18[which(dato_O18[, i]<max1[i]), i]
}
sum(!is.na(datoutmin)) 
#[1] 204
sum(!is.na(datoutmin2))
#[1] 201
sum(!is.na(datoutmax))
#[1] 203
dato_rmmin<- cbind(IDloc, datoutmin2)
dato_rmmax<- cbind(IDloc, datoutmax)


#4-mode reconstruction for every month
dato_O18<- dato[, 5:34]
dato_O18<- dato_rmmin[, 5:34]
f<- data.frame(cbind(mod_rm[, 3], clim_mod, sd_mod))
climo<- f[f$V1 %in% dato[,2], ][,2]
sdo<- f[f$V1 %in% dato[,2], ][,3]
dato_O18_std<- (dato_O18[-17, ] - climo)/sdo 
#model data does not have correspondences for the last station with BoxID 909
dim(dato_O18[-17, ])
#[1] 16 30
dato_std<- data.frame(dato[-17, 1:4], dato_O18_std)
colnames(dato_std)<- colnames(dato)

nonNA<- colSums(!is.na(dato_std))
nonNA
#Jun 1998 (dato[,8]), 1998 Aug (dato[, 10]), 1999 Aug (dato[, 13]) and Jun 2006(dato[, 32]) 
#have only 4 obs. 

recon=matrix(0,nrow=856,ncol=33)
for (i in 5:34) {y=complete.cases(dato_std[,i])
v=which(y)
u=dato_std[v,2]
datr=dato_std[v,i]
eofr=eofm4[u,c("E1","E2","E3","E4")]
df=data.frame(eofr,datr)
reg=lm(formula=datr~E1+E2+E3+E4, data=df)
coe=reg$coefficients
c1=rep(1,856)
res=cbind(c1,eofm4[,c("E1","E2","E3","E4")])
#resm=data.frame(cbind(1,eofm4[,3:6]))
#coem=data.frame(coe)
recon[,i-1]=data.matrix(res)%*%coe
}

#Test for an individual month
i=5
y=complete.cases(dato_std[,i])
v=which(y)
u=dato_std[v,2] #BoxID of non-NA values
datr=dato_std[v,i] #non-NA values in this year
eofr=eofm4[u,c("E1","E2","E3","E4")]
df=data.frame(eofr,datr)
reg=lm(formula=datr~E1+E2+E3+E4, data=df)
coe=reg$coefficients
c1=rep(1,856)
res=cbind(c1,eofm4[,c("E1","E2","E3","E4")])
#resm=data.frame(cbind(1,eofm4[,3:6]))
#coem=data.frame(coe)
recon1=data.matrix(res)%*%coe
#test done

#Put grid ID, lat and lon as the first three columns
recon[,1]=mod_rm[,3]
recon[,2]=mod_rm[,1]
recon[,3]=mod_rm[,2]
#Create a proper header
jja=rep(c("Jun","Jul","Aug"),10)
yr2=rep(1997:2006,each=3)
hdjja1=paste(jja,yr2)
colnames(recon)<-c("BoxID","Lon","Lat", hdjja1)
recon[1:3,1:7]
#June 1998 and June 2006 had only four data boxes, which cannot 
#support four modes regression, thus results are NA for these two months

#Three-mode reconstruction for the four missing months above
recon3=matrix(0,nrow=856,ncol=33)
for (i in 5:34) {y=complete.cases(dato_std[,i])
v=which(y)
u=dato_std[v,2]
datr=dato_std[v,i]
eofr=eofm4[u,c("E1","E2","E3")]
df=data.frame(eofr,datr)
reg=lm(formula=datr~E1+E2+E3, data=df)
coe=reg$coefficients
c1=rep(1,856)
res=cbind(c1,eofm4[,c("E1","E2","E3")])
recon3[,i-1]=data.matrix(res)%*%coe
}

#Put grid ID, lat and lon as the first three columns
recon3[,1]=mod_rm[,3]
recon3[,2]=mod_rm[,1]
recon3[,3]=mod_rm[,2]
#Put proper header
jja=rep(c("Jun","Jul","Aug"),10)
yr2=rep(1997:2006,each=3)
hdjja1=paste(jja,yr2)
colnames(recon3)<-c("BoxID","Lon","Lat", hdjja1)
frecon3<- recon3[, c(7, 9, 12, 31)]

fourmodrecon<- recon
recon[, c(7, 9, 12, 31)]<- frecon3
write.csv(recon,file="C:/Users/hniqd/OneDrive/Documents/TP2020-07-03YaoChen/2017-10-28Computing/reconoutmix.csv")

#plot the results: space-time averages
gridout2=read.csv("C:/Users/hniqd/OneDrive/Documents/TP2020-07-03YaoChen/2017-10-28Computing/reconoutmix.csv",header=TRUE)
dim(gridout2)
#[1] 856  34 #The first column is the grid ID
timeave2=rowMeans(gridout2[,5:34]) #Time ave 
areaw2=cos((pi/180)*gridout2[,4])/sum(cos((pi/180)*gridout2[,4]))
#Show area weight variation wrt lat, close to be uniform
plot(areaw2, ylim=c(1/856-0.0002,1/856+0.0002))
wtgrid2=areaw2*gridout2[,5:34] #Area weighted data
spaceave2=colSums(wtgrid2) #Spatial ave
write.csv(spaceave2,file="C:/Users/hniqd/OneDrive/Documents/TP2020-07-03YaoChen/2017-10-28Computing/spaceave2mix.csv")
write.csv(timeave2,file="C:/Users/hniqd/OneDrive/Documents/TP2020-07-03YaoChen/2017-10-28Computing/timeave2mix.csv")

#Plot space and time average
length(spaceave2)
#[1] 30
length(timeave2)
#[1] 856
plot(seq(1997,2006,len=30),spaceave2,type="o", ylim=c(-25,30), 
     main="Spatial Average O18 Anomalies",
     xlab="Time: June, July, August of each year", 
     ylab="TP spatial average O18", lwd=1.5)

##Large values for June 2006 (-24.21), July 2000 (-7.68), and Aug 1999 (-7.11) 
#which(recon[, 4:33] == max(recon[, 4:33]), arr.ind = TRUE)
#Max=56.66534 is in Jun 2006, recon[297,31]=56.67
#Min=-125.2796 is in Jun 2006, recon[711, 31]=-125.28


#Plot the data for a few months to check their ranges
gridid=1:856
plot(gridid, recon[,31],type="l", ylim=c(-150,100))
lines(gridid,recon[,30],type="l", col="red")
lines(gridid,recon[,32],type="l", col="blue")

plot(seq(1,856),timeave2,type="l", ylim=c(-5,2),
     main="Temporal Average O18 Anomalies over Tibetan Plateau: 3-Mode Reconstruction",
     xlab="Grid box ID from 1 to 856", 
     ylab="TP 1997-2006 average O18", lwd=1.5)

#test: plot reconstruction result
#tp=ggmap(myMap)
rm(list=c("myMap"))
myLocation <- c(60, 25, 110, 45)
#maptype = c("roadmap", "terrain", "satellite", "hybrid")
myMap = get_map(location = myLocation, source="google", maptype="terrain", crop=TRUE)
ggmap(myMap) + labs(x="Longitude", y="Latitude")
tpdatlatlon=data.frame(recon)
i=7
O18=pmax(pmin(recon[,i],10),-10) 
ggmap(myMap) + geom_point(data=tpdatlatlon, mapping=aes(x=Lon, y=Lat, colour=O18), size=2) +
  scale_colour_gradient2(limits=c(-15,15),low="blue",mid="white", 
                         midpoint=0, high = "red", space="rgb")+
  ggtitle(paste("Model O18 Anomalies:", hdjja1[i-3])) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Longitude", y="Latitude")

#Plot the reconstruction data and save the figures in a folder
setwd("C:/Users/hniqd/OneDrive/Documents/TP2020-07-03YaoChen/2017-10-28Computing/ReconFigs")
tpdatlatlon=data.frame(recon)
for(i in 4:33){
  scale=pmax(pmin(recon[,i],10),-10) 
  p<- ggmap(myMap) + geom_point(data=tpdatlatlon, mapping=aes(x=Lon, y=Lat, colour=scale), size=1.5) +
    scale_colour_gradient2(limits=c(-15,15),low="blue",mid="white", 
                           midpoint=0, high = "red", space="rgb")+
    ggtitle(paste("Reconstructed O18 Anomalies:", hdjja1[i-3])) +
    theme(plot.title = element_text(hjust = 0.5),legend.key.height = unit(0.8, "cm"), legend.key.width = unit(0.5, "cm"))+
    labs(x="Longitude", y="Latitude")
  png(paste("Reconstructed O18 Anomalies",hdjja1[i-3], ".png", sep = ""), width=600, height=400, res=120)
  print(p)
  dev.off()
}

#Plot the observed data and save the figures in a folder
setwd("C:/Users/hniqd/OneDrive/Documents/TP2020-07-03YaoChen/2017-10-28Computing/ObservedFigs")
rm(list=c("myMap"))
#myLocation <- c(10, 20, 115, 50)
#maptype = c("roadmap", "terrain", "satellite", "hybrid")
myMap = get_map(location = c(60, 25, 110, 45), source="google", maptype="terrain", crop=TRUE)
ggmap(myMap)
tpdatlatlon=data.frame(dato)
for(i in 5:34){
  scale=pmax(pmin(dato[,i],10),-10) 
  p<- ggmap(myMap) + geom_point(data=tpdatlatlon, mapping=aes(x=Lon, y=Lat, colour=scale), size=3.6) +
    scale_colour_gradient2(limits=c(-10,10),low="blue",mid="white", 
                           midpoint=0, high = "red", space="rgb")+
    ggtitle(paste("Observed O18 Anomalies:", hdjja1[i-4])) +
    theme(plot.title = element_text(hjust = 0.5),legend.key.height = unit(0.8, "cm"), legend.key.width = unit(0.5, "cm")) + 
    labs(x="Longitude", y="Latitude")
  png(paste("Observed O18 Anomalies ",hdjja1[i-4], ".png", sep = ""), width=600, height=400, res=120)
  print(p)
  dev.off()
}

#Plot the model data and save the figures in a folder
setwd("C:/Users/hniqd/OneDrive/Documents/TP2020-07-03YaoChen/2017-10-28Computing/ModelFigs")
rm(list=c("myMap"))
#myLocation <- c(10, 20, 115, 50)
#maptype = c("roadmap", "terrain", "satellite", "hybrid")
myMap = get_map(location = c(60, 25, 110, 45), source="google", maptype="terrain", crop=TRUE)
ggmap(myMap)
tpdatlatlon =data.frame(mod_rm)
jja=rep(c("Jun","Jul","Aug"),15)
yr2=rep(1997:2011,each=3)
hdjja2=paste(jja,yr2)
for(i in 4:48){
  scale=pmax(pmin(tpdatlatlon[,i],10),-10) 
  p<- ggmap(myMap) + geom_point(data=tpdatlatlon, mapping=aes(x=Lon, y=Lat, colour=scale), size=1.5) +
    scale_colour_gradient2(limits=c(-15,5),low="blue",mid="white", 
                           midpoint=0, high = "red", space="rgb")+
    ggtitle(paste("Model O18 Anomalies:", hdjja2[i-3])) +
    theme(plot.title = element_text(hjust = 0.5), legend.key.height = unit(0.8, "cm"), legend.key.width = unit(0.5, "cm")) + 
    labs(x="Longitude", y="Latitude")
  png(paste("Model O18 Anomalies ",hdjja2[i-3], ".png", sep = ""), width=600, height=400, res=120)
  print(p)
  dev.off()
}

#Validation
#Read the obs data
dim(dato)
#[1] 17 34 #17 grid boxes with stations, 
#10 years (1997-2006), JJA =>30 months
#First 4 columns: Stn name, stn ID, lat, lon
obsave=colMeans(dato[5:34], na.rm=T)
obsave<- colMeans(dato_rmmin[5:34],na.rm=T)
plot(obsave, type="l")

#Compare station and itd grid box data
dim(gridout2)
#gridout2 is data.frame of recon results
#[1] 856  34 #The first column is the grid ID
gridout2ori_O18<- gridout2[, 5:34]*sd_mod + clim_mod
gridout2[, 5:34]<- gridout2ori_O18
#dato=read.csv("C:/research/TP2020-07-03YaoChen/InputData/stations17-data-JJA-1997-2006-09-23.csv",header=TRUE)
dim(dato)
#[1] 17 34
stn_name=c("Nyalam","Dingri","Dui","Baidi",
           "Lhasa",
           "Lulang",  "Bomi","Nagqu",
           "Shiquanhe","Gaize",
           "Yushu","Tuotuohe","Lanzhou","Delingha", 
           "Taxkorgen","Zhangye", "Urumuqi")
#Zhangmu and Wengguo stations are not included. 
#19-2 = 17 stations
grid_id=dato[,2]
length(stn_name)
#[1] 17
length(grid_id)
#[1] 17

#Zhangye 2 vs 855
plot(t1,dato[16,5:34],type="o", ylim=c(-20,20), 
     main="Station and Reconstructed O18 Anomalies: Zhangye",
     xlab="Time: June, July, August of each year", 
     ylab="O18", lwd=1.5)
lines(t1,gridout2[which(gridout2$BoxID==855),5:34], type="l", col="blue")

#Dui 3 vs 51
plot(t1,dato[3,5:34],type="o", ylim=c(-40,30), 
     main="Station and Reconstructed O18 Anomalies: Dui",
     xlab="Time: June, July, August of each year", 
     ylab="O18", lwd=1.5)
lines(t1,gridout2[which(gridout2$BoxID==51),5:34], type="l", col="blue")
dev.off()
    
plot.new()
#png(file = 'monthtrend.png') #Automatical saving of a figure
t1=seq(1997,2006,len=30)
par(mfrow = c(3, 3))  # 4 rows and 4 columns
par(mgp=c(2,1,0))
par(mar=c(3,3,2,3))
for (i in 1:9) { 
  plot(t1, dato[i,5:34],type="o", ylim=c(-20,20),
       xlab="",ylab="",
       cex.axis=1.5,cex.lab=1.5,
       main = paste("Station", stn_name[i],",", "Grid ID", grid_id[i]))
  legend(1995, 56,  col=c("black"),lwd=2.0, lty=1,
         legend=c("Station data"),
         bty="n",text.font=2.0,cex=1.0, seg.len = 0.8) 
  lines(t1, gridout2[grid_id[i], 5:34], col="blue") 
  text(1998,-15, paste("(",letters[i],")"), cex=2.0)
  legend(1995, 52,  col=c("blue"),lwd=2.0, lty=1,
         legend=c("Reconstructed data"),text.col = "blue",
         bty="n",text.font=2.0,cex=1.0, seg.len = 0.8) 
}

plot.new()
#png(file = 'monthtrend.png') #Automatical saving of a figure
t1=seq(1997,2006,len=30)
par(mfrow = c(3, 3))  # 4 rows and 4 columns
par(mgp=c(2,1,0))
par(mar=c(3,3,2,1))
for (i in 10:17) { 
  plot(t1, dato[i,5:34],type="o", ylim=c(-20,20),
       xlab="",ylab="",
       cex.axis=1.5,cex.lab=1.5,
       main = paste("Station", stn_name[i],",", "Grid ID", grid_id[i]))
  
  lines(t1, gridout2[grid_id[i], 5:34], col="blue") 
  text(1998,-15, paste("(",letters[i],")"), cex=2)
}

