# install.packages() and load the package
library(ggplot2)
library(matrixStats)
library(ggmap)

#load Eigenvalues data
load(file="data/OBdata.RData")
load(file="data/RSdata.RData")
load(file="data/EOFs.RData")

#Compute standardized anomalies
mod_o18<- mod_rm[, 4:48]# only O18 values, 845 obs.of 45 variables
clim_mod<- rowMeans(as.matrix(mod_o18), na.rm=TRUE)    
sd_mod<- rowSds(as.matrix(mod_o18), na.rm=TRUE)   
mod_std<- (mod_o18 - clim_mod) / sd_mod

# Map 
#boundary coordinates of Tibetan Plateau
myLocation <- c(65, 25, 105, 45)
#lon-lat of lowerleft and lon-lat of upperright
#maptype = c("terrain", "toner", "watercolor")
maptype = c("roadmap", "terrain", "satellite", "hybrid")
#rm(list=c("myMap"))
myMap = get_map(location = myLocation, source="google", maptype="satellite", crop=TRUE)

dato_O18<- dato[,5:31]
f<- data.frame(cbind(mod_rm[, 1], clim_mod, sd_mod))
climo<- f[f$V1 %in% dato[,2], ][,2]            ### boxId match obs.data and get rawmean(remote sensing) from that boxId
sdo<- f[f$V1 %in% dato[,2], ][,3]
dato_O18_std<- (dato_O18 - climo)/sdo
dato_std<- data.frame(dato[, 1:4], dato_O18_std)   
colnames(dato_std)<- colnames(dato)

# check number of stations with values
n_mode=c()
for (i in 5:31) {
  v=which(complete.cases(dato_std[,i])) #the boxes with data
  n_mode[i-4]=length(v)-1 
}
n_mode

eofr=cbind(mod_rm[,2:3], EOFs)
eofm3=eofr[,1:5] 
colnames(eofm3) <- c("Lon", "Lat", "E1","E2","E3")
eofm3f=data.frame(eofm3)

# only EOF3
recon=matrix(0,nrow=856,ncol=30)
for (i in c(5:31)) {y=complete.cases(dato_std[,i]) #check no missing value row
v=which(y)
u=dato_std[v,2]
datr=dato_std[v,i]
eofr=eofm3[u,c("E1","E2","E3")]
df=data.frame(eofr,datr)
reg=lm(formula=datr~E1+E2+E3, data=df)
coe=reg$coefficients
c1=rep(1,856)
res=cbind(c1,eofm3[,c("E1","E2","E3")])
#resm=data.frame(cbind(1,eofm4[,3:6]))
#coem=data.frame(coe)
recon[,i-1]=data.matrix(res)%*%coe
}

#Put grid ID, lat and lon as the first three columns
recon <- data.frame(recon)
recon[,1:3] <- mod_rm[,1:3]
jja=rep(c("Jun","Jul","Aug"),9)
yr2=rep(1997:2005,each=3)
hdjja1=paste(jja,yr2)
colnames(recon)<-c("BoxID","Lon","Lat", hdjja1)

# reconstructed data
gridout1<-recon
gridout1ori_O18<- gridout1[, 4:30]*sd_mod + clim_mod
gridout1[, 4:30]<- gridout1ori_O18

save(gridout1, file = "Reconstruction.RData")
