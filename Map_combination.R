# install.packages() and load the package
library(ggplot2)
library(ggmap)
library(gridExtra)
library(cowplot) 
library(gtable)
library(grid)
library(patchwork)
library(dplyr)
library(hrbrthemes)

# load EOF modes data and satellite remote-sensing data
load(file="data/EOFs.RData")
load(file="data/RSdata.RData")
load(file="data/PCs.RData")
load(file="data/Eigenvalues.RData")

# extract first four EOF modes data with theirs corresponding latitude and longtitude
eofr=cbind(mod_rm[,2:3], EOFs)
eofm4=eofr[,1:6] 
colnames(eofm4) <- c("Lon", "Lat", "E1","E2","E3","E4")
eofm4f=data.frame(eofm4)

# create map 
#boundary coordinates of Tibetan Plateau
myLocation <- c(65, 25, 105, 45)
maptype = c("roadmap", "terrain", "satellite", "hybrid")
myMap = get_map(location = myLocation, source="google", maptype="satellite", crop=TRUE)
map_ti <- ggmap(myMap)

#Plot first four EOF modes
#for(i in 1:4){
#  scale=eofm4f[,i+2]
#  p<-ggmap(myMap) + geom_point(data=eofm4f, mapping=aes(x=Lon, y=Lat, colour=scale), size=2) +
#    scale_colour_gradient2(limits=c(-0.11,0.11),low="blue", mid="white", midpoint=0, high = "red", space="rgb") +
#    ggtitle(paste("EOF",i, sep="")) +
#    theme(plot.title = element_text(hjust = 0.5),legend.key.height = unit(1, "cm"), legend.key.width = unit(0.5, "cm"))+ 
#    labs(x="Longitude", y="Latitude")
#  print(p)
#}

scale1=eofm4f[,3]
p1<-map_ti + geom_point(data=eofm4f, mapping=aes(x=Lon, y=Lat, colour=scale1), size=2) +
  scale_colour_gradient2(limits=c(-0.11,0.11),low="blue", mid="white", midpoint=0, high = "red", space="rgb") +
  theme(plot.title = element_text(hjust = 0.5),legend.key.height = unit(1, "cm"), legend.key.width = unit(0.25, "cm"), plot.margin=unit(c(0.5,0.1,0.1,0), "lines")) +
  labs(x="Longitude", y="Latitudee",color="scale") + ggtitle("EOF 1")

scale2=eofm4f[,4]
p2<-map_ti + geom_point(data=eofm4f, mapping=aes(x=Lon, y=Lat, colour=scale2), size=2) +
  scale_colour_gradient2(limits=c(-0.11,0.11),low="blue", mid="white", midpoint=0, high = "red", space="rgb") +
  theme(plot.title = element_text(hjust = 0.5),legend.key.height = unit(1, "cm"), legend.key.width = unit(0.25, "cm"), plot.margin=unit(c(0.5,0.1,0.1,0), "lines")) +
  labs(x="Longitude", y="Latitudee",color="scale") + ggtitle("EOF 2")

scale3=eofm4f[,5]
p3<-map_ti + geom_point(data=eofm4f, mapping=aes(x=Lon, y=Lat, colour=scale3), size=2) +
  scale_colour_gradient2(limits=c(-0.11,0.11),low="blue", mid="white", midpoint=0, high = "red", space="rgb") +
  theme(plot.title = element_text(hjust = 0.5),legend.key.height = unit(1, "cm"), legend.key.width = unit(0.25, "cm"), plot.margin=unit(c(0.5,0.1,0.1,0), "lines")) +
  labs(x="Longitude", y="Latitudee",color="scale") + ggtitle("EOF 3")

scale4=eofm4f[,6]
p4<-map_ti + geom_point(data=eofm4f, mapping=aes(x=Lon, y=Lat, colour=scale4), size=2) +
  scale_colour_gradient2(limits=c(-0.11,0.11),low="blue", mid="white", midpoint=0, high = "red", space="rgb") +
  theme(plot.title = element_text(hjust = 0.5,face="bold"),legend.key.height = unit(1, "cm"), legend.key.width = unit(0.25, "cm"), plot.margin=unit(c(0.5,0.5,0.1,0), "lines")) +
  labs(x="Longitude", y="Latitudee") + ggtitle("EOF 4")

#Plot the first four PCs in one figure
time1=seq(1997,2011, len=45)
PCs <- data.frame(PCs)
PC1=PCs[,1]
PC2=PCs[,2]
PC3=PCs[,3]
PC4=PCs[,4]

pc1 <- ggplot() + geom_line(data=PCs, mapping=aes(time1,PC1)) + theme_bw() + ggtitle("PC1") + 
  labs(x="Time", y="Principle Component") + theme(plot.title = element_text(hjust = 0.5),
                                                  plot.margin = margin(t=0.2, r=3, b=0.1, l=0.1, unit="cm")) +
  xlim(1997,2011) 

pc2 <- ggplot() + geom_line(data=PCs, mapping=aes(time1,PC2)) + theme_bw() + ggtitle("PC2") + 
  labs(x="Time", y="Principle Component") + theme(plot.title = element_text(hjust = 0.5),
                                                  plot.margin = margin(t=0.2, r=3, b=0.1, l=0.1, unit="cm")) +
  xlim(1997,2011) 

pc3 <- ggplot() + geom_line(data=PCs, mapping=aes(time1,PC3)) + theme_bw() + ggtitle("PC3") + 
  labs(x="Time", y="Principle Component") + theme(plot.title = element_text(hjust = 0.5),
                                                  plot.margin = margin(t=0.2, r=3, b=0.1, l=0.1, unit="cm")) +
  xlim(1997,2011) 

# set graphic size
par(mar=c(4,4.5,1.5,4.5)) 
# Plot variances and cumulative variances from SVD eigenvalues
eig=(eigs)^2/45  
modn=1:45
plot(modn,100*eig/sum(eig), type='o',lwd=2.5,col='red', 
     xlab="", ylab="", main="Eigenvalues of covariance matrix",cex.axis=1, cex.lab=1.5)
mtext("Variance [%]",side=2,line=2.5, cex=1.5, col="red")
mtext("Mode Number",side=1,line=2.5, cex=1.5, col="black")
axis(2, col="red", col.ticks="red", col.axis="red", cex.axis=1)
par(new=TRUE)
varexp=100*cumsum(eig)/sum(eig)
plot(modn,varexp,type="o",col="blue",
     ylim=c(0,100),
     lwd=2.5,axes=FALSE,xlab="",ylab="", cex.axis=1.5)
axis(4, col="blue", col.ticks="blue", col.axis="blue", cex.axis=1)
mtext("Cumulative Variance [%]",side=4,line=2.5, cex=1.5, col="blue")

eig <- data.frame(eig)
eig_plot <- ggplot(data=eig,aes(x=modn)) + 
  geom_line(aes(y=100*eig/sum(eig)),size=1,color="red") +
  geom_line(aes(y=100*cumsum(eig)/sum(eig)),size=1,color="blue") +
  geom_point(aes(y=100*eig/sum(eig)),size=2,color="red") +
  geom_point(aes(y=100*cumsum(eig)/sum(eig)),size=2,color="blue") +
  ggtitle("Eigenvalues of Covariance Matrix") + theme(plot.title = element_text(hjust = 0.5,face="bold",size=16),
                                                      axis.title.y = element_text(color = "red", size=13),
                                                      axis.title.y.right = element_text(color = "blue", size=13),
            
                                                      axis.text=element_text(size=10),
                                                      axis.title=element_text(size=14,face="bold")) +
  scale_y_continuous(name="Variance [%]", sec.axis = sec_axis(~.,name="Cumulative Variance [%]")) + 
  scale_x_continuous(name="Mode Number",breaks=seq(0,45,by=5))

lay <- rbind(c(1,2),c(3,4),c(5,6))
gpls <- lapply(list(p1, p2, p3, pc1, pc2, pc3), ggplotGrob)
gridExtra::grid.arrange(gpls[[1]],gpls[[4]],gpls[[2]],gpls[[5]],gpls[[3]],gpls[[6]],layout_matrix=lay,widths=c(1,1),heights=c(1,1,1))

