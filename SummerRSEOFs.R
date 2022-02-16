# install.packages() and load the package
library(ggplot2)
library(ggmap)
library(gridExtra)
library(cowplot) 
library(gtable)
library(grid)

# load EOF modes data and satellite remote-sensing data
load(file="data/EOFs.RData")
load(file="data/RSdata.RData")

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
  theme(plot.title = element_text(hjust = 0.5,size=12,vjust=0),legend.position = "none", plot.margin=unit(c(0.5,0.5,0,0), "lines")) +
  labs(x=NULL, y=NULL) + ggtitle("EOF 1")

scale2=eofm4f[,4]
p2<-map_ti + geom_point(data=eofm4f, mapping=aes(x=Lon, y=Lat, colour=scale2), size=2) +
  scale_colour_gradient2(limits=c(-0.11,0.11),low="blue", mid="white", midpoint=0, high = "red", space="rgb") +
  theme(plot.title = element_text(hjust = 0.5,size=12,vjust=0),legend.position = "none", plot.margin=unit(c(0.5,0.5,0,0), "lines")) +
  labs(x=NULL, y=NULL) + ggtitle("EOF 2")

scale3=eofm4f[,5]
p3<-map_ti + geom_point(data=eofm4f, mapping=aes(x=Lon, y=Lat, colour=scale3), size=2) +
  scale_colour_gradient2(limits=c(-0.11,0.11),low="blue", mid="white", midpoint=0, high = "red", space="rgb") +
  theme(plot.title = element_text(hjust = 0.5,size=12,vjust=0),legend.position = "none", plot.margin=unit(c(0.5,0.5,0,0), "lines")) +
  labs(x=NULL, y=NULL) + ggtitle("EOF 3")

scale4=eofm4f[,6]
p4<-map_ti + geom_point(data=eofm4f, mapping=aes(x=Lon, y=Lat, colour=scale4), size=2) +
  scale_colour_gradient2(limits=c(-0.11,0.11),low="blue", mid="white", midpoint=0, high = "red", space="rgb") +
  theme(plot.title = element_text(hjust = 0.5,size=12,vjust=0),legend.position = "none", plot.margin=unit(c(0.5,0.5,0,0), "lines")) +
  labs(x=NULL, y=NULL) + ggtitle("EOF 4")

p <- map_ti + geom_point(data=eofm4f, mapping=aes(x=Lon, y=Lat, colour=scale1), size=2) +
  scale_colour_gradient2(limits=c(-0.11,0.11),low="blue", mid="white", midpoint=0, high = "red", space="rgb") +
  ggtitle(paste("EOF 1")) +
  theme(plot.title = element_text(hjust = 0.5,size=12,vjust=0), legend.key.height = unit(2, "cm"), legend.key.width = unit(0.5, "cm"))+ 
  labs(x="Longitude", y="Latitude")

g2 <- function(a.gplot){
  if (!gtable::is.gtable(a.gplot))
    a.gplot <- ggplotGrob(a.gplot)
  gtable::gtable_filter(a.gplot, 'guide-box', fixed=TRUE)
}

legend <- g2(p)

#margin = theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
lay <- rbind(c(1,2,5),c(3,4,5))
gpls <- lapply(list(p1, p2, p3, p4), ggplotGrob)
gridExtra::grid.arrange(gpls[[1]],gpls[[2]],gpls[[3]],gpls[[4]],legend,layout_matrix=lay,
                        bottom = textGrob("Latitude",just=c(1.2,0)), 
                        left=textGrob("Longitude", rot =90, hjust=0.5,vjust=1.5),widths = c(1, 1, 0.2), heights=c(0.5,0.5))
