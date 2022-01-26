# install.packages() and load the package
library(ggplot2)
library(reshape2)

#load Eigenvalues data
load(file="data/OBdata.RData")

time = paste(rep(1997:2005, each = 3), rep(c("-06-15", "-07-15", "-08-15"), 9), sep="")
colnames(dato) <- c("StationName","GridID","lat","lon",time)
save(dato, file = "OBdata.RData")

# plot observed data
obdata<-t(dato[5:31])
colnames(obdata) <- dato[,1]
obdata1<-data.frame(obdata)
obdata1 <- cbind(rownames(obdata1), data.frame(obdata1, row.names=NULL))
colnames(obdata1)[1]<-"Time"
obdata1 <- melt(obdata1, id.vars="Time")
obdata1$Time <- as.character(obdata1$Time)
obdata1$Time <- factor(obdata1$Time, levels = unique(obdata1$Time) )
ggplot(obdata1, aes(x=Time, value, col=variable)) + geom_point(size=2) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_color_manual(values = c("red", "orange", "green", "blue", "black", "purple",
                                "pink", "brown", "grey", "green4", "coral", "steelblue4",
                                "yellow4", "goldenrod4",
                                "yellow", "rosybrown"), label = dato[,1]) + ylim(-30, 10) + 
  labs(color="Station Name",y="Value",x="Time") +
  ggtitle("Original Observed Data") + theme(plot.title = element_text(hjust = 0.5,size=14, margin = margin(t = 0, r = 0, b = 10, l = 0)),
                                            axis.title=element_text(size=13),
                                            axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
                                            axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)))
