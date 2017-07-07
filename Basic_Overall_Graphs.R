
#sessionInfo()

#Basic Over all Plots
install.packages("ggplot2")
require(reshape2)
require(ggplot2)

install.packages("ggthemes")
library(ggthemes)


ggplot(sp.dsFinal, aes(y=AZ_Phoenix,x=Date)) +geom_line()

### this is individual line plot of all cities together
## take its output image 

sp.DFlinePlot<- melt(sp.dsFinal[,4:24], id="Date")
head(sp.DFlinePlot)

sp.matrixName<- names(sp.dsFinal[,4:24])
class(sp.matrixName)
sp.matrixName

plot.overall<- ggplot(data=sp.DFlinePlot,aes(x=Date,y=value, color=variable))

p1<- plot.overall + geom_line() + labs(x= "Year", y= "INDEX" ,title='S&P/Case-Shiller Home Price Index')+
  theme(plot.title = element_text(size=20, face="bold", vjust=2))

colors.factor<- c("dodgerblue4", "darkolivegreen4","darkorchid3", "goldenrod1", "red","darkgreen","yellow","black","grey58","brown",
                  "azure","skyblue","yellowgreen","orange4","khaki4","magenta2","chocolate","deeppink4","seagreen1","tomato2")

p2<- p1+ guides(colour = guide_legend(override.aes = list(size=4)))+scale_color_manual(values=colors.factor)+theme(panel.grid.minor = element_line(colour="grey", size=0.5))

plot.dir<- paste(sp.R_Plot, "OverAllPlot",sep="/")
pdf(paste(plot.dir,".pdf",sep=""))
p2
dev.off()


######################################################################################################################
######################################################################################################################






#############################################   Start OF mvtsplot   ##################################################


install.packages("mvtsplot")
require(mvtsplot)

## creating a datafrmae containig time-series for all the cities
sp.matrix<- sp.dsFinal[,4:23]
head(sp.matrix)

### this package is used just for the plotting purpose    ######

####  this is telling me when housing bubblt started building. It depicts the a trend of downfall of index 
## Setting norm as global would normalize the data wrt all data series. Note: This is not correlation with other series

## box plot on Right Hand panel confirms abscence of outliers in the data
## as we increase the number of levels we can more clearly identify the index change
## and this is possible since out data is not sparse and pretty smooth

## here individual time series for all states are depicted that means index is categorized
## based on  individual time series. It clearly shows the overall index level of every City

plot.dir<- paste(sp.R_Plot, "VisualAll",sep="/")
pdf(paste(plot.dir,".pdf",sep=""))

par(mfrow = c(2,1))

mvtsplot(sp.matrix, norm="global",  level= 3,
         margin = TRUE, rowstat = "mean", main="Multi- Region time series plot discretized in LOW(purple), MEDIUM(grey) and HIGH(green) category.")

mvtsplot(sp.matrix, norm="global",  level= 5,
         margin = TRUE,rowstat = "mean", main="Multi- Region time series plot discretized in LOW(purple), MEDIUM(grey) and HIGH(green) category." )

dev.off()

#############################################   END OF mvtsplot   ##################################################


