###### @Author: Jasmeet Singh Sasan  #######

################################ Prediction Plots   #######################################
###########################################################################################



#########################################################################################################
#### After some easy data reshaping in excel, we import the csv and plot curves for all regions    ###

sp.dfpredFinal.All<- read.table(paste(sp.R_Plot,"/CompleDataSetWithForecast.csv",sep=""), header=TRUE,sep=",", as.is= TRUE )
sp.dfpredFinal.All$Date<- as.Date(sp.dfpredFinal.All$Date,format = "%m/%d/%Y")
paste(sp.R_Plot,"/CompleDataSetWithForecast.csv",sep="")

################ this is individual line plotPrediction  of all cities togethe r#############

sp.DFlinePlot.pred<- melt(sp.dfpredFinal.All[,4:24], id="Date")
head(sp.DFlinePlot.pred)

sp.matrixNamePred<- names(sp.dfpredFinal.All[,4:24])
class(sp.matrixNamePred)
sp.matrixNamePred

plot.overall.pred<- ggplot(data=sp.DFlinePlot.pred,aes(x=Date,y=value, color=variable))

p1.pred<- plot.overall.pred + geom_line() + labs(x= "Year", y= "INDEX" ,title='An S&P/Case-Shiller Home Price Index Prediction')+
  theme(plot.title = element_text(size=20, face="bold", vjust=2))

colors.factor<- c("dodgerblue4", "darkolivegreen4","darkorchid3", "goldenrod1", "red","darkgreen","yellow","black","grey58","brown",
                  "azure","skyblue","yellowgreen","orange4","khaki4","magenta2","chocolate","deeppink4","seagreen1","tomato2")

p2.pred<- p1.pred+ guides(colour = guide_legend(override.aes = list(size=4)))+scale_color_manual(values=colors.factor)+theme(panel.grid.minor = element_line(colour="grey", size=0.5))

plot.dir.pred<- paste(sp.R_Plot, "OverAllPlot.Prediction",sep="/")
pdf(paste(plot.dir.pred,".pdf",sep=""))
p2.pred
dev.off()


################ this is individual line plot Prediction  of all cities together from year 2002 to 2010 #############

sp.DFlinePlotCut.pred<- melt(sp.dfpredFinal.All[145:240,4:24], id="Date")
head(sp.DFlinePlotCut.pred)

plot.overallCut.pred<- ggplot(data=sp.DFlinePlotCut.pred,aes(x=Date,y=value, color=variable))

p1.predCut<- plot.overallCut.pred + geom_line() + labs(x= "Year", y= "INDEX" ,title='An S&P/Case-Shiller Home Price Index Prediction')+
  theme(plot.title = element_text(size=20, face="bold", vjust=2))

colors.factor<- c("dodgerblue4", "darkolivegreen4","darkorchid3", "goldenrod1", "red","darkgreen","yellow","black","grey58","brown",
                  "azure","skyblue","yellowgreen","orange4","khaki4","magenta2","chocolate","deeppink4","seagreen1","tomato2")

p2.predCut<- p1.predCut+ guides(colour = guide_legend(override.aes = list(size=4)))+scale_color_manual(values=colors.factor)+theme(panel.grid.minor = element_line(colour="grey", size=0.5))

plot.dir.predCut<- paste(sp.R_Plot, "OverAllPlotCut.Prediction",sep="/")
pdf(paste(plot.dir.predCut,".pdf",sep=""))
p2.predCut
dev.off()



################ this is individual line plot Prediction  of all cities together from year Jul 2009 to  Dec 2010 #############

sp.DFlinePlotCut18.pred<- melt(sp.dfpredFinal.All[223:240,4:24], id="Date")
head(sp.DFlinePlotCut18.pred)


plot.overallCut18.pred<- ggplot(data=sp.DFlinePlotCut18.pred,aes(x=Date,y=value, color=variable))

p1.predCut18<- plot.overallCut18.pred + geom_line() + labs(x= "Year", y= "INDEX" ,title='An S&P/Case-Shiller Home Price Index Prediction')+
  theme(plot.title = element_text(size=20, face="bold", vjust=2))

colors.factor<- c("dodgerblue4", "darkolivegreen4","darkorchid3", "goldenrod1", "red","darkgreen","yellow","black","grey58","brown",
                  "azure","skyblue","yellowgreen","orange4","khaki4","magenta2","chocolate","deeppink4","seagreen1","tomato2")

p2.predCut18<- p1.predCut18+ guides(colour = guide_legend(override.aes = list(size=4)))+scale_color_manual(values=colors.factor)+theme(panel.grid.minor = element_line(colour="grey", size=0.5))

plot.dir.predCut18<- paste(sp.R_Plot, "OverAllPlotCut18.Prediction",sep="/")
pdf(paste(plot.dir.predCut18,".pdf",sep=""))
p2.predCut18
dev.off()




###########################################################################################
###########################################################################################










