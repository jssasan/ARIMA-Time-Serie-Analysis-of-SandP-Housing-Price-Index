#####   @Author: Jasmeet Singh Sasan #####



######## Here the maion goal is to decompose the time series into its components: trend, seasonality and remainder ##########

sp.dfAll<- sp.dsFinal[,4:23]

## creating individual time series for every city
for(x in 1:20){
  
  assign(paste(city.name[x],"AllRows",sep="."), sp.dfAll[x])
  
}

####  creating time-series objects vector to hold all Actual value series #######
ts.All<- c()

for(i in 1:20){    
  ts.All[i]<- paste(city.name[i],"tsAllRows",sep=".")
  
}


for(x in 1:20){
  
  assign(paste(ts.All[x]), ts(eval(parse(text=paste(city.name[x],"AllRows",sep=".")))[,1], start=1991, freq=12))
  
}

### Function to call time series by  name with its index ###
timeSeriesAllNum <- function(x)(eval(parse(text=ts.All[x])))


#####  Plotting all the decomposition for Actual Values including all the years ########
par(mar=c(1,1,1,1))

for(x in 1:20){   
  
  print(city.name[x])
  
  plot.dir<- paste(sp.ActualAllRowPlot, city.name[x],sep="/")
  pdf(paste(plot.dir,".pdf",sep=""))
  # par(mfrow = c(1,1))  
  
  plot(stl(timeSeriesAllNum(x), s.window = "periodic"), main=paste(" Decomposition plot of ", city.name[x]))
  ## plot(stl(log(timeSeriesAllNum(1)), s.window = "periodic"), main=paste(" Decomposition plot (log) of ", city.name[1]))
  
  dev.off()
  
}


#####  Plotting all the decomposition for 1st order differencing Values including all the years ########
for(x in 1:20){   
  
  print(city.name[x])
  
  plot.dir<- paste(sp.Diff1AllRowsPlot, city.name[x],sep="/")
  pdf(paste(plot.dir,".pdf",sep=""))
  # par(mfrow = c(1,1))  
  
  plot(stl(diff(timeSeriesAllNum(x)), s.window = "periodic"), main=paste("1st Order Differencing Decomposition plot of ", city.name[x]))
  ## plot(stl(log(timeSeriesAllNum(1)), s.window = "periodic"), main=paste(" Decomposition plot (log) of ", city.name[1]))
  
  dev.off()
  
}

#####  Plotting all the decomposition for 2nst order differencing Values including all the years ########
for(x in 1:20){   
  
  print(city.name[x])
  
  plot.dir<- paste(sp.Diff2AllRowsPlot, city.name[x],sep="/")
  pdf(paste(plot.dir,".pdf",sep=""))
  # par(mfrow = c(1,1))  
  
  plot(stl(diff(diff(timeSeriesAllNum(x))), s.window = "periodic"), main=paste("2nd Order Differencing Decomposition plot of ", city.name[x]))
  ## plot(stl(log(timeSeriesAllNum(1)), s.window = "periodic"), main=paste(" Decomposition plot (log) of ", city.name[1]))
  
  dev.off()
  
}
######################################################################################################################
######################################################################################################################


