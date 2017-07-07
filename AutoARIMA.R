#####   @Author: Jasmeet Singh Sasan #####


############################################################################################################################################################
############################################################################################################################################################



## creating dataset for forecasting from year 2003
## data set with no differencing
sp.dfcut0<- sp.dsFinal[145:222,4:23]
city.name<- names(sp.dfcut0)



## creating individual time series for every city
for(x in 1:20){
  
  assign(paste(city.name[x]), sp.dfcut0[x])
}


######## creating first and second order differencing of the actual time series    ########

#### creating two vectors for storing the outputs for first order ######

sp.dfcut1<- c()
for(i in 1:length(city.name)){
  
  sp.dfcut1[i]<- paste(city.name[i],"diff1",sep=".")
  
}

for(x in 1:20){
  
  assign(paste(sp.dfcut1[x]), diff((eval(parse(text = city.name[x])))[,1]))
}


#### creating  vectors for storing the outputs for second order ######
sp.dfcut2<- c()
for(i in 1:length(city.name)){
  
  sp.dfcut2[i]<- paste(city.name[i],"diff2",sep=".")
}

for(x in 1:20){
  
  assign(paste(sp.dfcut2[x]), diff(diff((eval(parse(text = city.name[x])))[,1])))
}

### 
(eval(parse(text = city.name[x])))[,1])


############################################################################################################
## checking normality and Stationarity of the data

print("############################################################################")
print("##################  shapiro Test of Stationary Starts Here   ###############")
print("############################################################################")

## creating a vector name for shapriro test which will hold all the values
city.shapiro<- c()
for(i in 1:length(city.name)){
  city.shapiro[i]<- paste(city.name[i],"shapTest",sep=".")
}

## automated the shapiro.test for all the 20 time series
for(x in 1:20){
  assign(paste(city.shapiro[x]), shapiro.test((eval(parse(text = city.name[x])))[,1]))
  print(city.name[x])
  print((eval(parse(text = city.shapiro[x]))))
  #print((eval(parse(text = city.shapiro[x])))$statistic)
  
}

print("############################################################################")
print("##################  shapiro Test of Stationary Ends Here   ###############")
print("############################################################################")





###   Kolmogorov-Smirnov test of Uniformity of data ####
####  dropped because of duplicate entries in the vector  ###
# for(x in 1:3){ 
#     ks.test((eval(parse(text = city.name[x])))[,1],
#        "pnorm", mean((eval(parse(text = city.name[x])))[,1]),
#        sd((eval(parse(text = city.name[x])))[,1]))
# }


### Dickey-Fuller Test

print("############################################################################")
print("##################  Dickey Fuller Test of Stationary Starts Here   #########")
print("############################################################################")

install.packages("tseries")
require(tseries)


for(x in 1:20){  
  
  print(city.name[x]) 
  print(adf.test(eval(parse(text= sp.dfcut0[x])), alternative="stationary"))
  
  print(city.name[x])
  print(adf.test(eval(parse(text= sp.dfcut1[x])), alternative="stationary"))
  
  print(city.name[x])
  print(adf.test(eval(parse(text= sp.dfcut2[x])), alternative="stationary"))
  
}

print("############################################################################")
print("################   Dickey Fuller Test of Stationary Ends Here     ##########")
print("############################################################################")



### Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test

print("############################################################################")
print("# Kwiatkowski-Phillips-Schmidt-Shin (KPSS)Test of Stationary Starts Here   #")
print("############################################################################")


for(x in 1:20){  
  
  print(city.name[x]) 
  print(kpss.test(eval(parse(text= sp.dfcut0[x]))))
  
  print(city.name[x])
  print(kpss.test(eval(parse(text= sp.dfcut1[x]))))
  
  print(city.name[x])
  print(kpss.test(eval(parse(text= sp.dfcut2[x]))))
  
}

print("############################################################################")
print("# Kwiatkowski-Phillips-Schmidt-Shin (KPSS)Test of Stationary Ends Here     #")
print("############################################################################")




### Box Test would be used to validate te model's accuracy
## Box.test(actual,lag=5, type="Ljung-Box")
## Box.test(actual, lag=2, type="Box-Pierce")




print("############################################################################")
print("################## ACF- PACF Test of Stationary Starts Here     ##############")
print("############################################################################")

### check for acf and pacf for all Time Series of Actual Values, First Order and Second Order Differencing  ###


for(x in 1:20){  
  
  print(city.name[x])
  
  plot.dir<- paste(sp.ACFPACFPlot, city.name[x],sep="/")
  pdf(paste(plot.dir,".pdf",sep=""))
  par(mfrow = c(3,2))
  
  plot(acf(eval(parse(text= sp.dfcut0[x])), lag.max = 60, plot = FALSE), main=paste(city.name[x], "ACF Actual Values", sep=" - "))
  plot(pacf(eval(parse(text= sp.dfcut0[x])), lag.max = 60, plot = FALSE), main=paste(city.name[x], "PACF Actual Values", sep=" - "))
  
  plot(acf( eval(parse(text= sp.dfcut1[x])), lag.max = 60, plot = FALSE), main=paste(city.name[x], "ACF First Order Values", sep=" - "))
  plot(pacf(eval(parse(text= sp.dfcut1[x])), lag.max = 60, plot = FALSE), main=paste(city.name[x], "PACF First Order Values", sep=" - "))
  
  plot(acf(eval(parse(text= sp.dfcut2[x])), lag.max = 60, plot = FALSE), main=paste(city.name[x], "ACF Second Order Values", sep=" - "))
  plot(pacf(eval(parse(text= sp.dfcut2[x])), lag.max = 60, plot = FALSE), main=paste(city.name[x], "PACF Second Order Values", sep=" - "))
  
  dev.off()
  
}


print("############################################################################")
print("################## ACF- PACF Test of Stationary Ends Here     ##############")
print("############################################################################")





######################################################################################################################
######################################################################################################################

require(forecast)

#### using ndiffs function of forecast to determine what order differencing is required for every time series #####

required.diffs<- c()
print("Order of differencing required for each series is given below")
for(x in 1:20){  
  
  required.diffs[x]<- ndiffs(x=eval(parse(text=city.name[x]))[,1])
  print(paste(city.name[x], required.diffs[x], sep= "--------------"))
}

########  the differncing results are confirmed with the ACF and PACF plots ###########



###########   auto.arima for every time series  ##########

## creating a list with city names for auto arima which will hold all the values
model.autoarima<- c()
for(i in 1:length(city.name)){
  model.autoarima[i]<- paste(city.name[i],"autoarima",sep=".")
}

model.autoarima[1]

for(x in 1:20){
  
  print("---------------------------------------------------------------")
  assign(paste(model.autoarima[x]), auto.arima((eval(parse(text = city.name[x])))[,1]))
  print(paste((city.name[x]), "Auto Arima Statistics are given below", sep="---------"))
  print("----------------------------------------------------------------")
  print("----------------------------------------------------------------")
  print((eval(parse(text = model.autoarima[x]))))
  print("----------------------------------------------------------------")
  #print((eval(parse(text = city.autoarima[x])))$statistic)
  
}

###########   Residuals of fitted auto.arima Model for every time series  ##########

###  (eval(parse(text = city.autoarima[x])))$residual

par(mar=c(1,1,1,1))

for(x in 1:20){  
  
  print(city.name[x])
  
  plot.dir<- paste(sp.AutoArimaPlot, city.name[x],sep="/")
  pdf(paste(plot.dir,".pdf",sep=""))
  par(mfrow = c(2,1))
  
  plot(acf(eval(parse(text = model.autoarima[x]))$residual, lag.max = 60, plot = FALSE), main=paste(city.name[x], "ACF of Residual of Auto Arima", sep=" - "))
  plot(pacf(eval(parse(text = model.autoarima[x]))$residual, lag.max = 60, plot = FALSE), main=paste(city.name[x], "PACF of Residual of Auto Arima", sep=" - "))
  
  
  dev.off()
  
}

####### Even though the ACF and PACF plots are off, still interested to see forecasted values    #######

##############   Predictions based on auto.arima Model for every Time Series  ###############

prediction.autoarima<- c()

for(i in 1:length(city.name)){
  
  prediction.autoarima[i]<- paste(city.name[i], "autoarima.pred",sep= ".")
  
}

prediction.autoarima

require(forecast)

for(x in 1:20){
  
  print("------------------------------------------------------------------------------")
  assign(paste(prediction.autoarima[x]), forecast((eval(parse(text = model.autoarima[x]))), h = 18))
  print(paste((city.name[x]), "Auto Arima Prediction Statistics are given below", sep="---------"))
  print("------------------------------------------------------------------------------")
  print("------------------------------------------------------------------------------")
  print((eval(parse(text = prediction.autoarima[x]))))
  print("------------------------------------------------------------------------------")
  
  
}



##############################################################################
########################### plotting the prediction   ##########################
##############################################################################

for(i in 1:20){  
  plot.dir<- paste(sp.AutoArimaPredPlot, city.name[i],sep="/")
  pdf(paste(plot.dir,".pdf",sep=""))
  print(city.name[i])
  
  plot((eval(parse(text = prediction.autoarima[i]))), 
       main=paste(city.name[i],": 18 Months Forecasting") , xlab = "Year", ylab = "Index", ylim = c(50,275),
       panel.first=grid(nx=4,ny=10, col="red",lty = "dotted"))
  
  dev.off()
}


######################################################################################################################
######################################################################################################################


