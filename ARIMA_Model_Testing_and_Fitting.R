


################################################     ARIMA MODEL Testing Starts here     #############################
######################################################################################################################


####  creating time-series objects vector to hold all Actual value series starting with year 2003#######
ts.arma03<- c()

for(i in 1:20){    
  ts.arma03[i]<- paste(city.name[i],"ts03",sep=".")
  
}

###ts.arma03

for(x in 1:20){
  
  assign(paste(ts.arma03[x]), ts(eval(parse(text=paste(city.name[x],"AllRows",sep=".")))[145:222,1], start=2003, freq=12))
  
}

### Function to call time series by  name with its index ###
timeSeriesARIMA03 <- function(x)(eval(parse(text=ts.arma03[x])))



## List of arima orders i.e. value of p,d,q to be modelled/fit -- differencing order 1 and order 2
order.arima.diff1<-list(c(0,1,0),c(0,1,1),c(0,1,2),c(0,1,3),c(1,1,0),c(1,1,1),c(1,1,2),c(1,1,3),c(2,1,0),c(2,1,1),c(2,1,2),c(2,1,3),c(3,1,0),c(3,1,1),c(3,1,2),c(3,1,3))

order.arima.diff2<- list(c(0,2,0),c(0,2,1),c(0,2,2),c(0,2,3),c(1,2,0),c(1,2,1),c(1,2,2),c(1,2,3),c(2,2,0),c(2,2,1),c(2,2,2),c(2,2,3),c(3,2,0),c(3,2,1),c(3,2,2),c(3,2,3))


##### Loop Over to generate model for first 4 cities #########
for(i in 1:4){     
  
  for(j in 1:length(order.arima.diff2)){ 
    
    print(paste("   (p,d,q) parameter for ", city.name[i], " is  ", order.arima.diff2[j]," and Statistics are shown below:"))
    print("------------------------------------------------------------------")
    print( arima(x=timeSeriesARIMA03(i),order=eval(parse(text= order.arima.diff2[j]))))
    
    print("#####################################################################")
  }
}

##### Loop Over to generate model forindex 5 city i.e. Denver since it needs only 1st order differencing #########

for(j in 1:length(order.arima.diff1)){ 
  
  print(paste("   (p,d,q) parameter for ", city.name[5], " is  ", order.arima.diff1[j]," and Statistics are shown below:"))
  print("------------------------------------------------------------------")
  print( arima(x=timeSeriesARIMA03(5),order=eval(parse(text= order.arima.diff1[j]))))
  
  print("#####################################################################")
}


##### Loop Over to generate model for index 6 to 16 cities #########
for(i in 6:8){     
  
  for(j in 1:length(order.arima.diff2)){ 
    
    print(paste("   (p,d,q) parameter for ", city.name[i], " is  ", order.arima.diff2[j]," and Statistics are shown below:"))
    print("------------------------------------------------------------------")
    print( arima(x=timeSeriesARIMA03(i),order=eval(parse(text= order.arima.diff2[j]))))
    
    print("#####################################################################")
  }
}


##### Loop Over to generate model for index 9 city i.e. Atlanta since it needs only 1st order differencing #########
for(j in 1:length(order.arima.diff1)){ 
  
  print(paste("   (p,d,q) parameter for ", city.name[9], " is  ", order.arima.diff1[j]," and Statistics are shown below:"))
  print("------------------------------------------------------------------")
  print( arima(x=timeSeriesARIMA03(9),order=eval(parse(text= order.arima.diff1[j]))))
  
  print("#####################################################################")
}

for(i in 10:12){     
  
  for(j in 1:length(order.arima.diff2)){ 
    
    print(paste("   (p,d,q) parameter for ", city.name[i], " is  ", order.arima.diff2[j]," and Statistics are shown below:"))
    print("------------------------------------------------------------------")
    print( arima(x=timeSeriesARIMA03(i),order=eval(parse(text= order.arima.diff2[j]))))
    
    print("#####################################################################")
  }
}

for(i in 13:16){     
  
  for(j in 1:length(order.arima.diff2)){ 
    
    print(paste("   (p,d,q) parameter for ", city.name[i], " is  ", order.arima.diff2[j]," and Statistics are shown below:"))
    print("------------------------------------------------------------------")
    print( arima(x=timeSeriesARIMA03(i),order=eval(parse(text= order.arima.diff2[j]))))
    
    print("#####################################################################")
  }
}

##### Loop Over to generate model for index 17 city i.e. Cleavland since it needs only 1st order differencing #########

for(j in 1:length(order.arima.diff1)){ 
  
  print(paste("   (p,d,q) parameter for ", city.name[17], " is  ", order.arima.diff1[j]," and Statistics are shown below:"))
  print("------------------------------------------------------------------")
  print( arima(x=timeSeriesARIMA03(17),order=eval(parse(text= order.arima.diff1[j]))))
  
  print("#####################################################################")
}

##### Loop Over to generate model for index 18 city #################

for(j in 1:length(order.arima.diff2)){ 
  
  print(paste("   (p,d,q) parameter for ", city.name[18], " is  ", order.arima.diff2[j]," and Statistics are shown below:"))
  print("------------------------------------------------------------------")
  print( arima(x=timeSeriesARIMA03(18),order=eval(parse(text= order.arima.diff2[j]))))
  
  print("#####################################################################")
}


##### Loop Over to generate model for index 19 city i.e. Seattle since it needs only 1st order differencing #########

for(j in 1:length(order.arima.diff2)){ 
  
  print(paste("   (p,d,q) parameter for ", city.name[19], " is  ", order.arima.diff2[j]," and Statistics are shown below:"))
  print("------------------------------------------------------------------")
  print( arima(x=timeSeriesARIMA03(19),order=eval(parse(text= order.arima.diff2[j]))))
  
  print("#####################################################################")
}


##### Loop Over to generate model for index 20- city

for(j in 1:length(order.arima.diff2)){ 
  
  print(paste("   (p,d,q) parameter for ", city.name[20], " is  ", order.arima.diff2[j]," and Statistics are shown below:"))
  print("------------------------------------------------------------------")
  print( arima(x=timeSeriesARIMA03(20),order=eval(parse(text= order.arima.diff2[j]))))
  
  print("#####################################################################")
}

#####################################   Model Testing Ends Here    #################################################################
##########################################################################################################################################




################################################     ARIMA MODEL Fitting Starts here     #############################
######################################################################################################################

###### final orders of all models (cities)  #############
order.arima.fit<- list(c(3,2,2),c(3,2,2),c(3,2,2),c(1,2,0),c(3,1,3),c(3,2,1),c(1,2,0),c(1,2,0),c(3,1,0),c(2,2,3),c(1,2,3),c(1,2,3),c(1,2,2),c(1,2,3),c(1,2,2),c(1,2,3),c(1,1,3),c(3,2,3),c(0,2,3),c(3,2,2))

length(order.arima.fit)

### individual models have been generated with names as e.g. model.fit.AZ_Phoenix
for(j in 1:20){ 
  print("")
  print(paste("   (p,d,q) parameter for ", city.name[j], " is  ", order.arima.fit[j]," and Statistics are shown below:"))
  print("------------------------------------------------------------------")
  model.fit.name<- paste("model.fit",city.name[j], sep=".")
  assign(model.fit.name,  arima(x=timeSeriesARIMA03(j),order=eval(parse(text= order.arima.fit[j]))))
  print(eval(parse(text=model.fit.name)))
  print("#####################################################################")
}





###############################################     ARIMA MODEL Fitting Ends here     #############################
######################################################################################################################

