

###################  Final parameters (p,d,q) for the prediction  ##################

order.arima.fit

#### this can be further improved given more time  #####

############################################################################    





########################################################################################################################################################
###############################      FORECASTING        ################################################################################################    
########################################################################################################################################################


#####################   FORECASTING USING THE MODEL FIT FOR NEXT 18 MONTHS    #######################

for(j in 1:20){ 
  print("")
  print(paste("   Forecasting of ", city.name[j], " for  ", order.arima.fit[j]," and Statistics are shown below:"))
  print("------------------------------------------------------------------")
  pred.SPIndex.Name<- paste("pred.SPIndex",city.name[j], sep=".")
  #assign(pred.SPIndex.Name,   predict(eval(parse(text=model.fit.ByName[j])), n.ahead=18, se.fit = TRUE))
  assign(pred.SPIndex.Name,   forecast(eval(parse(text=model.fit.ByName[j])), h=18))
  print(eval(parse(text=pred.SPIndex.Name)))
  print("#####################################################################")
}


pred.SPIndex.AZ_Phoenix


##############################################################################
########################### plotting the prediction   ##########################
##############################################################################

for(i in 1:20){  
  plot.dir<- paste(sp.PredictionsPlots, city.name[i],sep="/")
  pdf(paste(plot.dir,".pdf",sep=""))
  print(city.name[i])
  
  plot(eval(parse(text=paste("pred.SPIndex", city.name[i],sep="."))), 
       main=paste(city.name[i],": 18 Months Forecasting") , xlab = "Year", ylab = "Index", ylim = c(50,275),
       panel.first=grid(nx=4,ny=10, col="red",lty = "dotted"))
  
  dev.off()
}



##############################################################################
########################### Exporting the prediction   ##########################
##############################################################################



pred.SPIndex.JustName<-c()
for(i in 1:20){
  
  pred.SPIndex.JustName[i]<- paste("pred.SPIndex",city.name[i], sep=".")
}

#pred.SPIndex.JustName[1]


trial.df00<- as.data.frame(c())
for(i in 1:20){    
  
  trial.df0<- as.data.frame(t(tapply(eval(parse(text=pred.SPIndex.JustName[i]))$mean, 
                                     list(year = floor(time(eval(parse(text=pred.SPIndex.JustName[i]))$mean)), 
                                          month = month.abb[cycle(eval(parse(text=pred.SPIndex.JustName[i]))$mean)]),c)))
  
  trial.df0$Region<- city.name[i]
  
  trial.df00<- rbind(trial.df0,trial.df00)
}

tail(trial.df00)

# saving the dataset in csv format
write.table(trial.df00, file="PredictedValues.csv", sep=",")




###########################################################################################
###########################################################################################