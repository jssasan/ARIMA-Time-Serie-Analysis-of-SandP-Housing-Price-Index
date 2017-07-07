


##########################      diagnostic checks of the fitted model    ##############################

## vector to store the models by their names
model.fit.ByName<- c()

for(i in 1:20){  
  model.fit.ByName[i]<- paste("model.fit", city.name[i], sep=".")
  
}


##### Ljung-Box Test ##############
for(i in 1:20){  
  
  print(city.name[i])
  
  plot.dir<- paste(sp.LJungBoxTestPlots, city.name[i],sep="/")
  pdf(paste(plot.dir,".pdf",sep=""))
  par(mfrow = c(1,1))
  
  tsdiag(eval(parse(text=model.fit.ByName[i])))
  
  dev.off()
  
}

## qqNorm Plot on the residual values of the fitted model  ##########
for(i in 1:20){  
  
  print(city.name[i])
  
  plot.dir<- paste(sp.QQNormTestPlots, city.name[i],sep="/")
  pdf(paste(plot.dir,".pdf",sep=""))
  par(mfrow = c(1,1))
  
  
  qqnorm(eval(parse(text=model.fit.ByName[i]))$residuals, asp = 1)
  qqline(eval(parse(text=model.fit.ByName[i]))$residuals, asp=1)
  
  dev.off()
  
}




##########################      End of the diagnostic checks of the fitted model    ########################

