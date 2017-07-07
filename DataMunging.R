sp.RootDir<- "C:/Users/jasmeet/Desktop/R/HousePriceIndex_Forecast"


#####   @Author: Jasmeet Singh Sasan #####


############# Creating Root Directories    ##################
dir.R_Plot<- paste(sp.RootDir, "/R_Plot", sep="")
dir.create(dir.R_Plot)

dir.Auto.Arima <- paste(dir.R_Plot, "/Auto.Arima", sep="")
dir.create(dir.Auto.Arima)

dir.Initial_ACF_PACF <- paste(dir.R_Plot, "/Initial_ACF_PACF", sep="")
dir.create(dir.Initial_ACF_PACF)

dir.SeriesDecomposition<- paste(dir.R_Plot, "/SeriesDecomposition", sep="")
dir.create(dir.SeriesDecomposition)

dir.DiagnosticPlots<- paste(dir.R_Plot, "/DiagnosticPlots", sep="")
dir.create(dir.DiagnosticPlots)

dir.PredictionPlots <- paste(dir.R_Plot, "/PredictionPlots", sep="")
dir.create(dir.PredictionPlots)


## creating child directories
dir.AutoArimaPrediction<- paste(dir.Auto.Arima, "/AutoArimaPrediction", sep="")
dir.create(dir.AutoArimaPrediction)

dir.qqNormTest<- paste(dir.DiagnosticPlots, "/qqNormTest", sep="")
dir.create(dir.qqNormTest)

dir.Box_LjungTestt<- paste(dir.DiagnosticPlots, "/Residual_ACF_Box_LjungTest", sep="")
dir.create(dir.Box_LjungTestt)

dir.ActualValAllRows <- paste(dir.SeriesDecomposition, "/ActualValAllRows", sep="")
dir.create(dir.ActualValAllRows)

dir.Diff1ValAllRows <- paste(dir.SeriesDecomposition, "/Diff1ValAllRows", sep="")
dir.create(dir.Diff1ValAllRows)

dir.Diff2ValAllRows<- paste(dir.SeriesDecomposition, "/Diff2ValAllRows", sep="")
dir.create(dir.Diff2ValAllRows)

#### creating variables for respective directory to store plots  ###

sp.R_Plot<-     paste(sp.RootDir,"/R_Plot", sep="")                                   
sp.ACFPACFPlot<-   paste(sp.R_Plot,"/Initial_ACF_PACF", sep="")                         
sp.AutoArimaPlot<-  paste(sp.R_Plot,"/Auto.Arima", sep="")                             
sp.AutoArimaPredPlot<- paste(sp.AutoArimaPlot,"/AutoArimaPrediction", sep="")
sp.SeriesSplitRoot<-   paste(sp.R_Plot,"/SeriesDecomposition", sep="")                 
sp.ActualAllRowPlot<-  paste(sp.SeriesSplitRoot,"/ActualValAllRows", sep="")          
sp.Diff1AllRowsPlot<-  paste(sp.SeriesSplitRoot,"/Diff1ValAllRows", sep="")           
sp.Diff2AllRowsPlot<-   paste(sp.SeriesSplitRoot,"/Diff2ValAllRows", sep="")          
sp.DiagnosticRoot<-      paste(sp.R_Plot,"/DiagnosticPlots", sep="")                  
sp.LJungBoxTestPlots<-    paste(sp.DiagnosticRoot,"/Residual_ACF_Box_LjungTest", sep="")
sp.QQNormTestPlots<-       paste(sp.DiagnosticRoot,"/qqNormTest", sep="")               
sp.PredictionsPlots<-       paste(sp.R_Plot,"/PredictionPlots", sep="")


install.packages("XML")
require(XML)

sp.url<- "http://wiki.stat.ucla.edu/socr/index.php/SOCR_Data_Dinov_091609_SnP_HomePriceIndex"
sp.extractedData<- readHTMLTable(sp.url, which =1, header= TRUE, stringAsFactors= FALSE)

head(sp.extractedData)
tail(sp.extractedData)
dim(sp.extractedData)
class(sp.extractedData)
names(sp.extractedData)

#renaming the column, unwanted characters
colnames(sp.extractedData)[23]<- "Composite-10"

#function to get rid off leading and trailing spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
sp.oldName<- names(sp.extractedData)
sp.newName0<- trim(sp.oldName)

#function to replace hyphens from variable names with _
repHyp <- function (x) gsub("\\-", "_", x)
sp.newName<- repHyp(sp.newName0)

#replacing dataset with new variable names
colnames(sp.extractedData) <- sp.newName
names(sp.extractedData)

#Function to convert character value to numeric
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}


# creating new data set with numeric values-- Note: all variables are numeric
sp.ds <- data.frame(lapply(sp.extractedData, as.numeric.factor), stringsAsFactors=FALSE)
any(is.na(sp.ds))

#converting numeric to factors (categorical variables)
sp.ds$Month<- as.factor(sp.ds$Month)
sp.ds$Year<- as.factor(sp.ds$Year)

#getting the values from older dataset
sp.ds$Month<- sp.extractedData$Month

#Creating an ordered factor of Months
sp.dsFinal<- sp.ds

factorMon<-c("January","February","March","April","May","June","July","August","September","October","November","December")
factorMon

sp.dsFinal$Month<- factor(sp.ds$Month, levels=factorMon,ordered=TRUE)
head(sp.dsFinal)

class(sp.ds$Month)
class(sp.dsFinal$Month)

all.equal(sp.ds[,1:23],sp.dsFinal[,1:23])


### creating a date variable from exiting month and year  ###
sp.dsFinal$Date<- paste(sp.dsFinal$Year,sp.dsFinal$Month, "01", sep="-")
sp.dsFinal$Date<-as.Date(sp.dsFinal$Date,"%Y-%B-%d") 
head(sp.dsFinal)

#class(sp.dsFinal$Date)

# removing unwanted datasets and variables
rm(sp.ds, sp.newName,sp.newName0,sp.oldName)

#saving the dataframe in R format
save(sp.dsFinal, file="sp.fullDataset")
# saving the dataset in csv format
write.table(sp.dsFinal, file="fullDataset.csv", sep=",", row.names= FALSE)

#check for missing values 
any(is.na(sp.dsFinal))

#checking the order of levels of the Month
levels(sp.dsFinal$Month)

######### Here ends  data cleaning and validation ###########



######################################################################################################################
######################################################################################################################

