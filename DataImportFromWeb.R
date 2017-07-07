# Extracting data from the webpage using readHTMLTable of XML package since the data is available
# clearly on the HTML page

install.packages("XML")

require(XML)
sp.url<- "http://wiki.stat.ucla.edu/socr/index.php/SOCR_Data_Dinov_091609_SnP_HomePriceIndex"
sp.extractedData<- readHTMLTable(sp.url, which =1, header= TRUE, stringAsFactors= FALSE)

# checking the heads and tails of the output dataset.
head(sp.extractedData)
tail(sp.extractedData)

# checking the dimension of the dataset
dim(sp.extractedData)

#checking the class of dataset, should be a dataframe
class(sp.extractedData)

# checking the variable names of the dataset
names(sp.extractedData)

#renaming the last column since it has special character \n
colnames(sp.extractedData)[23]<- "Composite-10"

# returns string w/o leading whitespace
#trim.leading <- function (x)  sub("^\\s+", "", x)
# returns string w/o trailing whitespace
#trim.trailing <- function (x) sub("\\s+$", "", x)


#function to remove leading and trailing spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#storing old names to a variables
sp.oldName<- names(sp.dataNew)
sp.oldName
#trimming the old names and creating new 
sp.newName<- trim(sp.oldName)
sp.newName

#replacing dataset with new variable names
colnames(sp.extractedData) <- sp.newName
names(sp.extractedData)

#identical(names(sp.dataNew), names(sp.extractedData))

#saving the dataframe in R format
save(sp.extractedData, file="sp.fullDataset")

# saving the dataset in csv format
write.table(sp.extractedData, file="fullDataset.csv", sep=",", row.names= FALSE)

####### Here I have cut out the last six columns may be needed later ############
sp.dataNew<- sp.extractedData[1:216, ]
tail(sp.dataNew)

#replacing dataset with new variable names
colnames(sp.dataNew) <- sp.newName
names(sp.dataNew)

# saving the new dataset in csv format
write.table(sp.dataNew, file="sixLessDataset.csv", sep=",", row.names= FALSE)

#check for missing values
any(is.na(sp.extractedData))
any(is.na(sp.dataNew))


#Basic Plots

require(ggplot2)

ggplot(sp.extractedData, aes(y=Year ,x=Month)) +geom_boxplot()


plot(sp.extractedData[,4], type="l", lwd=2, col="red", xlab="AZ-Phoenix", ylab="Month",
     MAIN="Line Plot", ylim=c(0,250))

#setting seed for reproducibility
set.seed(234)

creating new columns with random values betweeen 1 and 100
sp.extractedData$trialColumn1<- sample(100, size=nrow(sp.extractedData), replace=TRUE) 

sp.extractedData$trialColWithhyp<- sample(100, size=nrow(sp.extractedData), replace=TRUE) 

names(sp.extractedData)
head(sp.extractedData)

ggplot(sp.extractedData, aes(y= trialColumn1,x=Month)) +geom_boxplot()




warnings()








