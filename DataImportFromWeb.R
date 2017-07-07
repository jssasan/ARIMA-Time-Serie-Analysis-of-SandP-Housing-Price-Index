# Extracting data from the webpage using readHTMLTable of XML package since the data is available
# clearly on the HTML page

install.packages("XML")
require(XML)

sp.url<- "http://wiki.stat.ucla.edu/socr/index.php/SOCR_Data_Dinov_091609_SnP_HomePriceIndex"
sp.extractedData<- readHTMLTable(sp.url, which =1, header= TRUE, stringAsFactors= FALSE)

head(sp.extractedData)
tail(sp.extractedData)
dim(sp.extractedData)
class(sp.extractedData)
names(sp.extractedData)

colnames(sp.extractedData)[23]<- "Composite-10"

#function to get rid off leading and trailing spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
sp.oldName<- names(sp.dataNew)
sp.newName<- trim(sp.oldName)

#replacing dataset with new variable names
colnames(sp.extractedData) <- sp.newName
names(sp.extractedData)

#function to replace hyphens from variable names with _
repHyp <- function (x) gsub("\\-", "_", x)
sp.oldName1<- names(sp.extractedData)
sp.newName1<- repHyp(sp.oldName1)
#replacing dataset with new variable names
colnames(sp.extractedData) <- sp.newName1
names(sp.dataNew)


#saving the dataframe in R format
save(sp.extractedData, file="sp.fullDataset")
# saving the dataset in csv format
write.table(sp.extractedData, file="fullDataset.csv", sep=",", row.names= FALSE)

#check for missing values
any(is.na(sp.extractedData))









