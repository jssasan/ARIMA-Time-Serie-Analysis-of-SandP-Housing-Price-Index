require(Quandl)

install.packages("rvest")
require(rvest)



## api authorization
Quandl.api_key(my_api_key)


### extracting the entire database  : https://www.quandl.com/data/BLSI?keyword=
eco_leading_index_weekly_raw <- Quandl("ECRI/USLEADING", api_key=my_api_key)
head(eco_leading_index_weekly_raw)

"BLS_I/APU0000701111"
Quandl("BLSI/APU0000701111", api_key=my_api_key)

library(XML)


###########################       BLS Inflation & Prices        ################################  

the_URL <- "https://www.quandl.com/data/BLSI/documentation/documentation"

html_file <- read_html(the_URL) 

all_tables<-html_nodes(html_file, "table")

table_1 <- html_table(all_tables[3], fill = TRUE)
table_1 <- as.data.frame(table_1)

table_2 <- html_table(all_tables[5], fill = TRUE)
table_2 <- as.data.frame(table_2)


table_3 <- html_table(all_tables[7], fill = TRUE)
table_3 <- as.data.frame(table_3)

dim(table_2)


########################################################################################################################
########################################################################################################################
#                                                 table-1

# syntax  BLS_I/AP{SA}{AREA}{PROD}


seasonal_var <- c("U","S")

ac_name <-  table_1[4:8,2]  ## area name
prod_name <- table_1[10:nrow(table_1),2]   ## product name

ac_code <-  table_1[4:8,3]  ## area code
prod_code <- table_1[10:nrow(table_1),3]   ## product code

## creating empty dataset
table_1_code <- data.frame(dataset_name=character(0),Seasonal_adjusted= character(0) ,dataset_region= character(0) , product_name= character(0) ,dataset_code= character(0), stringsAsFactors = FALSE)

x = 1
  for(i in 1:2){
    for(j in 1:length(ac_code)){
      for(k in 1: length(prod_code)){
        
        table_1_code[x,1] <- paste0("BLSI/AP","_",seasonal_var[i],"_",ac_name[j],"_",prod_name[k])
        table_1_code[x,2] <- paste0(seasonal_var[i])
        table_1_code[x,3] <- paste0(ac_name[j])
        table_1_code[x,4] <- prod_name[k]
        table_1_code[x,5] <- paste0("BLSI/AP",seasonal_var[i],ac_code[j],prod_code[k])
        x= x+1
        
      }
    }
  }

str(table_1_code)


########################################################################################################################
########################################################################################################################
#                                               table-2



# syntax:  BLS_I/CU{SA}R0000S{PROD}


str(table_2)


seasonal_var <- c("U","S")


prod_name <- table_2[4:nrow(table_2),2]   ## product name

prod_code <- table_2[4:nrow(table_2),3]   ## product code

## creating empty dataset
table_2_code <- data.frame(dataset_name=character(0),Seasonal_adjusted= character(0) ,dataset_region= character(0) , product_name= character(0) ,dataset_code= character(0), stringsAsFactors = FALSE)

x = 1
for(i in 1:2){
    for(j in 1: length(prod_code)){
      
      table_2_code[x,1] <- paste0("BLSI/CU_",seasonal_var[i],"_",prod_name[j])
      table_2_code[x,2] <- paste0(seasonal_var[i])
      table_2_code[x,3] <- NA
      table_2_code[x,4] <- prod_name[k]
      table_2_code[x,5] <- paste0("BLSI/CU",seasonal_var[i],"R0000S",prod_code[j])
      x= x+1
      
    }
  }
str(table_2_code)
## example

Quandl("BLSI/CUSR0000SA0", api_key=my_api_key)




########################################################################################################################
########################################################################################################################
#                                               table-3


# Syntax : BLS_I/CW{SA}R{AREA}S{PROD}



seasonal_var <- c("U","S")

ac_name <-  table_3[4:8,2]  ## area name
prod_name <- table_3[10:nrow(table_3),2]   ## product name

ac_code <-  table_3[4:8,3]  ## area code
prod_code <- table_3[10:nrow(table_3),3]   ## product code

## creating empty dataset
table_3_code <- data.frame(dataset_name=character(0),Seasonal_adjusted= character(0) ,dataset_region= character(0) , product_name= character(0) ,dataset_code= character(0), stringsAsFactors = FALSE)

x = 1
for(i in 1:2){
  for(j in 1:length(ac_code)){
    for(k in 1: length(prod_code)){
      
      table_3_code[x,1] <- paste0("BLSI/CW","_",seasonal_var[i],"_R_",ac_name[j],"_",prod_name[k])
      table_3_code[x,2] <- paste0(seasonal_var[i])
      table_3_code[x,3] <- NA
      table_3_code[x,4] <- prod_name[k]
      table_3_code[x,5] <- paste0("BLSI/CW",seasonal_var[i],"R",ac_code[j],prod_code[k])
      x= x+1
      
    }
  }
}

str(table_3_code)

## example

temp<- head(Quandl("BLSI/CWUR0000SAE1", api_key=my_api_key))
class(temp$Value)
temp<- NULL

####################################################################################################################################################################
#################################                   rbind all tables of   BLS Inflation & Prices              #####################################################


BLSI_All_codes <- rbind(table_1_code,table_2_code, table_3_code)
BLSI_All_codes$Index <- paste0("BLSI_", 1:nrow(BLSI_All_codes)) ## this index can be mapped back to the actual dataset name later

head(BLSI_All_codes)
tail(BLSI_All_codes)
dim(BLSI_All_codes)

## created individual dataset for each code
sink("BLSI_Error_Code_v2.txt")

BLSI_df <- data.frame(dataset_name=character(0),Seasonal_adjusted= character(0) ,dataset_region= character(0) , product_name= character(0) ,dataset_code= character(0),Date= character(0), Value=numeric(0) , stringsAsFactors = FALSE)

for (i in 1:nrow(BLSI_All_codes)){
  
  tryCatch({
  
  # BLSI_All_DF[i] <- paste0(BLSI_All_codes[i,3])  
  # assign(paste0(BLSI_All_codes[i,6]), Quandl(BLSI_All_codes[i,5], api_key=my_api_key))
    
    temp<- cbind(BLSI_All_codes[i,1:6], Quandl(BLSI_All_codes[i,5], api_key=my_api_key))
    temp$Date <- as.character(temp$Date)
    BLSI_df<- rbind(BLSI_df, temp)
    rownames(BLSI_df) <- NULL
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), " - Error at index number: ", i  , "\n")})  ## trycatch ends here
  
}

sink()

dim(BLSI_df)
head(BLSI_df)
tail(BLSI_df)

BLSI_df$Date <- as.Date(BLSI_df$Date)
BLSI_df$Seasonal_adjusted <- as.factor(BLSI_df$Seasonal_adjusted)
BLSI_df$dataset_region <- as.factor(BLSI_df$dataset_region)
BLSI_df$product_name <- as.factor(BLSI_df$product_name)
BLSI_df$Index <- as.factor(BLSI_df$Index)
BLSI_df$dataset_code <- as.factor(BLSI_df$dataset_code)

str(BLSI_df)

save.image()

write.csv(file = "BLS_Inflation_Prices.csv",x = BLSI_df)  

