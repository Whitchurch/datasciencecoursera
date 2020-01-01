dataSet <- read.csv("Data/hw1_data.csv")
## 11: Print Dataframe headings
head(dataSet)

## 12: Extract first 2 rows
head(dataSet, n = 2)

## 13: How many rows in the data frame
nrow(dataSet)

## 14: last 2 rows in data frame
tail(dataSet, n = 2)

## 15: value of Ozone in the 47th row
dataSet$Ozone[47]

## 16: Missing values in Ozone

TotalMissingOzoneNAs <- sum(as.integer(is.na(dataSet$Ozone)))
TotalMissingOzoneNAs

##17: what is the mean of the Ozone column, exclude the NAs (Wrong approach)
copyofOzoneColumn <- dataSet$Ozone # Make a copy to manipulate
copyofOzoneColumn # print the copy
copyofOzoneColumn[is.na(copyofOzoneColumn)]<- 0 # Set NAs to 0 
copyofOzoneColumn # print for sanity check

MeanOzone <- mean(copyofOzoneColumn) #Calculate the mean
MeanOzone # print result

summary(copyofOzoneColumn) # sanity check result

#just exclude without cleanup to do the calculations <- This is the correnct approach
OzoneNAMAsk <- is.na(dataSet$Ozone)
OzoneafterMasking <- dataSet[!OzoneNAMAsk,"Ozone"]
OzoneafterMasking
x <- mean(OzoneafterMasking)
## Extract rows of DF, where Ozone  > 31 and Temp > 90. What is the mean of Solar.R in this subset
dataSet #print the dataset

#Check is rows of interest have NAs
TotalMissingOzoneNAs <- sum(as.integer(is.na(dataSet$Ozone)))
TotalMissingOzoneNAs

TotalMissingTempNAs <- sum(as.integer(is.na(dataSet$Temp)))
TotalMissingTempNAs

#Do cleanup of Ozone values
copyofOzoneColumn <- dataSet$Ozone # Make a copy to manipulate
copyofOzoneColumn # print the copy
copyofOzoneColumn[is.na(copyofOzoneColumn)]<- 0 # Set NAs to 0 
cleaned <- copyofOzoneColumn
cleaned
# Add new coulumn to the table
dataSet$OzoneCleaned <- cleaned

result <- dataSet$OzoneCleaned > 31 & dataSet$Temp > 90 # get the true false mask

SolarResult <- dataSet[result,"Solar.R"] # index rows based on the true false mask vector and then select Solar.R

meanSolarR <- mean(SolarResult)

meanSolarR # print result


# 19: Mean of Temp , when month equals 6
dataSet
anyNAs <- sum(is.na(dataSet$Month)) # check for NAs
anyNAs

selectMonthVec <- dataSet$Month == 6

TempResult <- dataSet[selectMonthVec,"Temp"]
meanTempResult <- mean(TempResult)
meanTempResult

#20 Maximum ozone value in the month of May 
selectMonthVec <- dataSet$Month == 5
OzoneValueResult <- dataSet[selectMonthVec,"OzoneCleaned"]
meanOzoneValueResult <- max(OzoneValueResult)
meanOzoneValueResult