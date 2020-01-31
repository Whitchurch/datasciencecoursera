
#Read in the file:-

pathofinterest <- rstudioapi::getSourceEditorContext()$path
pathofinterest <- gsub("Script/Course4Week1.R","Data/household_power_consumption.txt",pathofinterest)

if(file.exists(pathofinterest))
{
  print("the file exists")
  
  readDataFrame <- read.csv(pathofinterest,header = TRUE,sep = ";")
  #Analyze DataFrame Structure
  names(readDataFrame)
  head(readDataFrame)
  dim(readDataFrame)
  # So we have 2075259 rows and 9 columns
  
  #Check for missing values:
  Date <- sum(as.integer(is.na(readDataFrame$Date))) # check column 1 - 9 for missing values:
  Time <-sum(as.integer(is.na(readDataFrame$Time)))
  GAP <-sum(as.integer(is.na(readDataFrame$Global_active_power)))
  GRP <-sum(as.integer(is.na(readDataFrame$Global_reactive_power)))
  Voltage <-sum(as.integer(is.na(readDataFrame$Voltage)))
  GI <-sum(as.integer(is.na(readDataFrame$Global_intensity)))
  SBM1 <-sum(as.integer(is.na(readDataFrame$Sub_metering_1)))
  SBM2 <-sum(as.integer(is.na(readDataFrame$Sub_metering_2)))
  SBM3 <-sum(as.integer(is.na(readDataFrame$Sub_metering_3))) # this one has 25979 missing values
  
  #To verify further and to ensure we have not missed anything
   complete <- readDataFrame[!complete.cases(readDataFrame), ]
   dim(complete) # we have 2049280 complete rows without any missing values
   
   head(complete) # so we have a bunch of NAs and some cases with ? marks
   
   #therefore 2075259 - 2049280 = 25979.  And this corresponds to the fact that missing values occur only in the Sub_metering_3 column.
   
  # We are going to grep the ? to see if that occurs in other rows as well
   length(grep("[?]",readDataFrame$Global_active_power)) #etc.
   
   # it looks like ? only occurs in the same rows that have NAs.
   
  
  }else{
  print("File absent: ending execution")
}

