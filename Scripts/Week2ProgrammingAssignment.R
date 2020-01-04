# 0th Get the working directory path
directoryPath <- paste("~/Desktop/Rproject/datasciencecoursera/Data","specdata",sep = "/")

#Helper functions

# 1th Function: Load a CSV file based on the path specified
loadCSVfile <- function(directory,filename)
{
   CompleteFilepath <- paste(directory,filename,sep = "/")
  dataFrameHandle <- read.csv(CompleteFilepath)
  
}

# 2: format the filename
formatFilename <- function(index,id)
{
  filename <- sprintf("%03d.csv", id[index]) 
   
}

formatFilename_v1 <- function(id)
{
  filename <- sprintf("%03d.csv", id) 
  
}

# Second Assignment functions

# pollutant mean: Gives the mean pollutant value
# Parameters: directory -> location of the CSV files; pollutant -> type of pollutant to measure; id: ID of pollution monitiors being used

pollutantmean <- function(directory,pollutant= "sulfate",id = 1:332)
{
  accumulator <- NULL
  for(value in 1:length(id) )
  {
    
    filename <- formatFilename(value,id)
    
    dataframehandle <- loadCSVfile(directory,filename)
    meanValue <- 0
    
    #print(head(dataframehandle$sulfate,n=3))
    
    #Code to combine nitrate/sulphate columns from multiple files into one large vector called accumulator
    if(length(accumulator) == 0)
    {
      if(pollutant == "sulfate")
      {
        accumulator <- dataframehandle$sulfate 
      }
      else
      {
        accumulator <- dataframehandle$nitrate   
      }
         
    }
    else
    {
      if(pollutant == "sulfate")
      {
        accumulator <- c(accumulator,dataframehandle$sulfate )
      }
      else
      {
        accumulator <- c(accumulator,dataframehandle$nitrate )
      }
      
    }
    
  }
  
# Calculate and spit out the mean of the accumulator vector
meanValue <- mean(accumulator,na.rm = TRUE)
print(round(meanValue,digits = 3)) #rounding the value to 3

}

pollutantmean(directoryPath,"sulfate",1:10)
pollutantmean(directoryPath, "nitrate", 70:72)
pollutantmean(directoryPath, "nitrate", 23)

#complete: Reads a directory ful of files and reports on the number of complete observations
#parameters directory <- Location of the CSV files, id <- ID of sensor

complete <- function(directory,id=1:332)
{
  fileid <- NULL
  nobscount <- NULL
  returnedResult <- NULL
  
  for(value in id)
  {

    filename <-   formatFilename_v1(value)
    dataframehandle <- loadCSVfile(directory,filename) 
    
    
    sumresult <- sum(as.integer(is.na(dataframehandle$sulfate) | is.na(dataframehandle$nitrate)))
   
    #print(sumresult)
    #print(nrow(dataframehandle))
    #print(nrow(dataframehandle)-sumresult)
    #mask <- (is.na(dataframehandle$Date) & is.na(dataframehandle$sulfate) &is.na(dataframehandle$nitrate)&is.na(dataframehandle$ID))
    
    #datafromahandle <- dataframehandle[mask,"ID"]
    #print(datafromahandle)
    
    # Record nobs
    if(length(nobscount)==0)
    {
      fileid <- head(dataframehandle$ID,n = 1)
      nobscount <- (nrow(dataframehandle)-sumresult)
    
    }
    else
    {
      fileid <- c(fileid,head(dataframehandle$ID,n = 1))
      nobscount <- c(nobscount,(nrow(dataframehandle)-sumresult))
      
    }
    
  }
  
  
  #print(fileid)
  #print(nobscount)
  
  returnedResult <- data.frame(fileid,nobscount)  
  #print(returnedResult)
  
}

complete(directoryPath,30:25)

complete1 <- function(directory,id=1:332)
{
  fileid <- NULL
  nobscount <- NULL
  returnedResult <- NULL
  
  for(value in id)
  {
    filename <-   formatFilename_v1(value)
    dataframehandle <- loadCSVfile(directory,filename)
    
    sumresult <- sum(as.integer(is.na(dataframehandle$sulfate) | is.na(dataframehandle$nitrate)))
    
    if(length(nobscount)==0)
    {
      
      nobscount <- (nrow(dataframehandle)-sumresult)
      
    }
    else
    {
      
      nobscount <- nobscount+(nrow(dataframehandle)-sumresult)
      
    }  
  }

  
  returnedResult <- nobscount
  returnedResult
  
}

#corr: Finds correlation betwween sulphate and nitrate , based on a threshold of complete values
#parameters: directory <- location of CSV file, threshold <- the limit for number of complete values


corr <- function(directory,threshold = 0)
{
  
  dataframeCountValue <- complete(directory = directory)
  counter <- 0
  storeCorResults <- NULL
  
  for(value in dataframeCountValue[,"nobscount"])
  {
    counter <- counter + 1
    if(value > threshold)
    {
      filename <-   formatFilename_v1(counter)
      dataframehandle <- loadCSVfile(directory,filename)
      mask <- is.na(dataframehandle$sulfate) | is.na(dataframehandle$nitrate)

      dataframehandleSulfate <- dataframehandle[!mask,"sulfate"]
      dataframehandleNitrate <- dataframehandle[!mask,"nitrate"]
      

      corAnalyzedValue <- cor(dataframehandleSulfate,dataframehandleNitrate)
      
      if(length(storeCorResults) == 0)
      {
        storeCorResults <- corAnalyzedValue
      }
      else
      {
        storeCorResults <- c(storeCorResults,corAnalyzedValue)
      }
    }
  }
  
  storeCorResults
  #print(head(storeCorResults))
  #print(summary(storeCorResults))
  # startindex <- 1
  # stopindex <- 1
  # dataframeCountValue <- complete1(directory = directory,stopindex)
  # 
  # 
  # while (dataframeCountValue < threshold) {
  #   stopindex <- stopindex+1
  #   updateValue <- complete1(directory = directory,stopindex)
  #   dataframeCountValue <- dataframeCountValue+updateValue
  #   print(dataframeCountValue)
  # }
  # 
  #   CorrelationValue <- function(directory,startindex,stopindex)
  #   {
  #     for(value in 1:length(stopindex) )
  #     {
  #       
  #       print(value)
  #       filename <-   formatFilename_v1(value)
  #       dataframehandle <- loadCSVfile(directory,filename) 
  #       mask <- is.na(dataframehandle$sulfate) | is.na(dataframehandle$nitrate)
  #       
  #       dataframehandleSulfate <- dataframehandle[!mask,"sulfate"]
  #       dataframehandleNitrate <- dataframehandle[!mask,"nitrate"]
  #       
  #       corrResult <- cor(dataframehandleSulfate,dataframehandleNitrate)
  #       
  #       print(summary(corrResult))
  #     }
  #   }
  #   
  #   CorrelationValue(directory,startindex,stopindex)   

}


corr(directoryPath)

#Quiz questions to be processed
pollutantmean(directoryPath, "sulfate", 1:10)
pollutantmean(directoryPath, "nitrate", 70:72)
pollutantmean(directoryPath, "sulfate", 34)
pollutantmean(directoryPath, "nitrate")

cc <- complete(directoryPath, c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete(directoryPath, 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete(directoryPath, 332:1)
use <- sample(332, 10)
print(cc[use, "nobscount"])

cr <- corr(directoryPath)                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr(directoryPath, 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr(directoryPath, 2000)                
n <- length(cr)                
cr <- corr(directoryPath, 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
