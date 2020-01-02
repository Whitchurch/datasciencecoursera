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
id <- 1:10
pollutantmean(directoryPath,"sulfate",id)
