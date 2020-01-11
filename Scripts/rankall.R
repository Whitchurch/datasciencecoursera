#Helper function to return: The best performing hospital in a state, based on a certain outcome criteria

rankall <- function(outcome,rankValue = "best")
{
  
  
  ## Read the outcome data
  print("Read data section")
  setwd(directoryDataPath)
  dataframeofinterest <- loadCSVfile(directoryDataPath,"outcome-of-care-measures.csv")
  
  
  # similar logic is applied for outcome
  if(outcome == "heart attack" | outcome == "heart failure" | outcome == "pneumonia")
  {
    ## Do nothing since vaid outcome is supplied
  }
  else
  {
    stop("invalid outcome")
  }
  
  
  ## REturn hospital name in that state with lowest 30 day death rate
  
  #Change Data formats
  dataframeofinterest$Hospital.Name <- as.character(dataframeofinterest$Hospital.Name)
  maskheatattackNAS <- dataframeofinterest$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == "Not Available"
  maskheartfailureNAS <- dataframeofinterest$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == "Not Available"
  maskpneumoniaNAS <- dataframeofinterest$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == "Not Available"
  
  #Assign NAS to the Not availables 
  dataframeofinterest[maskheatattackNAS,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] <- NA
  dataframeofinterest[maskheartfailureNAS,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"] <- NA
  dataframeofinterest[maskpneumoniaNAS,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"] <- NA
  
  # str(dataframeofinterest$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  # str(dataframeofinterest$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  # str(dataframeofinterest$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  # str(dataframeofinterest$State)
  # str(dataframeofinterest$Hospital.Name)
  
  #Make changes to data format, cleanup factor to nums etc etc.
  dataframeofinterest$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(as.character(dataframeofinterest$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
  dataframeofinterest$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(as.character(dataframeofinterest$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
  dataframeofinterest$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(as.character(dataframeofinterest$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  
  str(dataframeofinterest$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  str(dataframeofinterest$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  str(dataframeofinterest$dataframeofinterest$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  str(dataframeofinterest$State)
  str(dataframeofinterest$Hospital.Name)
  
  #filter out based on state and make a new dataframe reference
  
  newdataframe <- dataframeofinterest[,c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  
  # cleanup the names to shorten them for better visualization as well as ease of typing in the future
  colnames(newdataframe)[3] <- "HA30"
  colnames(newdataframe)[4] <- "HF30"
  colnames(newdataframe)[5] <- "PN30"
  
  # verify if name changes are success full
  #str(newdataframe)
  
  #print out to see how stuff looks  now
  #head(newdataframe, n = 2)
  
  
  #Start the real analysis, since our data is already filtered by state , we just need to find the min value of mortality rate
  if(outcome == "heart attack")
  {
   
    #Split the data based on the states
    newSplitData <- split(newdataframe,newdataframe$State)
    
    #Verify if the split worked
    orderedLList <- lapply(newSplitData, function(x)
      {
      
      vecresult <- x[order(x$HA30,x$Hospital.Name),]
      vecRank <- rank(vecresult$HA30,ties.method = "first")
      vecresult$Rank <- vecRank
      vecresultMask <- !is.na(vecresult$HA30)
      vecresult <- vecresult[vecresultMask,]
      
        if(rankValue == "best")
        {
          head(vecresult,1)
        }
        else if(rankValue == "worst")
        {
          tail(vecresult,1)
        }
        else if(rankValue <= nrow(vecresult))
        {
           vecresult[rankValue,c("State","Hospital.Name")]
        }
        else
        {
          return(NA)
        }
      
      })
  }
  
  else if(outcome == "heart failure")
  {
    #Split the data based on the states
    newSplitData <- split(newdataframe,newdataframe$State)
    
    #Verify if the split worked
    orderedLList <- lapply(newSplitData, function(x)
    {
      
      vecresult <- x[order(x$HF30,x$Hospital.Name),]
      vecRank <- rank(vecresult$HF30,ties.method = "first")
      vecresult$Rank <- vecRank
      vecresultMask <- !is.na(vecresult$HF30)
      vecresult <- vecresult[vecresultMask,]
      if(rankValue == "best")
      {
        head(vecresult,1)
      }
      else if(rankValue == "worst")
      {
        tail(vecresult,1)
      }
      else if(rankValue <= nrow(vecresult))
      {
        vecresult[rankValue,"Hospital.Name"]
      }
      else
      {
        return(NA)
      }
      
    })
    
  }
  else
  {

    #Split the data based on the states
    newSplitData <- split(newdataframe,newdataframe$State)
    
    #Verify if the split worked
    orderedLList <- lapply(newSplitData, function(x)
    {
      
      vecresult <- x[order(x$PN30,x$Hospital.Name),]
      vecRank <- rank(vecresult$PN30,ties.method = "first")
      vecresult$Rank <- vecRank
      vecresultMask <- !is.na(vecresult$PN30)
      vecresult <- vecresult[vecresultMask,]
      
      if(rankValue == "best")
      {
      head(vecresult,1)
      }
      else if(rankValue == "worst")
      {
      tail(vecresult,1)
      }
      else if(rankValue <= nrow(vecresult))
      {
        vecresult[rankValue,"Hospital.Name"]
      }
      else
      {
        return(NA)
      }
      
    })   
  }
  # 
  # #Sort by alphabetical order and send the top most result
  # head(result)
}