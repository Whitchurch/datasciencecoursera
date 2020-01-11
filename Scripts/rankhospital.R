#Helper function to return: The best performing hospital in a state, based on a certain outcome criteria

rankhospital <- function(state, outcome,rankValue = "best")
{
  print("Inside the Best.R")
  
  ## Read the outcome data
  print("Read data section")
  setwd(directoryDataPath)
  dataframeofinterest <- loadCSVfile(directoryDataPath,"outcome-of-care-measures.csv")
  
  
  ## Check that state and outcome are valid
  # Check if state is present: We apply a mask, sum up all trues, if there is atleast one occurence >0, then state is valid
  filtermask <- state == dataframeofinterest$State
  StatePresent <- sum(as.integer(filtermask))
  
  if(StatePresent > 0)
  {
    ## Do nothing since the state is present
  }
  else
  {
    stop("invalid state")
  }
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
  
  newdataframe <- dataframeofinterest[filtermask,c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  
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
    vecresult <- newdataframe[order(newdataframe$HA30,newdataframe$Hospital.Name),]
    vecRank <- rank(vecresult$HA30,ties.method = "first")
    vecresult$Rank <- vecRank
    vecresultMask <- !is.na(vecresult$HA30)
    vecresult <- vecresult[vecresultMask,]
    
    if(rankValue == "best")
    {
      print(head(vecresult,1))   
    }
    else if(rankValue == "worst")
    {
      print(tail(vecresult,1))    
    }
    else if(rankValue <= nrow(vecresult))
    {
      print(vecresult[rankValue,"Hospital.Name"])
    }
    else
    {
      return(NA)
    }
  }
  else if(outcome == "heart failure")
  {
    vecresult <- newdataframe[order(newdataframe$HF30,newdataframe$Hospital.Name),]
    vecRank <- rank(vecresult$HF30,ties.method = "first")
    vecresult$Rank <- vecRank
    vecresultMask <- !is.na(vecresult$HF30)
    vecresult <- vecresult[vecresultMask,]
    
    if(rankValue == "best")
    {
      print(head(vecresult,1))   
    }
    else if(rankValue == "worst")
    {
      print(tail(vecresult,1))    
    }
    else if(rankValue <= nrow(vecresult))
    {
      print(vecresult[rankValue,"Hospital.Name"])
    }
    else
    {
      return(NA)
    }
    
    
  }
  else
  {
    vecresult <- newdataframe[order(newdataframe$PN30,newdataframe$Hospital.Name),]
    vecRank <- rank(vecresult$PN30,ties.method = "first")
    vecresult$Rank <- vecRank
    vecresultMask <- !is.na(vecresult$PN30)
    vecresult <- vecresult[vecresultMask,]
    
    if(rankValue == "best")
    {
      print(head(vecresult,1))   
    }
    else if(rankValue == "worst")
    {
      print(tail(vecresult,1))    
    }
    else if(rankValue <= nrow(vecresult))
    {
      print(vecresult[rankValue,"Hospital.Name"])
    }
    else
    {
      return(NA)
    }
    
  }
  # 
  # #Sort by alphabetical order and send the top most result
  # head(result)
}