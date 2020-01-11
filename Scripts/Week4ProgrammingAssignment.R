# 0th Get the working directory path
directoryDataPath <- paste("~/Desktop/Rproject/datasciencecoursera/Data","hospitaldata",sep = "/")

# 1 Plot the 30-day mortality rates for heart attack

#Helper functions

# 1th Function: Load a CSV file based on the path specified
loadCSVfile <- function(directory,filename)
{
  CompleteFilepath <- paste(directory,filename,sep = "/")
  dataFrameHandle <- read.csv(CompleteFilepath)
  
}

outcome <- loadCSVfile(directoryPath,"outcome-of-care-measures.csv")
head(outcome, n = 1) # Have a look at sample data
names(outcome) # Get the column names 
str(outcome) # Deep dive to see the structure of the data frame
NROW(outcome) # get the total rows
ncol(outcome) # get the total columns


#Get the column of 30 day Mortaility due to heart attacks
ThirtyDayHeartAttachMortality <- outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
print(head(ThirtyDayHeartAttachMortality,n = 5))
summary(ThirtyDayHeartAttachMortality)

#convert the ThirtyDayHeartAttackMortality from a factor to numeric for displaying in the histogram
ThirtyDayHeartAttachMortality <- as.numeric(ThirtyDayHeartAttachMortality)
print(head(ThirtyDayHeartAttachMortality, n = 5))
hist(ThirtyDayHeartAttachMortality)

#Bonus playing around with the data
NameofHospitals <- outcome$Hospital.Name
str(NameofHospitals)
print(NameofHospitals)



directoryPath <- paste("~/Desktop/Rproject/datasciencecoursera","Scripts",sep = "/")
setwd(directoryPath)
# 2 Finding the best hospital in a State.
source("Best.R")
best(state = "MD",outcome = "pneumonia")
source("rankhospital.R")
rankhospital(state = "TX",outcome = "heart attack",rankValue = "best")
rankhospital(state = "TX",outcome = "heart failure",rankValue = "worst")
rankhospital(state = "MD",outcome = "heart attack",rankValue = 5)
source("rankall.R")
r1 <- head(rankall(outcome = "heart attack",20),10)
print(r1$AL)
tail(rankall("pneumonia","worst"),3)
tail(rankall(outcome = "heart failure"),10)

#Quiz questions:
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)

r <- rankall("heart attack", 4) # Lol I made my life easier, :)
print(r$HI)

r <- rankall("pneumonia", "worst")
print(r$NJ)

r <- rankall("heart failure", 10)
print(r$NV)
