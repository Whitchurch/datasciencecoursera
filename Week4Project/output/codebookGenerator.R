View(tidySet)
pathtolaad <- rstudioapi::getSourceEditorContext()$path
pathtolaad <- gsub("codebookGenerator.R","tidydata.txt",pathtolaad)

loadDataset <- read.csv(pathtolaad,header = TRUE,sep=",")
View(loadDataset)

# We got the names for the Variable column
columnNames <- names(loadDataset) # Get all the column names
str(columnNames)

columnUnits <- columnNames
columnLocation <- columnNames


columnUnitsFrequency <- grep("^Freq",columnUnits)
columnUnitsTme <- grep("^Time",columnUnits)

for(x in columnUnitsFrequency)
{
  columnUnits[x] <- "Frequency"
}

for(x in columnUnitsTme)
{
  columnUnits[x] <- "Time"
}


columnUnits <- gsub("volunteer_id","Numeric",columnUnits)
columnUnits <- gsub("activities","String",columnUnits)

# We got the units for the Units column
str(columnUnits)


# We get the column location for the variables
columnLocation <- gsub("volunteer_id",as.integer(1),columnLocation)
columnLocation <- gsub("activities",as.integer(2),columnLocation)

for(x in columnUnitsFrequency)
{
  columnLocation[x] <- as.integer(x)
}
for(x in columnUnitsTme)
{
  columnLocation[x] <- as.integer(x)  
}
str(as.integer(columnLocation))

# We now generate the desctiption for the variables programatically
columnDescription <- vector(mode = "character", length = length(columnNames))
counter = 0
for(x in columnNames)
{
  counter <- counter + 1
  if(grepl("^volun",x))
  {
    columnDescription[counter] = "This is the Id assigned to the person taking part in the experiment"
  
  }
  
  if(grepl("^activ",x))
  {
    columnDescription[counter] = "Name of the Activity performed in the experiment"
    
  }
  
  if(grepl("[M,m][E,e][A,a][N,n]",x))
  {
    columnDescription[counter] <- paste(toString(columnDescription[counter]), "The average") 
  }
  if(grepl("StandardDeviatio",x))
  {
    columnDescription[counter] <- paste(toString(columnDescription[counter]), "The Standard deviation of") 
  }
  
  if(grepl("^Time",x))
  {
    columnDescription[counter] <- paste(toString(columnDescription[counter]), "time taken by")
  }
  
  if(grepl("^Frequ",x))
  {
    columnDescription[counter] <- paste(toString(columnDescription[counter]), "frequency of")
  }
  
  if(grepl("BodyAcceleration",x))
  {
    columnDescription[counter] <- paste(toString(columnDescription[counter]), " the body accelerating")
  }
  
  if(grepl("GravityAcceleration",x))
  {
    columnDescription[counter] <- paste(toString(columnDescription[counter]), " the  acceleration due to gravity")
  }
  
  if(grepl("AngularVelocity",x))
  {
    columnDescription[counter] <- paste(toString(columnDescription[counter]), " the  gyroscopic angular velocty")
  }
  
  if(grepl("JerkSignal",x))
  {
    columnDescription[counter] <- paste(toString(columnDescription[counter]), " of the Jerk Signal")
  }
  
  if(grepl("Magnitude",x))
  {
    columnDescription[counter] <- paste(toString(columnDescription[counter]), " Magnitude")
  }
  
  if(grepl("AlongXAxis",x))
  {
    columnDescription[counter] <- paste(toString(columnDescription[counter]), "along the X axis")
  }
  if(grepl("AlongYAxis",x))
  {
    columnDescription[counter] <- paste(toString(columnDescription[counter]), "along the Y axis")
  }
  if(grepl("AlongZAxis",x))
  {
    columnDescription[counter] <- paste(toString(columnDescription[counter]), "along the Z axis")
  }

    
}

validRange <- vector(mode = "character", length = length(columnNames))

validRange <- gsub("Numeric","Values: 1 to 99999.... to infinity",columnUnits)
validRange <- gsub("String","Values: LAYING, SITTING, STANDING, WALKING, WALKING_DOWNSTAIRS, WALKING_UPSTAIRS",validRange)
validRange <- gsub("Time","10^9 accuracy in decimal",validRange)
validRange <- gsub("Frequency","10^9 accuracy in decimal",validRange)

VariableName <- columnNames
VariableName <- cbind(VariableName,columnUnits)
VariableName <- cbind(VariableName,validRange)
VariableName <- cbind(VariableName,columnDescription)


result <- pandoc.table.return(dataframeToMarkdown,split.tables = Inf,style = "rmarkdown")
class(result)

pathofinterest <- rstudioapi::getSourceEditorContext()$path
pathofinterest <- gsub("codebookGenerator.R","codebook.md",pathofinterest)

fileconn <- file(pathofinterest)
writeLines(result,fileconn)
close(fileconn)


