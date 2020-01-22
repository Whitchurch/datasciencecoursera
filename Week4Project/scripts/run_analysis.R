#Step 0: Clear the environment variables 
rm(list = ls())

#Step 1: Execute the DataFrameCreator function
DataFrameCreator <- function(path1,path2,path3,features){
  
  library(dplyr)
  # ======================================Load the Training Data Set==================================#
  pathtolaad <- path1
  traininputDF <- read.csv(pathtolaad,header = FALSE,sep = "")
  
  #Get an idea about the dataFrame structure
  head(traininputDF)
  nrow(traininputDF)
  ncol(traininputDF)
  str(traininputDF) #7352 rows/obs and 561 variables for Training// 2947 obs/rows Test
 
  
  
  # ======================================Load the Activity Dataset==================================#
  pathtolaad <- path2
  trainoutputDF <- read.csv(pathtolaad,header = FALSE,sep ="")
  
  #Get an idea about the dataFrame structure.
  #head(trainoutputDF)
  #nrow(trainoutputDF)
  #ncol(trainoutputDF)
  #str(trainoutputDF) # 7352 rows/obs and 1 variable // 2947 obs/rows Test
  
  
  
  # ======================================Load the Volunteer Dataset==================================#
  pathtolaad <- path3
  subjecttDF <- read.csv(pathtolaad,header = FALSE,sep ="")
  
  #Get an idea about the dataFrame structure
  #head(subjecttDF)
  #nrow(subjecttDF)
  #ncol(subjecttDF)
  #str(subjecttDF) #7352 rows/obs and 1 variable 2947 obs/rows Test
  

  # ==============================Assign appropriate Variable names to Dataframe columns==================================#
  
  
  subjecttDF <- subjecttDF%>%rename("volunteer_id" = names(subjecttDF))
  trainoutputDF <- trainoutputDF%>%rename("activities" = names(trainoutputDF))
  
  
  #============================Load the 561 feature vector names from the features.txt file=================
  pathtolaad <- features
  featureVariableNamesDF <- read.csv(pathtolaad,header = FALSE,sep="\\")
  #nrow(featureVariableNamesDF)
  #ncol(featureVariableNamesDF)
  #str(featureVariableNamesDF)
  #head(featureVariableNamesDF) #we get a vector with 561 rows, each representing 1 feature variable.
  #class(featureVariableNamesDF$V1) # the column has factor levels.
  
  #Inorder to remove the numerics and perform string opertaions we convert it to a character type
  featureVariableNamesDF$V1 <- as.character(featureVariableNamesDF$V1)
  resultantSplit <- strsplit(featureVariableNamesDF$V1," ")
  featureVariableNameVector <- sapply(resultantSplit, function(x) x[-1]) # we got back a vector with all the featurenames
  
  #Finally we apply the variable names to the columns of the appropriate data set
  names(traininputDF)[1:ncol(traininputDF)] <- featureVariableNameVector[1:length(featureVariableNameVector)]
  
  #=============================Finally combine all 3 dataframes to create the Training Data Set =====#
  
  FinalTrainingDF <- cbind(subjecttDF,trainoutputDF,traininputDF)
  
  
}

#Step 2:
# Create the Training Dataset
#Setup the correct paths:-
pathtolaad1 <- "./Week4Project/data/dataset/train/X_train.txt"
pathtolaad2 <- "./Week4Project/data/dataset/train/y_train.txt"
pathtolaad3 <- "./Week4Project/data/dataset/train/subject_train.txt"
featurestoload <- "./Week4Project/data/dataset/features.txt"

#Call DataFrameCreator function to get the Trained Dataset 
TrainDataSet <- DataFrameCreator(path1 = pathtolaad1,path2 = pathtolaad2,path3 = pathtolaad3,features = featurestoload)


#Step3:
#Create the TEst Dataset
pathtolaad1 <- "./Week4Project/data/dataset/test/X_test.txt"
pathtolaad2 <- "./Week4Project/data/dataset/test/y_test.txt"
pathtolaad3 <- "./Week4Project/data/dataset/test/subject_test.txt"
featurestoload <- "./Week4Project/data/dataset/features.txt"

#Call DataFrameCreator function to get the Trained Dataset 
TestDataSet <- DataFrameCreator(path1 = pathtolaad1,path2 = pathtolaad2,path3 = pathtolaad3,features = featurestoload)

#Step 4: Create the Merged Data Set; Merging Training and Test Datasets
MergedDataSet <- rbind(TrainDataSet,TestDataSet)

#Step5: Coming soon...

#Step6: Use descriptive activity name for activities in the dataset
MergedDataSet$activities
pathtoload <- "./Week4Project/data/dataset/activity_labels.txt"
activityDF <- read.csv(pathtoload,header = FALSE,sep=" ")
activityDF <- activityDF%>%rename("activity_id" = "V1")
activityDF <- activityDF%>%rename("activity" = "V2")

head(MergedDataSet$activities)
head(activityDF)


for(X in MergedDataSet$activities)
{
  if(grep(X,activityDF$activity_id))
  {
    gsub(X,activityDF[X,"activity"],X)
    
  }
}




