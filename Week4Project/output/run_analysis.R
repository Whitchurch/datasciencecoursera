#Step 0: Clear the environment variables and loadup required packages

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
pathofinterest <- rstudioapi::getSourceEditorContext()$path

#Step 2:
# Create the Training Dataset by calling the function DataFrameCreator
#Setup the correct paths:-
pathofinterest <- rstudioapi::getSourceEditorContext()$path
pathtolaad1 <- gsub("Week4Project/output/run_analysis.R","/Week4Project/data/dataset/train/X_train.txt",pathofinterest)
pathtolaad2 <- gsub("Week4Project/output/run_analysis.R","/Week4Project/data/dataset/train/y_train.txt",pathofinterest)
pathtolaad3 <- gsub("Week4Project/output/run_analysis.R","/Week4Project/data/dataset/train/subject_train.txt",pathofinterest)
featurestoload <- gsub("Week4Project/output/run_analysis.R","/Week4Project/data/dataset/features.txt",pathofinterest)


#Call DataFrameCreator function to get the Trained Dataset 
TrainDataSet <- DataFrameCreator(path1 = pathtolaad1,path2 = pathtolaad2,path3 = pathtolaad3,features = featurestoload)


#Step3:
#Create the Test Dataset by calling the function DataFrame creator
pathofinterest <- rstudioapi::getSourceEditorContext()$path
pathtolaad1 <- gsub("Week4Project/output/run_analysis.R","/Week4Project/data/dataset/test/X_test.txt",pathofinterest)
pathtolaad2 <- gsub("Week4Project/output/run_analysis.R","/Week4Project/data/dataset/test/y_test.txt",pathofinterest)
pathtolaad3 <- gsub("Week4Project/output/run_analysis.R","/Week4Project/data/dataset/test/subject_test.txt",pathofinterest)
featurestoload <- gsub("Week4Project/output/run_analysis.R","/Week4Project/data/dataset/features.txt",pathofinterest)


#Call DataFrameCreator function to get the Trained Dataset 
TestDataSet <- DataFrameCreator(path1 = pathtolaad1,path2 = pathtolaad2,path3 = pathtolaad3,features = featurestoload)

result <- pandoc.table(TestDataSet[1:5,1:8],style="rmarkdown")

#Step 4: Create the Merged Data Set; Merging Training and Test Datasets
MergedDataSet <- rbind(TrainDataSet,TestDataSet)


#Step5: Extract the mean and SD columns:-
str(v1 <- grep("std|mean",names(MergedDataSet)))
ColumnsToSelect <- names(MergedDataSet[v1])
ColumnsToSelect <- append(ColumnsToSelect,c("volunteer_id","activities"),after = 0)


#Note I have conscioulsy included MeafFrequency, as it calculates Mean of Frequencies, and qualifies as a Mean, albeit indirectly.
str(ColumnsToSelect)
duplicated(ColumnsToSelect)

#There seem to be some duplicate columns with the same name that is causing dplyr select to fail:-
duplicateColumnMask <- duplicated(names(MergedDataSet))
duplicatevalues <- sum(as.integer(duplicateColumnMask)) #84 duplicates

v2 <- grep("std|mean",(names(MergedDataSet[duplicated(names(MergedDataSet))])))
ColumnsToSelectV2 <- names(MergedDataSet[v2]) # returns 0, This means none of the duplicated columns are of interest to us



sprintf("Total Number of Duplicate Columns = %i", duplicatevalues)
sprintf("Are there any mean std columns that are duplicated= %i", sum(as.integer(ColumnsToSelectV2)))

#Therefore instead of wrangling the data further, I take only the columns of interest, ignore the rest.
MergedDataSet <- MergedDataSet[,ColumnsToSelect]
str(MergedDataSet)

result <- pandoc.table(MergedDataSet[1,],split.tables = Inf,style = "rmarkdown")


#Step6: Use descriptive activity name for activities in the dataset
print(head(MergedDataSet$activities,n=5))

pathofinterest <- rstudioapi::getSourceEditorContext()$path
pathtolaad <- gsub("/Week4Project/output/run_analysis.R","/Week4Project/data/dataset/activity_labels.txt",pathofinterest)
#pathtoload <- "./Week4Project/data/dataset/activity_labels.txt"
activityDF <- read.csv(pathtolaad,header = FALSE,sep=" ")
activityDF <- activityDF%>%rename("activity_id" = "V1")
activityDF <- activityDF%>%rename("activity" = "V2")

head(MergedDataSet$activities)
head(activityDF)

#res <- vector(mode="character",length = length(MergedDataSet$activities))

for(X in activityDF$activity_id)
{
  if(grep(X,activityDF$activity_id))
  {
    #print(X)
    #print(as.character(activityDF[X,"activity"]))
    MergedDataSet$activities <- gsub(X,as.character(activityDF[X,"activity"]),MergedDataSet$activities)
   
    
  }
}

head(MergedDataSet$activities,n=5)


#Step 7: Give more descriptive names for the variables so non-domain experts understand it
names(MergedDataSet) <- gsub("-","",names(MergedDataSet))
names(MergedDataSet) <- gsub("X$","_AlongXAxis",names(MergedDataSet))
names(MergedDataSet) <- gsub("Y$","_AlongYAxis",names(MergedDataSet))
names(MergedDataSet) <- gsub("Z$","_AlongZAxis",names(MergedDataSet))
names(MergedDataSet) <- gsub("std()","_StandardDeviation",names(MergedDataSet))
names(MergedDataSet) <- gsub("meanFreq()","_Meanfrequency",names(MergedDataSet))
names(MergedDataSet) <- gsub("[()]","",names(MergedDataSet))
names(MergedDataSet) <- gsub("mean","_Mean",names(MergedDataSet))
names(MergedDataSet) <- gsub("Acc","Acceleration",names(MergedDataSet))
names(MergedDataSet) <- gsub("Mag","Magnitude",names(MergedDataSet))
names(MergedDataSet) <- gsub("Gyro","AngularVelocity",names(MergedDataSet))
names(MergedDataSet) <- gsub("^t","Time_of_",names(MergedDataSet))
names(MergedDataSet) <- gsub("^f","Frequency_of_",names(MergedDataSet))
names(MergedDataSet) <- gsub("Jerk","JerkSignal",names(MergedDataSet))
names(MergedDataSet) <- gsub("BodyBody","Body",names(MergedDataSet))

# I have grepped and used highly descriptive verbose variable names so all lay people can understand the meaning


#Step 8: Creating the tidy dataset 
# We group by Volunteer_id ad activities.

MergedDataSet <- group_by(MergedDataSet,volunteer_id)
MergedDataSet <- group_by(MergedDataSet,activities,add = TRUE)


#Now if we apply summarize_at, with means as the function to apply. it will summarize the columns of interest,
#by breaking them into sections. The sections are demarcated by (Volunteer_id and Activity) which is what group_by
#did for us.

vec <- names(MergedDataSet)
vec <- tail(vec,-2)
str(vec)

tidySet <- summarize_at(MergedDataSet,.vars = vec ,.funs = mean)


#============================================Viewing the Tidy Data Set ==============================#
View(tidySet)
nrow(tidySet)
ncol(tidySet)

#========================Generating the required filed: tidydata.txt=====================#
#Writing the tidyDataSet
pathofinterest <- rstudioapi::getSourceEditorContext()$path
pathtolaad <- gsub("Week4Project/output/run_analysis.R","Week4Project/output/tidydata.txt",pathofinterest)
write.csv(tidySet,pathtolaad,row.names = FALSE)


