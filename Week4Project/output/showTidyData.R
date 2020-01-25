#====== Run this script to view the tidy Data ========#
pathofinterest <- rstudioapi::getSourceEditorContext()$path
pathofinterest <- gsub("showTidyData.R","tidydata.txt",pathofinterest)
tidyDataToDisplay <- read.csv(pathofinterest,header = TRUE,sep = ",")
View(tidyDataToDisplay)