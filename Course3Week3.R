library(dplyr)

#Question -1
if(file.exists("./DownloadFiles/uscommunitiesidaho.csv"))
{
  print("the file exists")
}else
{
  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
  download.file(fileurl,destfile = "./DownloadFiles/uscommunitiesidaho.csv", method = "curl")
}

filepath <- "./DownloadFiles/uscommunitiesidaho.csv"
dataFrameCSV <- read.csv(filepath)
head(dataFrameCSV, n = 2)

agricultureLogical <- (dataFrameCSV$ACR == 3 & dataFrameCSV$AGS == 6)
which(agricultureLogical)


#Question -2
install.packages("jpeg")
library(jpeg)

jpegInfo <- readJPEG(url("https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"),native = TRUE)
