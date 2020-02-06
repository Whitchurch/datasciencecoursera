#load dplyr
library(dplyr)
library(tidyr)
library(ggplot2)


#Function to read and return the files

readfiles <- function(filenametoreplace, filenametosubstitute)
{
  currentpath <- rstudioapi::getSourceEditorContext()$path
  currentpath <- gsub(filenametoreplace,filenametosubstitute, currentpath)
  if(file.exists(currentpath))
  {
    print("The file exists")
    readRDS(currentpath)  
  }else{
    print("Check if the file: Source_Classification_Code.rds is in the same folder as plot2.r")
  }
  
  
}

SCC <- readfiles("plot3.r","Source_Classification_Code.rds")
NEI <- readfiles("plot3.r","summarySCC_PM25.rds")

#Subset the data by: type as well as by years
options(scipen = 0) #deactivate scientific notations
currentpath <- rstudioapi::getSourceEditorContext()$path
path1 = gsub("plot3.r","plot3.png", currentpath)
png(path1,width = 480, height = 480)
typeyearsubset <- NULL
typeyearsubset <- group_by(NEI, year)
typeyearsubset <- group_by(typeyearsubset,type, add = TRUE)%>%filter(fips == "24510")

typeyearsubset <- summarize_at(typeyearsubset, .vars = c("Emissions") ,.funs = sum)
type <- as.factor(typeyearsubset$type)
#qplot(year,Emissions, data = typeyearsubset, color = type, geom = "line", main = "Emission by types (Baltimore City)1999 - 2008")
p <- ggplot(typeyearsubset, aes(year,Emissions))+geom_line(aes(color = type))+labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)"))
p+labs(title=expression("Emission by types (Baltimore City)1999 - 2008"))
dev.off()

