#load dplyr
library(dplyr)
library(tidyr)

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

SCC <- readfiles("plot2.r","Source_Classification_Code.rds")
NEI <- readfiles("plot2.r","summarySCC_PM25.rds")


#Subset only the data for Baltimore City based on it's fips code, and sum all it's PM2.5 pollutants

baltimoreonly <- NULL
baltimoreonly <- group_by(NEI, year)
baltimoreonly <- group_by(baltimoreonly, fips, add = TRUE)
baltimoreonly <- summarize_at(baltimoreonly,.vars = c("Emissions") ,.funs = sum)%>%filter(fips == "24510")
print(baltimoreonly)

#Prepare the data to be fed into a boxplot
baltimoreonly <- select(baltimoreonly,-fips) #drop fips as we dn't need for display purposes
names(baltimoreonly) # verify that fips column is dropped

#convert the data into a one dimensional array with attributes for display in barplot
length(baltimoreonly$year)
arrayForBarplot <- array(baltimoreonly$Emissions, dim = length(baltimoreonly$year))
dimnames(arrayForBarplot) <- list(baltimoreonly$year)

#Plot the data
options(scipen = 999) #deactivate scientific notations
currentpath <- rstudioapi::getSourceEditorContext()$path
path1 = gsub("plot2.r","plot2.png", currentpath)
png(path1,width = 480, height = 480)
barplot(arrayForBarplot, xlab = "Years", ylab = "Total emission in (Tons)", main = "(Baltimore City) 1999 - 2008: PM 2.5 emission")
lines(arrayForBarplot, col = "green")
points(arrayForBarplot, pch = 16, col = "green")
legend("topright",legend = c("Trend"),lty = 1,col ="green", bty = "n")
dev.off()
