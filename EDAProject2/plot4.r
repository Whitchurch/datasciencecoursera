library(dplyr)
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

SCC <- readfiles("plot4.r","Source_Classification_Code.rds")
NEI <- readfiles("plot4.r","summarySCC_PM25.rds")



indexforCombustion <- grep("[C,c]omb",SCC$SCC.Level.One) # rows filtered by combustion
indexforCoal <- grep("[C,c]oal", SCC$SCC.Level.Three) # rows filtered by coal.

#we need the values for combustion by coal. So we will be using these 2 indexes
options(scipen = 999) #deactivate scientific notations
SCCCombustion <- SCC[indexforCombustion,]
SCCCCoal <- SCC[indexforCoal,]
SCCCCoal <- SCCCCoal[(SCCCCoal$SCC%in%SCCCombustion$SCC),]


NEIFiltered <-  NEI[(NEI$SCC%in%SCCCCoal$SCC),] # this gives us the coal combustion sources (subsetted)
typeyearsubset <- group_by(NEIFiltered, year)
typeyearsubset <- summarize_at(typeyearsubset, .vars = c("Emissions") ,.funs = sum)


arrayForBarplot <- array(typeyearsubset$Emissions, dim = length(typeyearsubset$year))
dimnames(arrayForBarplot) <- list(typeyearsubset$year)
currentpath <- rstudioapi::getSourceEditorContext()$path
path1 = gsub("plot4.r","plot4.png", currentpath)
png(path1,width = 480, height = 480)
barplot(arrayForBarplot, xlab = "Years", ylab = "Total PM 2.5 emission in (Tons)", main = "Coal Combustion from : 1999 - 2008", col = "blue")
lines(arrayForBarplot, col = "green",lwd = 3)
points(arrayForBarplot, pch = 16, col = "green")
legend("topright",legend = c("Trend"),lty = 1,col ="green", bty = "n", lwd = 3)
dev.off()

# str(grep("[C,c]oal",SCC$EI.Sector, value = TRUE))
# str(SCC$EI.Sector)
# str(grep("[C,c]oal", SCC$SCC.Level.Three, value = TRUE))
# str(grep("[C,c]oal", SCC$SCC.Level.Four, value = TRUE))
# str(grep("[C,c]oal", SCC$Short.Name, value = TRUE))
# str(grep("[C,c]omb", SCC$Short.Name, value = TRUE))
# SCC[SCC$Data.Category == "Point",]
# SCC[SCC$SCC == "10100101",]
# dim(SCC[SCC$Data.Category == "Onroad","SCC.Level.Three"])
# str(grep("Vehicles",SCC$SCC.Level.Two, value = TRUE))
# unique(SCC$Data.Category)
# dim(SCC[SCC$Data.Category == "Onroad",])
# dim(SCC[SCC$Data.Category == "Nonroad",])
# str(unique(SCC[SCC$Data.Category == "Nonroad","SCC.Level.Four"]))