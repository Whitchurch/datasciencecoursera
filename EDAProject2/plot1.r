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
    print("Check if the file: Source_Classification_Code.rds is in the same folder as plot1.r")
  }
  
  
}

SCC <- readfiles("plot1.r","Source_Classification_Code.rds")
NEI <- readfiles("plot1.r","summarySCC_PM25.rds")

#Analyze the structure of the Data in both SCC and NEI
str(SCC)
names(SCC)
library(pander)
head(SCC, n = 1)

str(NEI)
names(NEI)
head(NEI)
result <- pandoc.table(head(NEI, n = 2),split.tables = Inf,style="rmarkdown")
unique(NEI$type)  # 4 types are present
unique(NEI$Pollutant)  # Table has data on only PM 2.5 
str(unique(NEI$SCC)) # there are 5387 unique source classification codes in this data set.
str(unique(NEI$fips)) # There is information for 3264 counties in this NEI dataset
unique(NEI$year) # there is data from 4 publications of PM 2.5 levels from 1999. 2002, 2005, 2008 spanning 10 years

#Plotting a table of the total pollutant from all sources divided by 3 year intervals.
#This shows there is an increase in the pollutant sources over this 10 year interval from 1999 - 2008
table(NEI$Pollutant,NEI$year)

#Method 1:-
datafrom1999 <- subset(NEI, year == "1999", select = c("Emissions"))
sum(datafrom1999) #73329767 tons is the total pm2.5 emission from all pollutant sources in 1999
#.... you can repeat the same for every year

#Method 2:-
#using tapply to subset across all years and apply this sum function
plot1Result <- tapply(NEI$Emissions, NEI$year, sum) # we get 7332967 5635780 5454703 3464206 , year 1999 matches what we got in method 1, thus proving tapply works.
str(plot1Result)
# Plotting the data
options(scipen = 999) #deactivate scientific notations
currentpath <- rstudioapi::getSourceEditorContext()$path
path1 = gsub("plot1.r","plot1.png", currentpath)
png(path1,width = 480, height = 480)
barplot(plot1Result, xlab = "Years", ylab = "Total emission PM 2.5 in (Tons)", main = "PM 2.5 Emission from 1999 - 2008")
lines(plot1Result, col = "red")
points(plot1Result, pch = 16, col = "red")
legend("topright",legend = c("Emission trend line"),lty = 1,col ="red", bty = "n")
# in the last 2 lines I added points and lines to better depict the downward trend
dev.off()



#=============End of Code===========#

#Old method of plotting ignore.
#First Method to plot (It is useful , but I abandoned it in favor of a better plotting technique)
# str(plot1Result)
# head(plot1Result)
# names(plot1Result)
# class(plot1Result)

# #We need to tidy this up, as column names are 1999... 2008, these should be classified under a years column, with corresponding emissions in the emissions column
# options(scipen=0)
# plot1ResultTidy <- data.frame(year = as.Date(as.character(names(plot1Result)),"%Y"), emissions = plot1Result)
# range(plot1ResultTidy$emissions)
# 
# # using a traditional plot, with lines to show downward trend is pretty informative, but hard to interpret
# plot(plot1ResultTidy$year,plot1ResultTidy$emissions, pch = 16, col =as.factor(plot1ResultTidy$year) , xlab = "Year", ylab = "Total PM 2.5 emission (Tons)")
# points(plot1ResultTidy$year,plot1ResultTidy$emissions, type = "l")
# legend("topright",legend = c("PM 2.5 in 1999","PM 2.5 in 2002","PM 2.5 in 2005","PM 2.5 in 2008"),pch = 16,col = as.factor(plot1ResultTidy$year), bty = "n")







