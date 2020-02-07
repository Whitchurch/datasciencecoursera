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

SCC <- readfiles("plot5.r","Source_Classification_Code.rds")
NEI <- readfiles("plot5.r","summarySCC_PM25.rds")


onRoadMVD <- SCC%>%filter(Data.Category == "Onroad")  #Onroad vehicles
nonRoadMVD <- SCC%>%filter(Data.Category == "Nonroad") #Off road vehicles

dim(onRoadMVD) #1137 MVD
dim(nonRoadMVD) #572 some are vechicles/vessels, the rest is noise.

str(grep("Mobile",onRoadMVD$SCC.Level.One)) # All are mobile vehicles
str(grep("Mobile",nonRoadMVD$SCC.Level.One))


length(unique(nonRoadMVD$SCC.Level.Three)) # there are 29 unique categories, we will be selecting those from this that make sense as MVD
unique(nonRoadMVD$SCC.Level.Three) # After observing we can see a majority of the categories do apply to MVD

unique(nonRoadMVD$SCC.Level.Four) # We observe and select what to exclude from this list
# list of things to include:

# [3] Motorcycles: Off-road                                       Snowmobiles                                                
# [5] All Terrain Vehicles                                        Minibikes                                                  
# [7] Golf Carts                                                  Specialty Vehicles/Carts  
# 
# Total, All Vessel Types                                     Ocean-going Vessels                                        
# [97] Harbor Vessels                                              Fishing Vessels                                            
# [99] Military Vessels                                            Inboard                                                    
# [101] Outboard                                                    Personal Water Craft                                       
# [103] Sailboat Auxiliary Inboard                                  Sailboat Auxiliary Outboard 

# there is a 9 percent contribution from Motor vechicles in the non-road category. Which is quite substantial
# As a result I will be adding these along with the Onroad category for analysis
mask <- nonRoadMVD$SCC.Level.Four == "Motorcycles: Off-road"|nonRoadMVD$SCC.Level.Four == "Snowmobiles"|nonRoadMVD$SCC.Level.Four == "All Terrain Vehicles"|nonRoadMVD$SCC.Level.Four == "Minibikes"|nonRoadMVD$SCC.Level.Four == "Golf Carts"|nonRoadMVD$SCC.Level.Four == "Specialty Vehicles/Carts"|nonRoadMVD$SCC.Level.Four == "Ocean-going Vessels"|nonRoadMVD$SCC.Level.Four == "Harbor Vessels"|nonRoadMVD$SCC.Level.Four == "Fishing Vessels"|nonRoadMVD$SCC.Level.Four == "Military Vessels"|nonRoadMVD$SCC.Level.Four == "Outboard" |nonRoadMVD$SCC.Level.Four == "Inboard"|nonRoadMVD$SCC.Level.Four == "Personal Water Craft" |nonRoadMVD$SCC.Level.Four == "Sailboat Auxiliary Inboard" |nonRoadMVD$SCC.Level.Four == "Sailboat Auxiliary Outboard"

nonroadContribution <- sum(mean(nonRoadMVD$SCC.Level.Four == "Motorcycles: Off-road")*100,
                           mean(nonRoadMVD$SCC.Level.Four == "Snowmobiles")*100,
                           mean(nonRoadMVD$SCC.Level.Four == "All Terrain Vehicles")*100,
                           mean(nonRoadMVD$SCC.Level.Four == "Minibikes")*100,
                           mean(nonRoadMVD$SCC.Level.Four == "Golf Carts")*100,
                           mean(nonRoadMVD$SCC.Level.Four == "Specialty Vehicles/Carts")*100,
                           mean(nonRoadMVD$SCC.Level.Four == "Ocean-going Vessels")*100,
                           mean(nonRoadMVD$SCC.Level.Four == "Harbor Vessels")*100,
                           mean(nonRoadMVD$SCC.Level.Four == "Fishing Vessels")*100,
                           mean(nonRoadMVD$SCC.Level.Four == "Military Vessels")*100,
                           mean(nonRoadMVD$SCC.Level.Four == "Outboard")*100,
                           mean(nonRoadMVD$SCC.Level.Four == "Inboard")*100,
                           mean(nonRoadMVD$SCC.Level.Four == "Personal Water Craft")*100,
                           mean(nonRoadMVD$SCC.Level.Four == "Sailboat Auxiliary Inboard")*100,
                           mean(nonRoadMVD$SCC.Level.Four == "Sailboat Auxiliary Outboard")*100)

print(nonroadContribution)
nonRoadMVDContribution <- nonRoadMVD[mask,]

# To summarize
dim(onRoadMVD) #1137 MVD
dim(nonRoadMVD) #572 some are vechicles/vessels, the rest is noise.
dim(nonRoadMVDContribution) # So we get 57 items which are of relavance from nonRoad as MVD

# We now do a Row bind of onRoadMVD with nonRoadMVDContribution: we will get 1194 rows and 15 columns
totalmvdContribution <- rbind(onRoadMVD,nonRoadMVDContribution)
dim(totalmvdContribution)

#Now the data is ready to be analyzed to answer our question:  Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (
#fips == "06037"
#fips=="06037"). Which city has seen greater changes over time in motor vehicle emissions?

NEIFiltered <-  NEI[(NEI$SCC%in%totalmvdContribution$SCC),] # this gives us MVD subsetted by the TotalMVD: includes the Onroad and Non-Road types of relevance

#Make separate sets for Balti and LA from the total
typeyearsubsetBalti <- group_by(NEIFiltered, year)%>%filter(fips == "24510")
typeyearsubsetLA <- group_by(NEIFiltered, year)%>%filter(fips == "06037")
dim(typeyearsubsetBalti)
dim(typeyearsubsetLA)


#Combine these 2 into a new set
typeyearTotal <- rbind(typeyearsubsetBalti,typeyearsubsetLA)
dim(typeyearTotal)

typeYearTotalByCity <- group_by(typeyearTotal, fips, add = TRUE)
typeYearTotalByCity <- summarize_at(typeYearTotalByCity, .vars = c("Emissions") ,.funs = sum)

typeYearTotalByCity$fips <- gsub("06037","LosAngeles",typeYearTotalByCity$fips)
typeYearTotalByCity$fips <- gsub("24510","Baltimore",typeYearTotalByCity$fips)

dim(typeYearTotalByCity)
unique(typeYearTotalByCity$year)
unique(typeYearTotalByCity$fips)

currentpath <- rstudioapi::getSourceEditorContext()$path
path1 = gsub("plot6.r","plot6_skewed.png", currentpath)
png(path1,width = 480, height = 480)

# The data is too skewed in the normal scale
city <- as.factor(typeYearTotalByCity$fips)
p1 <- ggplot(typeYearTotalByCity, aes(year,Emissions))+geom_line(aes(color = city))+labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)"))
p1+labs(title=expression("Emission Baltimore Vs Los Angeles1999 - 2008"))
dev.off()
#switching to log scale to better see the trends

path2 = gsub("plot6.r","plot6.png", currentpath)
png(path2,width = 480, height = 480)
#log scale
city <- as.factor(typeYearTotalByCity$fips)
p2 <- ggplot(typeYearTotalByCity, aes(year,log2(Emissions)))+geom_line(aes(color = city))+labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)"))
p2+labs(title=expression("Emission Baltimore Vs Los Angeles1999 - 2008"))+scale_y_continuous(breaks = seq(6,14,1))

dev.off()


