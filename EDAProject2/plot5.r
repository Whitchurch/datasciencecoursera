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

#Now the data is ready to be analyzed to answer our question:


