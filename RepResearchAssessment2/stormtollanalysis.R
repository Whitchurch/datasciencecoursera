library(dplyr)
library(ggplot2)
library(colorspace)
library(gridExtra)

#====== Begin by loading the data=====
path <- rstudioapi::getActiveDocumentContext()$path
pathzip <- gsub("stormtollanalysis.R","",path)
setwd(pathzip)
datatoanalyze <- read.csv("repdata-data-StormData.csv.bz2", header = TRUE, sep = ",")

#== Analyze the structure of the dataset
str(datatoanalyze)
head(datatoanalyze)
with(datatoanalyze, unique(EVTYPE)) #The various event types
with(datatoanalyze, unique(FATALITIES)) # The Fatalities
with(datatoanalyze, unique(INJURIES)) # Injuries 
with(datatoanalyze, unique(PROPDMGEXP)) # Units K, M, B, K = thousand, M = Million, B= bilion
with(datatoanalyze, unique(CROPDMGEXP)) #units for cop damage

#== Pre-process: Cleaning up the data

#Part 1: Removing the aliases for the event names, and associating them with the NOAA list of approvent events

#== Create the list of valid event types as per the NOAA standards.
listofpermittedEvents <- c("Astronomical Low Tide", "Avalanche",  "Blizzard",  "Coastal Flood",  "Cold/Wind Chill",  "Debris Flow", "Dense Fog",  "Dense Smoke", "Drought",  "Dust Devil", "Dust Storm", "Excessive Heat", "Extreme Cold/Wind Chill", "Flash Flood", "Flood", "Frost/Freeze", "Funnel Cloud", "Freezing Fog", "Hail" , "Heat" , "Heavy Rain" , "Heavy Snow", "High Surf", "High Wind")
listofpermittedEvents <- c(listofpermittedEvents, "Hurricane/Typhoon", "Ice Storm", "Lake-Effect", "Snow", "Lakeshore Flood",  "Lightning", "Marine Hail", "Marine High Wind", "Marine Strong Wind", "Marine Thunderstorm Wind", "Rip Current", "Seiche", "Sleet", "Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind", "Tornado", "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", "Waterspout",  "Wildfire",  "Winter Storm", "Winter Weather")

datatoanalyze$EVTYPE <- as.character(datatoanalyze$EVTYPE)
length(unique(datatoanalyze$EVTYPE))

#=== Table of events before sorting
sort(table(datatoanalyze$EVTYPE))

#== Search list of events and print out EventTypes that are not part of the valid event types, these are the aliases
for(i in sort(unique(datatoanalyze$EVTYPE)))
{
  if(identical(grep(paste0("^",i,"$") , listofpermittedEvents ,ignore.case = T, value=TRUE),character(0)))
  {
    print(i)
  }
  
}

#=== Combine tose non-NOAA events into NOAA event types.
datatoanalyze[grep("(^TSTM)",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Thunderstorm Wind" 
datatoanalyze[grep("(^Thunder|thunderstorm)",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Thunderstorm Wind" 
datatoanalyze[grep("(^Thu)",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Thunderstorm Wind" 
datatoanalyze[grep("^hurr",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Hurricane/Typhoon" 
datatoanalyze[grep("^typ",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Hurricane/Typhoon" 
datatoanalyze[grep("(^heavy snow)",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Heavy Snow" 
datatoanalyze[grep("(^heavy rain)",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Heavy Rain" 
datatoanalyze[grep("(^hail|hail)",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Hail" 
datatoanalyze[grep("(^high wind)",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "High Wind" 
datatoanalyze[grep("(^tornado|^TORNDAO)",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Tornado" 
datatoanalyze[grep("(^rip)",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Rip Current" 
datatoanalyze[grep("(^wild)",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Wildfire"
datatoanalyze[grep("(^summary)",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Summary"
datatoanalyze[grep("(^lightn)",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Lightning"
datatoanalyze[grep("(^winter weather)",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Winter Weather"
datatoanalyze[grep("(^volcanic)",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Volcanic Ash"
datatoanalyze[grep("(^winter stor|^snow)",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Winter Storm"
datatoanalyze[grep("(flood)",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Flood"
datatoanalyze[grep("^water",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Waterspout"
datatoanalyze[grep("mix$",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Sleet"
datatoanalyze[grep("wind",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Strong Wind"
datatoanalyze[grep("Ice|Icy",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Ice Storm"
datatoanalyze[grep("heat|warm|hot",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Heat"
datatoanalyze[grep("coast",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Coastal Flood"
datatoanalyze[grep("cold",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Extreme Cold/Wind Chill"
datatoanalyze[grep("tropical s",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Tropical Storm"
datatoanalyze[grep("tropical d",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Tropical Depression"
datatoanalyze[grep("snow",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Heavy Snow"
datatoanalyze[grep("rain|slide",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Heavy Rain"
datatoanalyze[grep("og$",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Dense Fog"
datatoanalyze[grep("smoke",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Dense Smoke"
datatoanalyze[grep("freez",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Frost/Freeze"
datatoanalyze[grep("surf",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "High Surf"
datatoanalyze[grep("surge",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Storm Surge/Tide"
datatoanalyze[grep("funnel",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Funnel Cloud"
datatoanalyze[grep("fld",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Flood"
datatoanalyze[grep("blizzard",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Blizzard"
datatoanalyze[grep("wind$|WND$",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Strong Wind"
datatoanalyze[grep("HYPERTHERMIA/EXPOSURE|hypothermia|RECORD LOW|RECORD COOL|RECORD PRECIPITATION",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Extreme Cold/Wind Chill"
datatoanalyze[grep("DRY SPELL|DRY WEATHER|RECORD HIGH TEMPERATURE|RECORD HIGH|HIGH TEMPERATURE RECORD|RECORD DRYNESS|Record dry month|Temperature record|Record temperature|Record temperatures",datatoanalyze$EVTYPE, ignore.case = T),"EVTYPE"] <- "Excessive Heat"

#=== Newly reduced list of events
sort(table(datatoanalyze$EVTYPE))


#Part 2: Processing the Property and Crop Damages, by changing the EXP to their respective exponents.
# and then multiplying the exponents with the values to get the total values

datatoanalyze$PROPDMGEXP <- as.character(datatoanalyze$PROPDMGEXP)
listofPROPDMGEXP <- unique(datatoanalyze$PROPDMGEXP)

datatoanalyze$CROPDMGEXP <- as.character(datatoanalyze$CROPDMGEXP)
listofCROPDMGEXP <- unique(datatoanalyze$CROPDMGEXP)

#=== Creating Crop damage multiplier.
datatoanalyze$CROPDMGMULTIPLIER[datatoanalyze$CROPDMGEXP == ""] <- 0
datatoanalyze$CROPDMGMULTIPLIER[datatoanalyze$CROPDMGEXP == "M"] <- 1000000
datatoanalyze$CROPDMGMULTIPLIER[datatoanalyze$CROPDMGEXP == "K"] <- 1000
datatoanalyze$CROPDMGMULTIPLIER[datatoanalyze$CROPDMGEXP == "m"] <- 1000000
datatoanalyze$CROPDMGMULTIPLIER[datatoanalyze$CROPDMGEXP == "B"] <- 1000000000
datatoanalyze$CROPDMGMULTIPLIER[datatoanalyze$CROPDMGEXP == "?"] <- 0
datatoanalyze$CROPDMGMULTIPLIER[datatoanalyze$CROPDMGEXP == "0"] <- 1
datatoanalyze$CROPDMGMULTIPLIER[datatoanalyze$CROPDMGEXP == "k"] <- 1000
datatoanalyze$CROPDMGMULTIPLIER[datatoanalyze$CROPDMGEXP == "2"] <- 100

#== creating Property Damage Multiplier
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP ==  "K"    ]  <-    1000
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP == "M"     ]   <-  1000000
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP == ""      ]   <-  0
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP == "B"     ]   <-  1000000000
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP == "m"     ]   <-  1000000
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP == "+"     ]   <-  0
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP == "0"     ]   <-  1
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP == "5"     ]   <-  100000
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP == "6"     ]   <-  1000000
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP == "?"     ]   <-  0
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP == "4"     ]   <-  10000
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP == "2"     ]   <-  100
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP == "3"     ]   <-  1000
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP == "h"     ]   <-  100
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP == "7"     ]   <-  10000000
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP == "H"     ]   <-  100
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP == "-"     ]   <-  0
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP =="1"     ]   <-  10
datatoanalyze$PROPDMGMULTIPLIER[datatoanalyze$PROPDMGEXP =="8"     ]   <-  100000000

datatoanalyze$PROPERTYDMGVAL <- datatoanalyze$PROPDMG * datatoanalyze$PROPDMGMULTIPLIER
datatoanalyze$CROPDMGVAL <- datatoanalyze$CROPDMG * datatoanalyze$CROPDMGMULTIPLIER



#RESULTS PART1: Which EVENT TYPES CAUSE THE GREATEST HEALTH TOLL

# == Plotting out the information for health toll
#== Check for NAs. in Injusties and Fatalities. There are none.
sum(as.integer(is.na(datatoanalyze$FATALITIES)))
sum(as.integer(is.na(datatoanalyze$INJURIES)))

#=== we will combine the injuries and Fatalities into a common variable called healthToll
datatoanalyze$healthtoll <- datatoanalyze[,"FATALITIES"] + datatoanalyze[,"INJURIES"]

#== we wil look only at subset of rows with healthtoll(FATALITIES+INJURIES) with greather than 0 occurences
OnlyDataWithhealthToll <- datatoanalyze[datatoanalyze$healthtoll > 0,]
table(OnlyDataWithhealthToll$healthtoll)
hist(log2(OnlyDataWithhealthToll$healthtoll))

length(OnlyDataWithhealthToll$EVTYPE) #total rows is 21929
length(unique(OnlyDataWithhealthToll$EVTYPE)) # there are 220 unique event types


##=== We now group the events and summarize 

groupbyEVTYPE <- group_by(OnlyDataWithhealthToll,EVTYPE)
groupbyEVTYPE <- summarise_at(groupbyEVTYPE, .vars = c("healthtoll"),sum)%>%arrange(desc(healthtoll))



##==== Results:
# Log scale
ggplot(data = groupbyEVTYPE,aes(x=reorder(EVTYPE, (log2(healthtoll))),y =log2(healthtoll)))+geom_bar(stat = "identity",fill = rainbow(n=length(groupbyEVTYPE$EVTYPE)))+coord_flip()+xlab("Events") + ylab("Fatalities/Injuries")+ggtitle("Breakup of Events causing maximum health toll")

ggplot(data = groupbyEVTYPE,aes(x=reorder(EVTYPE, healthtoll),y = healthtoll))+geom_bar(stat = "identity",fill = rainbow(n=length(groupbyEVTYPE$EVTYPE)))+coord_flip()+xlab("Events") + ylab("Fatalities/Injuries")+ggtitle("Breakup of Events causing maximum health toll")


#RESULTS PART1: Which EVENT TYPES ARE OF GREATEST ECONOMIC CONSEQUENCE
OnlyDataWithPropertydmg <- datatoanalyze[datatoanalyze$PROPDMG > 0,]
groupbyEVTYPE1 <- group_by(OnlyDataWithPropertydmg,EVTYPE)
groupbyEVTYPE1 <- summarise_at(groupbyEVTYPE1, .vars = c("PROPERTYDMGVAL"), sum)%>%arrange(desc(PROPERTYDMGVAL))

OnlyDataWithCropdmg <- datatoanalyze[datatoanalyze$CROPDMG > 0,]
groupbyEVTYPE2 <- group_by(OnlyDataWithCropdmg,EVTYPE)
groupbyEVTYPE2 <- summarise_at(groupbyEVTYPE2, .vars = c("CROPDMGVAL"), sum)%>%arrange(desc(CROPDMGVAL))


plot1 <- ggplot(data = groupbyEVTYPE1[1:10,],aes(x=reorder(EVTYPE, (PROPERTYDMGVAL)),y = PROPERTYDMGVAL))+geom_bar(stat = "identity",fill = rainbow(n=length(groupbyEVTYPE1[1:10,]$EVTYPE)))+coord_flip()+xlab("Events") + ylab("Damage")+ggtitle("Top 10 Events for Property Damage")
plot2 <- ggplot(data = groupbyEVTYPE2[1:10,],aes(x=reorder(EVTYPE, (CROPDMGVAL)),y = CROPDMGVAL))+geom_bar(stat = "identity",fill = rainbow(n=length(groupbyEVTYPE2[1:10,]$EVTYPE)))+coord_flip()+xlab("Events") + ylab("Damage")+ggtitle("Top 10 Events for Crop Damage")

grid.arrange(plot1,plot2, nrow = 2)



