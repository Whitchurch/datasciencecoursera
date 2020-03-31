library(ggplot2)
library(gridExtra)

coronaGrowth <- function(absoluteNumbers){
  growthfactor <- NULL
  absgrowth <- NULL
  for(i in 1:length(absoluteNumbers))
  {
    #print((absoluteNumbers[i]-absoluteNumbers[i+1]/absoluteNumbers[i+1])/absoluteNumbers[i+1])
    growthfactor[i] <- ((absoluteNumbers[i]-absoluteNumbers[i+1]))/(absoluteNumbers[i+1]-absoluteNumbers[i+2])  
    #growthfactor[i] <- ((absoluteNumbers[i]-absoluteNumbers[i+1]))/(absolutenumbers[i])
  }
  
  return (rev(growthfactor))
}

DailyGrowthRate <- function(absoluteNumbers)
{
  growthfactor <- NULL
  absgrowth <- NULL
  for(i in 1:length(absoluteNumbers))
  {
    #print((absoluteNumbers[i]-absoluteNumbers[i+1]/absoluteNumbers[i+1])/absoluteNumbers[i+1])
    growthfactor[i] <- ((absoluteNumbers[i]/absoluteNumbers[i+1]))-1 
    #growthfactor[i] <- ((absoluteNumbers[i]-absoluteNumbers[i+1]))/(absolutenumbers[i])
  }
  
  return (rev(growthfactor))  
}

absolutenumbers <- c(886,727,657,536,499,396,330,244,194,156,142,119,113,102,82,73,62,56,43,39,34,31,30,28,5,3)

growthrate <- na.exclude(coronaGrowth(absolutenumbers))
days <- seq(1:length(growthrate))
dataTrend <- data.frame(GrowthRate = growthrate,Days = days)
print(dataTrend)
p1 <- ggplot(data=dataTrend, aes(x=Days, y=GrowthRate, group=1)) +ggtitle("India: Rate of change in Growth Rate")+
  geom_line()+
  geom_point()

print(dataTrend)
#print(dataTrend1)




growthrate1 <- na.exclude(DailyGrowthRate(absolutenumbers))
days1 <- seq(1:length(growthrate1))
dataTrend1 <- data.frame(GrowthRate = growthrate1,Days = days1)
print(dataTrend1)
multiplier <- tail(dataTrend1, n = 1)
pred_val <- (head(absolutenumbers, n = 1)*multiplier[,"GrowthRate"])+head(absolutenumbers, n = 1)

p2 <- ggplot(data=dataTrend1, aes(x=Days, y=GrowthRate, group=1)) +ggtitle("India: Growth Rate",pred_val)+
  geom_line()+
  geom_point()

days <- seq(1:length(absolutenumbers))
growthrate <- append(growthrate, c(1,1),after = 0)
dataOriginal <- data.frame(Cases = rev(absolutenumbers), Days = days)
p3<- ggplot() +geom_line(data=dataOriginal, aes(x=Days, y=Cases,),color = "Red")+labs(title="India: Daily Cases", y="Cases", x="Days")+
  #geom_line(data=dataOriginal, aes(x=Days+1, y=Prediction),color = "Blue")
  geom_point()


# Analyze growth rate to get interval of values
summary(dataTrend1$GrowthRate)
dataTrend1%>%select("GrowthRate")%>%summarise(meanGR = mean(GrowthRate), medianGR = median(GrowthRate), sdGR = sd(GrowthRate))
p4 <- ggplot(data = dataTrend1, aes(x = GrowthRate))+ggtitle("Histogram of GrowthRate")+geom_histogram()

#Confidence Interval for mean population GrowthRate:
meanGR = mean(dataTrend1$GrowthRate)
sdGR = sd(dataTrend1$GrowthRate)

uppperlimitGR= meanGR+sdGR
lowerlimitGR = meanGR-sdGR

#Predictions based on: Current Growth Rate, MeanGrowth Rate, Upper GrowthRate, Lower GrowthRate
pred_val_Current <- (head(absolutenumbers, n = 1)*multiplier[,"GrowthRate"])+head(absolutenumbers, n = 1)
pred_val_MeanGR <- (head(absolutenumbers, n = 1)*meanGR)+head(absolutenumbers, n = 1)
pred_val_upperGR <- (head(absolutenumbers, n = 1)*uppperlimitGR)+head(absolutenumbers, n = 1)
pred_val_lowerGR <- (head(absolutenumbers, n = 1)*lowerlimitGR)+head(absolutenumbers, n = 1)


print("Current GR:")
print(multiplier[,"GrowthRate"])
print(pred_val_Current)

print("Mean GR:")
print(meanGR)
print(pred_val_MeanGR)

print("Upperlimit GR:")
print(uppperlimitGR)
print(pred_val_upperGR)

print("Lowerlimit GR:")
print(lowerlimitGR)
print(pred_val_lowerGR)

grid.arrange(p3,p1,p2,p4,nrow = 4)







