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

DailycoronaGrowthRate <- function(absoluteNumbers){
  growthfactor <- NULL
  absgrowth <- NULL
  for(i in 1:length(absoluteNumbers))
  {
    #print((absoluteNumbers[i]-absoluteNumbers[i+1]/absoluteNumbers[i+1])/absoluteNumbers[i+1])
    #growthfactor[i] <- ((absoluteNumbers[i]-absoluteNumbers[i+1]))/(absoluteNumbers[i+1]-absoluteNumbers[i+2])  
    growthfactor[i] <- ((absoluteNumbers[i]-absoluteNumbers[i+1]))/(absolutenumbers[i])
  }
  
  return (rev(growthfactor))
}

#absolutenumbers <- c(657,536,499,396,330,244,194,156,142,119,113,102,82,73,62,56,43,39,34,31,30,28,5,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,1,1,1)
absolutenumbers <- c(657,536,499,396,330,244,194,156,142,119,113,102,82,73,62,56,43,39,34,31,30,28,5,3)

growthrate <- na.exclude(coronaGrowth(absolutenumbers))
days <- seq(1:length(growthrate))
dataTrend <- data.frame(GrowthRate = growthrate,Days = days)
print(dataTrend)
p1 <- ggplot(data=dataTrend, aes(x=Days, y=GrowthRate, group=1)) +ggtitle("India: Rate of change in Growth Rate")+
  geom_line()+
  geom_point()

DailyGrowthRate <- na.exclude(DailycoronaGrowthRate(absolutenumbers))
days_1 <- seq(1:length(DailyGrowthRate))
dataTrend_1 <- data.frame(DailyGrowthRate = DailyGrowthRate,Days = days_1)
print(dataTrend_1)
p2 <- ggplot(data=dataTrend_1, aes(x=Days, y=DailyGrowthRate, group=1)) +ggtitle("India: Daily Growth Rate")+
  geom_line()+
  geom_point()

# Growth factor is the factor by which a quantity multiplies itself over time. The formula used is every day's new cases / new cases on the previous day. 
#For example, a quantity growing by 7% every period (in this case daily) has a growth factor of 1.07.
# 
# A growth factor above 1 indicates an increase, whereas one which remains between 0 and 1 it is a sign of decline, with the quantity eventually becoming zero, 
#whereas a growth factor constantly above 1 could signal exponential growth


#Plot current cases versus predicted cased by growthfactor
days <- seq(1:length(absolutenumbers))
growthrate <- append(growthrate, c(1,1),after = 0)
# prediction <- NULL
# if(growthrate >1)
# {
#   prediction = growthrate * rev(absolutenumbers)
# }else{
#   prediction = (1-dataOriginal[length(absolutenumbers),"growthrate"])*dataOriginal[length(absolutenumbers),"Cases"]+dataOriginal[length(absolutenumbers),"Cases"]
# }
dataOriginal <- data.frame(Cases = rev(absolutenumbers), Days = days,growthrate = growthrate ,Prediction = growthrate * rev(absolutenumbers))
p3<- ggplot() +
  geom_line(data=dataOriginal, aes(x=Days, y=Cases,),color = "Red")+labs(title="Cases(Red) Vs Predicted(Blue)", y="Cases", x="Days")+
  #geom_line(data=dataOriginal, aes(x=Days+1, y=Prediction),color = "Blue")
geom_point()

print(dataOriginal)
print(dataTrend_1)
if(dataOriginal[length(absolutenumbers),"growthrate"] > 1)
{
  print(dataOriginal[length(absolutenumbers),"growthrate"]*dataOriginal[length(absolutenumbers),"Cases"])
}else{
  print("Total Cases tomorrow(India):")
  print((1-dataOriginal[length(absolutenumbers),"growthrate"])*dataOriginal[length(absolutenumbers),"Cases"]+dataOriginal[length(absolutenumbers),"Cases"])
}
grid.arrange(p1,p2,p3,nrow = 2)


