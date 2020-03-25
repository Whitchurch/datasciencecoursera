library(ggplot2)

coronaGrowth <- function(absoluteNumbers){
  growthfactor <- NULL
for(i in 1:length(absoluteNumbers))
{
  #print((absoluteNumbers[i]-absoluteNumbers[i+1]/absoluteNumbers[i+1])/absoluteNumbers[i+1])
  growthfactor[i] <- ((absoluteNumbers[i]-absoluteNumbers[i+1]))/(absoluteNumbers[i+1]-absoluteNumbers[i+2])  
}

   return (rev(growthfactor))
}

absolutenumbers <- c(326,234,152,104,71)

growthrate <- na.exclude(coronaGrowth(absolutenumbers))
days <- seq(1:length(growthrate))

dataTrend <- data.frame(GrowthRate = growthrate, Days = days)
print(dataTrend)
ggplot(data=dataTrend, aes(x=Days, y=GrowthRate, group=1)) +ggtitle("Growth Factor Corona cases in Arizona")+
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
dataOriginal <- data.frame(Cases = rev(absolutenumbers), Days = days,growthrate = growthrate ,Prediction = growthrate * rev(absolutenumbers))
ggplot() +
  geom_line(data=dataOriginal, aes(x=Days, y=Cases),color = "Red")+labs(title="Cases(Red) Vs Predicted(Blue)", y="Cases", x="Days")+
  geom_line(data=dataOriginal, aes(x=Days+1, y=Prediction),color = "Blue")
  geom_point()
  
  
  
  
