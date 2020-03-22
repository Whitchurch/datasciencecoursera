library(statsr)
library(dplyr)
library(ggplot2)

data(nc)

str(nc)

#Analysis of age of mothers
ggplot(data = nc, aes(x = mage))+geom_histogram()
summary(nc$mage)
mean(nc$mage)
ggplot(data = nc, aes(x = mage))+geom_bar()
table(nc$mage)

#Mean age is 27, MEadian age is: 27.
#From the table it can be seen that 20 year olds have the highest births.
# As women approach ther late late 30s number of pregant mother's decreases.

#Analysis of mothers and fathers age as a scatter plot
ggplot(data = nc, aes(x = mage, y = fage))+geom_point()+geom_smooth()
ggplot(data = nc, aes(x = fage, y = mage))+geom_point()+geom_smooth()

#An interesint trend emerges, barring a few guys, most guys who are really old have really young pregnant wife's.
#The curve seems to be lineat from 20 -30 years for fathers to overwheliming flatten out at the age of 40 to 49 for guys before increasing.
# That flat secton means that guys between 40  to 49 have younger wife's who are pregnant or older ladies have a hard time getting pregant after 40.

str(nc$gained)
ggplot(data = nc, aes(x = gained))+geom_histogram()
ggplot(data = nc, aes(x = gained))+geom_bar()
table(nc$gained)
summary(nc$gained)

#Plots of weight of baby and mon's smoking habits
ggplot(data = nc, aes(x = habit, y = weight ))+geom_boxplot()

nc %>%
  group_by(habit) %>%
  summarise(mean_weight = mean(weight), median_weight = median(weight))

# mean < median, so clearily both are left skewed. So it is false to claim both distributins are right skewed.

#Run a hypothesis test to see if there is any difference between mean weights of babies born to smokers versus non-smoker moms
inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical")

# Get the 99% CI for average length of pregnanices weeks
inference(y = weeks, data = nc, statistic = "mean", type = "ci", conf_level = 0.99,
          alternative = "twosided", method = "theoretical")

#USe groupby function for moms 
nc%>%select(mage,mature)%>%group_by(mature)%>%summarise(max(mage), min(mage))


#hypothesis test if average weight gain  of younger mothers is different from average weight gain of older mothers
inference(y = gained, x = mature, data = nc, statistic = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical")
inference(y = gained,  data = nc, statistic = "mean", type = "ci", 
          alternative = "twosided", method = "theoretical")

#The average weight gained by young mothers and mature mothers is equal, there is no noticable difference

#Run a hypothesis test to see if there is any difference between mean weights of babies born to smokers versus non-smoker moms
inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical")

#Calculate 95% confidence interval
inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ci", null = 0, 
          alternative = "twosided", method = "theoretical")

#Run hypothesis on whether being white vesus nonwhite has any influence on motherws age
inference(y = mage, x = whitemom, data = nc, statistic = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical")

#Calculate onfidence interval
inference(y = mage, x = whitemom, data = nc, statistic = "mean", type = "ci", null = 0, 
          alternative = "twosided", method = "theoretical")

#See if there is difference in mean age between white and non-white mons
nc%>%select(mage,whitemom)%>%group_by(whitemom)%>%summarise(mean(mage), mean(mage))

#plot a box plot to be doubly sure
ggplot(data = nc, aes(x = whitemom, y = mage ))+geom_boxplot()

#Conclusion: There is overwhelimg evidence that race(white vs non-white) plays a huge role, so there is a difference between 
# mean ages of white and non-white ewomen , non-white women get pregnant earlier.
