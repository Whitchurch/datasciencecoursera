library(ggplot2)
library(dplyr)
library(gridExtra)
library(forcats)

path <- rstudioapi::getSourceEditorContext()$path
path <- gsub("scratchpad.R","brfss2013.RData",path)
load(path)

head(brfss2013)
unique(brfss2013$rrclass2)
brfss2013%>%select(c("exerany2","exract11","employ1","income2","marital","children"))
brfss2013%>%select(c("genhlth"))%>%summarise(total = n())

unique(brfss2013$genhlth)
table(brfss2013$genhlth)
sum(is.na(brfss2013$genhlth))
length(brfss2013$genhlth)
summary(brfss2013$genhlth)

# So we have genhealth categories: an ordinal variable.  there are few nas, so we can discard them.


#Check for relationship between adequate sleep and income:-
brfss2013$sleptim1
unique(brfss2013$sleptim1)
table(brfss2013$sleptim1)
summary(brfss2013$sleptim1)
ggplot(data= brfss2013, aes(x = sleptim1))+geom_histogram(bins = 24)

#Select the variables of interest: sleptim1(numeric), genhlth(Ordinal), income2(Oridnal), exerany2(categorical)
sleeptime <- brfss2013%>%select(c("sleptim1","genhlth","income2","exerany2"))%>%filter(sleptim1 >= 0, sleptim1 <= 24,!is.na(genhlth) )

#plot out each of the variables to check for Na's and to decide to drop the Na's if they are a small percentage.
plothealth <- ggplot(sleeptime, aes(x=genhlth))+geom_bar()
plotsleep <- ggplot(sleeptime, aes(x = sleptim1))+geom_histogram(bins = 24 )

plotincome <- ggplot(sleeptime, aes(fill = income2,x = income2))+geom_bar(position = "stack")

sleeptime$income2 <- fct_explicit_na(sleeptime$income2, na_level = "NotDisclosed")
#income seems to have a lot of NAs that might influence the trend.  So we decide to keep the NAs
# rename NAs to Notdiscosed
#sleeptime$income2 <- as.character(sleeptime$income2)
#sleeptime$income2[is.na(sleeptime$income2)] <- "NotDisclosed"
#sleeptime$income2 <- as.factor(sleeptime$income2, levels = c("NotDisclosed","Less than $10,000","Less than $15,000","Less than $20,000","Less than $25,000","Less than $35,000","Less than $50,000","Less than $75,000","$75,000 or more"))


sleeptime%>%group_by(income2)%>%summarise(countgp = n(), total = length(sleeptime$income2), percentage = (countgp/total)*100 )
#14% is not disclosed which is a high amount. So we cannot ignore it.
plotincome <- ggplot(sleeptime, aes(fill = income2,x = income2))+geom_bar(position = "stack",stat = "identity")

#plot out exercise to see if NAs can be ignored

sleeptime$exerany2 <- as.character(sleeptime$exerany2)
sleeptime$exerany2[is.na(sleeptime$exerany2)] <- "NotDisclosed"
sleeptime$exerany2 <- as.factor(sleeptime$exerany2)
plotexercis <- ggplot(sleeptime, aes(fill = exerany2,x=exerany2))+geom_bar()
sleeptime%>%group_by(exerany2)%>%summarise(countgp = n(), total = length(sleeptime$exerany2), percentage = (countgp/total)*100 )
# 6 percent is NotDisclosed for exercise this is a very small number, hence we can ignore it

sleeptime <- sleeptime%>%select(c("sleptim1","genhlth","income2","exerany2"))%>%filter(!(exerany2 == "NotDisclosed") )
plotexercis <- ggplot(sleeptime, aes(fill = exerany2,x=exerany2))+geom_bar()

# There seems to be an associative relationship between income and health
plotincomevshealth <- ggplot(sleeptime, aes(fill=genhlth, x = income2))+geom_bar(position = "fill")

# There is an associative relationship between exercise and health.
plotexercisvshealth <- ggplot(sleeptime, aes(fill = genhlth,x=exerany2))+geom_bar(position = "fill")

# Relationship between income and exercise
plotincomevsexercise<- ggplot(sleeptime, aes(fill = exerany2,x=income2))+geom_bar(position = "fill")

#Findings: There is an associative connection between: income&Health, exercise&health, income&exercise.
# Let us see what the relationship between these 3 would be:

p1 <- plotincomevshealth+facet_grid(.~exerany2)+coord_flip()
p2 <- plotexercisvshealth+facet_grid(.~income2)
grid.arrange(p1,p2, nrow = 2)
#Conclusion: It looks like higher income leads to better health; 

# However the exercise causes a greater benefit to health. 
# Drop in unhealthy people in the same income bracket is greater between those who workout versus those who don't 

#Investigate relationship between obesity and demographics
#Variables used: weight, height, marital.
#Variables created: inches = height*12   ;BMI = (703*weight)/(inches)

#Select maritalStatus, weight and height
frameofinterest <- brfss2013%>%select(c("weight2","height3","marital","X_state","income2"))
#Check and decided if NAs need to be dropped
unique(frameofinterest$marital)
#frameofinterest$marital <- as.character(frameofinterest$marital)
#frameofinterest$marital[is.na(frameofinterest$marital)] <- "NotDisclosed"
#frameofinterest$marital <- as.factor(frameofinterest$marital)
frameofinterest$marital <- fct_explicit_na(frameofinterest$marital, na_level = "NotDisclosed")
frameofinterest%>%group_by(marital)%>%summarise(countgp = n(), total = length(frameofinterest$marital), percentage = (countgp/total)*100 )

# very little NAS so it is safe to drop them
frameofinterest <- frameofinterest%>%select(c("weight2","height3","marital","X_state","income2"))%>%filter(!(marital == "NotDisclosed"))
ggplot(frameofinterest, aes(x=marital))+geom_bar()

sum(is.na(frameofinterest$weight2)) # no NAS
sum(is.na(frameofinterest$height3)) # 6670 / 488355 NAS is roughly only 1.3% so we can drop those rows.
unique(frameofinterest$height3)
length(frameofinterest$height3)

# We see the strange values starting with 9 are values entered in cms, with 9 to indicate that fact: 9 followed bt cm value.

# we find these values are only 0.3 percent of our reading, so we drop them, instead of spending time processing them.

#Convert Feet to inches
frameofinterest <- frameofinterest%>%select(c("weight2","height3","marital","X_state","income2"))%>%filter(!is.na(height3))%>%filter(height3 >= 200, height3 <= 711)%>%
  mutate(inches = height3/100)%>%mutate(inches = gsub("0","",inches))%>%mutate(inches = as.double(inches))%>%mutate(inches = inches*12)

# Now we have values strictly in feet and inches, we proceed to deal with these now.
frameofinterest <- frameofinterest%>%select(c("weight2","height3","marital","inches","X_state","income2"))%>%mutate(weight2 = as.character(weight2))%>%mutate(weight2 = as.double(weight2))%>%
  mutate(bmi = (703*weight2)/(inches*inches))%>%filter(!is.na(bmi))
#dropping NAs as they constitute only 3.2 percent of the data.

frameofinterest$income2 <- fct_explicit_na(frameofinterest$income2, na_level = "NotDisclosed")

frameofinterest$bmicat <- frameofinterest$bmi
frameofinterest$bmicat[frameofinterest$bmicat < 18.5] <- 0
frameofinterest$bmicat[frameofinterest$bmicat >= 18.5 & frameofinterest$bmicat < 25] <- 1
frameofinterest$bmicat[frameofinterest$bmicat >= 25 & frameofinterest$bmicat < 30] <- 2
frameofinterest$bmicat[frameofinterest$bmicat >= 30] <- 3

frameofinterest$bmicat <- as.character(frameofinterest$bmicat)

frameofinterest$bmicat[frameofinterest$bmicat == "0"] <- "Underweight"
frameofinterest$bmicat[frameofinterest$bmicat == "1"] <- "Normalweight"
frameofinterest$bmicat[frameofinterest$bmicat == "2"] <- "Overweight"
frameofinterest$bmicat[frameofinterest$bmicat == "3"] <- "Obese"


#Plot distrobution of BMI over the country, 
summary(frameofinterest$bmi)
ggplot(frameofinterest, aes(x = bmicat, fill = bmicat))+geom_bar(stat = "count")+facet_wrap(~X_state, nrow = 6)+coord_flip()

#economy over the country
ggplot(frameofinterest, aes(x = income2, fill = income2))+geom_bar(stat = "count")+facet_wrap(~X_state, nrow = 6)+coord_flip()


#Check for any interesting pattern between demographic and BMI
ggplot(frameofinterest, aes(fill = bmicat,x=marital))+geom_bar(position = "fill")


#Last analysis, any relationship between income and bmi
ggplot(frameofinterest, aes(fill = bmicat, x = income2))+geom_bar(position = "fill")+coord_flip()

#mean bmi versus median BMi per state
sBMI <- frameofinterest%>%group_by(X_state)%>%summarise(meanBMIt = mean(bmi),medianBMI = median(bmi))
#ggplot(sBMI,aes(x= medianBMI,y=meanBMIt, col=X_state))+geom_point()+facet_wrap(~bmicat)

ggplot(sBMI,aes(x= medianBMI,y=meanBMIt, label=X_state))+geom_point()+geom_text(aes(label=X_state),hjust=1, vjust=0)

#Texas and Newyork are outliers.  It seems that they have a right skew with few really obese people, that is shifting the mean > median BMI.

TexasBMI <- frameofinterest%>%group_by(X_state)%>%filter(X_state == "Texas")%>%arrange(desc(bmi))
NewYorkBMI <- frameofinterest%>%group_by(X_state)%>%filter(X_state == "New York")
CaliforniaBMI <- frameofinterest%>%group_by(X_state)%>%filter(X_state == "California")

hist(TexasBMI$bmi, breaks = 1000, xlim = c(0,100))+abline(v = median(TexasBMI$bmi))
hist(CaliforniaBMI$bmi, breaks = 1000, xlim = c(0,100))+abline(v = median(CaliforniaBMI$bmi))
quantile(TexasBMI$bmi)
quantile(CaliforniaBMI$bmi)
#Plot histograms of BMI distributions for Newyork and texas


analysis3 <- brfss2013%>%select(c("income2","menthlth"))
analysis3$income2 <- fct_explicit_na(analysis3$income2, na_level = "NotDisclosed")
analysis3$menthlth <- as.factor(analysis3$menthlth)
analysis3$menthlth <- fct_explicit_na(analysis3$menthlth, na_level = "Not Shared")

table(analysis3)

ggplot(analysis3, aes(fill = menthlth,x=income2))+geom_bar(position = "fill")+coord_flip()+ggtitle("Income Vs Days with Bad MentalHealth")+xlab("income")+ylab("Bad days")++guides(fill=guide_legend(title="Bad days"))



