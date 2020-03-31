library(ggplot2)
library(dplyr)
library(statsr)
library(forcats)
load("gss.Rdata")

names(gss)
gss%>%select(c("income06","coninc","relig","owngun","race"))
table(gss$owngun)
table(gss$race)
table(gss$relig)
table(gss$income06)
gss$coninc

#Steps to get the required data
# 1) Select the Race and OwnGuns questions , and print a summary
gunsandrace <- gss%>%select(c("race","owngun"))
grouped <- gunsandrace%>%group_by(owngun,race)%>%summarise(Total = n(), percentage = (n()/length(gunsandrace$owngun))*100)
grouped$owngun <- fct_explicit_na(grouped$owngun, na_level = "NotAvailable")

#Visualize the categorical variables next:-
ggplot(gunsandrace,aes(race))+geom_bar(aes(fill=owngun), position = "fill")+ 
  labs(title="Gun-ownership by Race") 
