library(statsr)
library(dplyr)
library(shiny)
library(ggplot2)

# Create the population to sample from:
pop_size <- 48

# Add in the Male Female into the population , equal number of males and females as per study setup
populationwithGender <- data.frame(gender = c(rep("M",0.50*pop_size), rep("F",0.50*pop_size)))
str(populationwithGender)
summary(populationwithGender)

#Randomly select 35 for promotion, irrespecive of Gender, with replacement,
diiference_35 <- populationwithGender %>%rep_sample_n(size = 35, reps = 100, replace = TRUE)
difference <- diiference_35%>%filter(gender == "M")%>%summarise(Male = (n()/24))%>%select(Male) - diiference_35%>%filter(gender == "F")%>%summarise(Female = (n()/24))%>%select(Female)

str(difference)
#Plot out the sampling distribution, of difference over 1500 samples of size 35.
ggplot(difference,aes(x = Male))+geom_histogram()
summary(difference)
difference%>%summarise(meanvalue = mean(Male))

#It looks like the null hypothesis is true , and the difference is purely by chance.

