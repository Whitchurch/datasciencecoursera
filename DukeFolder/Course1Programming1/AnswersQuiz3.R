library(statsr)
library(dplyr)
library(ggplot2)
library(gridExtra)

data("kobe_basket")
names(kobe_basket)
str(kobe_basket)
head(kobe_basket, n = 8)

#1) Hit

#2) miss

#3 )

kobe_streak <- calc_streak(kobe_basket$shot)
kobe_plot <- ggplot(data = kobe_streak, aes(x = length))+geom_histogram()
quantile(kobe_streak$length)
summarise(kobe_streak,meanstread = mean(length), medianstreak = median(length))

#Shortest streak is of length 1 is false, length 0 is the shortest streak as per our definition

# 4) 
coin_outcomes <- c("heads", "tails")

sim_fair_coin <- sample(coin_outcomes, size = 100, replace = TRUE)
table(sim_fair_coin)

sim_unfair_coin <- sample(coin_outcomes, size = 100, replace = TRUE, prob = c(0.2,0.8))
table(sim_unfair_coin)

# simulating an independent shooter
shot_outcome <- c("H","M")
sim_basket<- sample(shot_outcome, size = 133, replace = TRUE, prob = c(0.45,0.55))
table(sim_basket)

#look at the streak of the independent shooter
independent_shooter_streak <- calc_streak(sim_basket)
shooter_plpot <- ggplot(data = independent_shooter_streak, aes(x = length))+geom_histogram()
quantile(independent_shooter_streak$length)
summarise(independent_shooter_streak,meanstread = mean(length), medianstreak = median(length))

grid.arrange(kobe_plot,shooter_plpot, nrow = 2)
