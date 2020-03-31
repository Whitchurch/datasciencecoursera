library(statsr)
library(dplyr)
library(ggplot2)

data("atheism")
str(atheism)

atheism%>%select(c("nationality","response","year"))%>%filter(year == 2012, nationality == "United States")%>%group_by(response)%>%summarise(percent = n())

us12 <- atheism %>%
  filter(nationality == "United States" , atheism$year == "2012")

inference(y = response, data = us12, statistic = "proportion", type = "ci", method = "theoretical", success = "atheist")

d <- data.frame(p <- seq(0, 1, 0.01))
n <- 1000
d <- d %>%
  mutate(me = 1.96*sqrt(p*(1 - p)/n))
ggplot(d, aes(x = p, y = me)) +
  geom_line()

#inference for Spain
spainData <- atheism%>%select(c("nationality","response","year"))%>%filter(nationality == "Spain")%>%group_by(year,response)%>%summarise(n())
spainData1 <- atheism%>%select(c("nationality","response","year"))%>%filter(nationality == "Spain")
# patheist2005 <- 115/(115+1031)
# patheist2012 <- 103/(103+1042)
# 
# pathesitdifference <- patheist2005 - patheist2012
inference(y = response, x = as.factor(year),data = spainData1, statistic = "proportion", type = "ht", method = "theoretical", success = "atheist", null = 0,alternative = "twosided")
inference(y = response, x = as.factor(year), data = spainData1, statistic = "proportion", type = "ht", null = 0, success = "atheist", alternative = "twosided", method = "theoretical")


#inference US
USData1 <- atheism%>%select(c("nationality","response","year"))%>%filter(nationality == "United States")
inference(y = response, x = as.factor(year), data = USData1, statistic = "proportion", type = "ht", null = 0, success = "atheist", alternative = "twosided", method = "theoretical")
