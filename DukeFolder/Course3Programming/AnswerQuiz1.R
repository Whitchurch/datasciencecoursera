library(statsr)
library(dplyr)
library(ggplot2)
library(gridExtra)

data(mlb11)
str(mlb11)

#Answer1: Scatterplot

#Answer: Scatter plot of runs vs at_bats

p1 <- ggplot(data = mlb11, aes(x=at_bats,y=runs),color="red")+geom_point()
print(p1)
p1+stat_smooth(method = "lm", se = FALSE)

#Bonus: use Correlation coefficient to test the lineratiy
mlb11%>%summarise(cor(runs,at_bats))
#Corr coefficient is : 0.611
#R squared, percentage of variabliltiy in runs explained by at runs is: 0.373321 ot applox 37 percent.

#Fit a least square line to the data using the lm function
ml <- lm(runs~at_bats,data = mlb11)
summary(ml)

p2 <- ggplot(data = mlb11, aes(x=homeruns,y=runs))+geom_point()
print(p2)
p2+stat_smooth(method = "lm", se = FALSE)


ml_1 <- lm(runs~homeruns, data=mlb11)
summary(ml_1)



#Observed at_bats 5579
mlb11 %>%
  filter(at_bats == 5579) %>%
  select(runs)

#Model diagnonists" Check: Linearity, notmal distribtuin of residuals(hist/Q-Q plot), Constant variability

#Linearity: via residual plot.

ggplot(data = ml, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

#NEarly Normal residuals
ggplot(data = ml, aes(x = .resid)) +
  geom_histogram(binwidth = 24) +
  xlab("Residuals")
#QQ-plot
ggplot(data = ml, aes(sample = .resid)) +
  stat_qq()+stat_qq_line(size=1.25, color="red")  

#Apply the same techniques to the other variables to choose the best predictor for runs:-
p_1 <- ggplot(data = mlb11, aes(x=at_bats,y=runs),color="red")+geom_point()+stat_smooth(method = "lm", se = FALSE)
p_2 <- ggplot(data = mlb11, aes(x=hits,y=runs),color="red")+geom_point()+stat_smooth(method = "lm", se = FALSE)
p_3 <- ggplot(data = mlb11, aes(x=homeruns,y=runs),color="red")+geom_point()+stat_smooth(method = "lm", se = FALSE)
p_4 <- ggplot(data = mlb11, aes(x=bat_avg,y=runs),color="red")+geom_point()+stat_smooth(method = "lm", se = FALSE)
p_5 <- ggplot(data = mlb11, aes(x=strikeouts,y=runs),color="red")+geom_point()+stat_smooth(method = "lm", se = FALSE)
p_6 <- ggplot(data = mlb11, aes(x=stolen_bases,y=runs),color="red")+geom_point()+stat_smooth(method = "lm", se = FALSE)
p_7 <- ggplot(data = mlb11, aes(x=wins,y=runs),color="red")+geom_point()+stat_smooth(method = "lm", se = FALSE)

grid.arrange(p_1,p_2,p_3,p_4,p_5,p_6,p_7,ncol = 2)

#print summaries of all plots, to get the R squared values that explain variability
m_1 <- lm(runs~at_bats, data=mlb11)
m_2 <- lm(runs~hits, data=mlb11)
m_3 <- lm(runs~homeruns, data=mlb11)
m_4 <- lm(runs~bat_avg, data=mlb11)
m_5 <- lm(runs~strikeouts, data=mlb11)
m_6 <- lm(runs~stolen_bases, data=mlb11)
m_7 <- lm(runs~wins, data=mlb11)

mlb11%>%summarise(v = cor(runs,at_bats))%>%mutate(Rsquare = v*v)
mlb11%>%summarise(v = cor(runs,hits))%>%mutate(Rsquare = v*v)
mlb11%>%summarise(v = cor(runs,homeruns))%>%mutate(Rsquare = v*v)
mlb11%>%summarise(v = cor(runs,bat_avg))%>%mutate(Rsquare = v*v)
mlb11%>%summarise(v = cor(runs,strikeouts))%>%mutate(Rsquare = v*v)
mlb11%>%summarise(v = cor(runs,stolen_bases))%>%mutate(Rsquare = v*v)
mlb11%>%summarise(v = cor(runs,wins))%>%mutate(Rsquare = v*v)

p_8 <- ggplot(data = mlb11, aes(x=new_onbase,y=runs),color="red")+geom_point()+stat_smooth(method = "lm", se = FALSE)
p_9 <- ggplot(data = mlb11, aes(x=new_slug,y=runs),color="red")+geom_point()+stat_smooth(method = "lm", se = FALSE)
p_10 <- ggplot(data = mlb11, aes(x=new_obs,y=runs),color="red")+geom_point()+stat_smooth(method = "lm", se = FALSE)

grid.arrange(p_8,p_9,p_10,ncol = 2)

m_8 <- lm(runs~new_onbase, data=mlb11)
m_9 <- lm(runs~new_slug, data=mlb11)
m_10 <- lm(runs~new_obs, data=mlb11)

mlb11%>%summarise(v = cor(runs,new_onbase))%>%mutate(Rsquare = v*v)
mlb11%>%summarise(v = cor(runs,new_slug))%>%mutate(Rsquare = v*v)
mlb11%>%summarise(v = cor(runs,new_obs))%>%mutate(Rsquare = v*v)

#Plot to check whether we can apply linear regression to this model new_obs:

c1 <- ggplot(data = m_10, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

#NEarly Normal residuals
c2 <- ggplot(data = m_10, aes(x = .resid)) +
  geom_histogram(binwidth = 24) +
  xlab("Residuals")
#QQ-plot
c3 <- ggplot(data = m_10, aes(sample = .resid)) +
  stat_qq()+stat_qq_line(size=1.25, color="red") 

grid.arrange(c1,c2,c3,ncol = 2)


