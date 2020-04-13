library(statsr)
library(dplyr)
library(ggplot2)
library(GGally)

data("evals")
str(evals)

summary(evals$score)
ggplot(data = evals, aes(x = score))+geom_histogram()
quantile(evals$score,seq(0,1, length = 437) ,type = 5)

ggplot(data = evals, aes(x = bty_avg, y = score)) +
  geom_point()

ggplot(data = evals, aes(x = bty_avg, y = score)) +
  geom_jitter()

ggplot(data = evals, aes(x = bty_avg, y = score)) +
  geom_jitter() +
  geom_smooth(method = "lm")

# Let us plot the Correlation coefficient to test the strength of linearity.
evals%>%summarise(cor(score,bty_avg))

#Let us fir a model to the points and then summarize what is going on:-
#Fit a least square line to the data using the lm function
ml <- lm(score~bty_avg,data = evals)
summary(ml)

#Model diagnonists" Check: Linearity, notmal distribtuin of residuals(hist/Q-Q plot), Constant variability

#Linearity: via residual plot.

ggplot(data = ml, aes(x = .fitted, y = .resid)) +
  geom_jitter() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

#NEarly Normal residuals
ggplot(data = ml, aes(x = .resid)) +
  geom_histogram(binwidth = 0.5) +
  xlab("Residuals")
#QQ-plot
ggplot(data = ml, aes(sample = .resid)) +
  stat_qq()+stat_qq_line(size=1.25, color="red")  


#Multiple regression section of the quiz:
ggplot(data = evals, aes(x = bty_f1lower, y = bty_avg)) +
  geom_jitter()+geom_smooth(method = "lm")

evals %>% 
  summarise(cor(bty_avg, bty_f1lower))

#Correlation is high this proves there is a strong linear relationship.

#Let us fir the model to see how much of the variability can be explained:
ml <- lm(bty_avg~bty_f1lower,data = evals)
summary(ml)

#Has a high degree for Adjusted R- This is therefore a significant predictor.

# Let us see if this satisfies the least square line conditions

#Linearity: via residual plot.

ggplot(data = ml, aes(x = .fitted, y = .resid)) +
  geom_jitter() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

#NEarly Normal residuals
ggplot(data = ml, aes(x = .resid)) +
  geom_histogram(binwidth = 0.5) +
  xlab("Residuals")
#QQ-plot
ggplot(data = ml, aes(sample = .resid)) +
  stat_qq()+stat_qq_line(size=1.25, color="red")  

# All three conditions are satified we can say we can confidently use the least square line approach of linear regression here.

ggpairs(evals, columns = 13:19)

m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
summary(m_bty_gen)

# Let us start by checking if condtions for regression are reasonable.
#check for linear relationship between numeric predictor x and response y.
plot(m_bty_gen$residuals ~ evals$bty_avg)+abline(h = 0) # There is random scatter.

# check for normal distrubution of residuals: using hist and normal QQ plot
hist(m_bty_gen$residuals)

qqnorm(m_bty_gen$residuals)
qqline(m_bty_gen$residuals)

# There seems to be a left skew, so this does not seem to satisfy the conditions.

ggplot(data = m_bty_gen, aes(sample = .resid)) +
  stat_qq()+stat_qq_line(size=1.25, color="red") 

# residuals versus response scatter plot
plot(m_bty_gen$residuals~m_bty_gen$fitted)+abline(h = 0)

plot(m_bty_gen$residuals)

# Next model using rank instead of gender
m_bty_rank <- lm(score ~ bty_avg + rank, data = evals)
summary(m_bty_rank)

#predicton
newprof <- data.frame(gender = "male", bty_avg = 3)
predict(m_bty_gen, newprof)
predict(m_bty_gen, newprof, interval = "prediction", level = 0.95)

#search for the best model:-
m_full <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval 
             + cls_students + cls_level + cls_profs + cls_credits + bty_avg 
             + pic_outfit + pic_color, data = evals)
summary(m_full)
