library(MASS)
library(tidyverse)
library(statsr)
library(BAS)
library(broom)

data(wage)

set.seed(18382)

#EDA on the wages: This is our response variable, over which we hopefully fit a nice model, to explain it.
ggplot(data = wage, aes(x = wage)) +geom_histogram(binwidth = 100)+scale_y_continuous(breaks = round(seq(0,1000, by = 2),1))+scale_x_continuous(breaks = round(seq(0,4000, by = 300),1))

summary(wage$wage)

#Simple Linear regression: Bayseran
#Explanatory: IQ
ggplot(data = wage, aes(x = iq, y = wage)) +geom_point()

m_wage_iq <- lm(wage ~ iq, data = wage)
summary(m_wage_iq)

ggplot(data = wage, aes(x = iq, y = wage)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

confint(m_wage_iq)

#Explanatory:Educ
ggplot(data = wage, aes(x = educ, y= wage))+geom_point()
m_wage_educ <- lm(wage ~ educ, data = wage)
summary(m_wage_educ)

ggplot(data = wage, aes(x = educ, y = wage)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

confint(m_wage_educ)

cor(wage$educ,wage$wage)
cor(wage$iq,wage$wage)
# Educ has a higher correlation, than IQ.

#Model Diagnonstics:

#IQ
# Linearity and Constant VAriance:
m_wage_iq_aug <- augment(m_wage_iq)

#Linearity and Constant Variance: You already checked if the relationship between weekly wages and IQ is linear using a scatterplot. We should also verify this condition with a plot of the residuals vs. fitted (predicted) values.

ggplot(data = m_wage_iq_aug, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals")

#Check normality
ggplot(data = m_wage_iq_aug, aes(x = .resid)) +
  geom_histogram(binwidth = 100) +
  xlab("Residuals")

ggplot(m_wage_iq_aug) +
  geom_qq(aes(sample = .std.resid)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Theoretical quantiles", y = "Standardized residuals")

#Education:
m_wage_educ_aug <- augment(m_wage_educ)

ggplot(data = m_wage_educ_aug, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals")

#Check normality
ggplot(data = m_wage_educ_aug, aes(x = .resid)) +
  geom_histogram(binwidth = 100) +
  xlab("Residuals")

ggplot(m_wage_educ_aug) +
  geom_qq(aes(sample = .std.resid)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Theoretical quantiles", y = "Standardized residuals")

#Both education and wage are right skewed, and do not exhibit constant variance.


m_lwage_iq = lm(lwage ~ iq, data = wage)
m_lwage_iq_aug <- augment(m_lwage_iq)

ggplot(data = wage, aes(x = iq, y = lwage)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

#Check for linearity, constant variance, normal distribution of residuals

ggplot(data = m_lwage_iq_aug, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals")

ggplot(data = m_lwage_iq_aug, aes(x = .resid)) +
  geom_histogram(binwidth = 100) +
  xlab("Residuals")

ggplot(m_lwage_iq_aug) +
  geom_qq(aes(sample = .std.resid)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Theoretical quantiles", y = "Standardized residuals")

outliers <- Bayes.outlier(m_lwage_iq, k = 3)

outliers_df <- data.frame(probability = outliers$prob.outlier,
                          case = 1:length(outliers$prob.outlier))
ggplot(outliers_df, aes(ymax = probability, x = case)) +
  geom_linerange(ymin = 0) +
  labs(y = "Probability")

outliers_df %>%
  filter(probability > 0.50)

#Check for outliers above 3 stadnard deviation
(prob_outlier <- pnorm(-4) + pnorm(4, lower.tail = FALSE))
(prob_not_outlier <- 1 - prob_outlier)

#Probability of no outliers. for a sample of size n.
n <- nrow(wage)
(prob_no_outliers <- prob_not_outlier^1000)
1 - prob_no_outliers

#instead of settink K, set prior probability and then sove for K. DO the reverese of whatever was done.
n <- nrow(wage)
(prob_obs_not_outlier <- 0.95^(1/1000))

(newk <- qnorm(0.5 + 0.5 * prob_obs_not_outlier))

outliers <- Bayes.outlier(m_lwage_iq, prior.prob=0.92)


outliers_df <- data.frame(probability = outliers$prob.outlier,
                          case = 1:length(outliers$prob.outlier))
ggplot(outliers_df, aes(ymax = probability, x = case)) +
  geom_linerange(ymin = 0) +
  labs(y = "Probability")

outliers_df %>%
  filter(probability > 0.50)


# Multiple regressin bayesan style and throw in BMA too.
m_lwage_full <- lm(lwage ~ . - wage, data = na.omit(wage))
summary(m_lwage_full)
BIC(m_lwage_full)

BIC(stepAIC(m_lwage_full, direction = "backward", k = log(n)))
#Best BIC model: lwage ~ hours + iq + educ + tenure + age + married + urban + meduc
m_lwage_reduced <- lm(lwage ~ hours + iq + educ + tenure + age + married + urban + meduc, data = na.omit(wage))
summary(m_lwage_reduced)
BIC(m_lwage_reduced)
#Apply BMA


# Exclude observations with missing values in the data set
wage_no_na <- na.omit(wage)

# Fit the model using Bayesian linear regression, `bas.lm` function in the `BAS` package
bma_lwage <- bas.lm(lwage ~ . -wage, data = wage_no_na,
                    prior = "BIC", 
                    modelprior = uniform())



# Print out the marginal posterior inclusion probabilities for each variable                
bma_lwage
summary(bma_lwage)


# Obtain the coefficients from the model `bma_lwage`
coef_lwage <- coefficients(bma_lwage)

plot(coef_lwage)

confint(coef_lwage)


#use a reduced model
wage_red <- wage %>%
  select(-wage, -sibs, -brthord, -meduc, -feduc)

bma_lwage_red <- bas.lm(lwage ~ ., data = wage_red,  
                        prior = "ZS-null",
                        modelprior = uniform())

summary(bma_lwage_red)
plot(bma_lwage_red, which = 4)


#naive model with all variables included
bma_lwage <- bas.lm(lwage ~ . -wage, data = wage_no_na,
                    prior = "ZS-null", 
                    modelprior = Beta(1,1))
image(bma_lwage,rotate = F)

plot(bma_lwage, which = 4)

coef(bma_lwage_red)

coef(bma_lwage_red) %>%
  confint()

#Baysean prediction:-
BMA_pred_lwage <- predict(bma_lwage, estimator = "BMA", se.fit = TRUE)
variable.names(BMA_pred_lwage)

BPM_pred_lwage <- predict(bma_lwage, estimator = "BPM", se.fit = TRUE)
variable.names(BPM_pred_lwage)

HPM_pred_lwage <- predict(bma_lwage, estimator = "HPM")
variable.names(HPM_pred_lwage)

MPM_pred_lwage <- predict(bma_lwage, estimator = "MPM")
variable.names(MPM_pred_lwage)

# Find the index of observation with the largest fitted value
opt <- which.max(BPM_pred_lwage$fit)

# Extract the row with this observation and glimpse at the row
wage_no_na %>% 
  slice(opt) %>%
  glimpse()

ci_lwage <- confint(BPM_pred_lwage, parm = "pred")
ci_lwage[opt,]

exp(ci_lwage[opt,])

BMA_pred_lwage <- predict(bma_lwage, estimator = "BMA", se.fit = TRUE)
ci_bma_lwage <- confint(BMA_pred_lwage, estimator = "BMA")
opt_bma <- which.max(BMA_pred_lwage$fit)
exp(ci_bma_lwage[opt_bma, ])

BPM_pred_lwage <- predict(bma_lwage, estimator = "MPM", se.fit = TRUE)
ci_bpm_lwage <- confint(BPM_pred_lwage, estimator = "MPM")
opt_bpm <- which.max(BPM_pred_lwage$fit)
exp(ci_bpm_lwage[opt_bpm, ])


