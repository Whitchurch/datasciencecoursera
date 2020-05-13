library(statsr)
data(brfss)

credible_interval_app()

# 95% Credible interval for posterior distribution with Mean = 10, Variance = 2.236
qnorm(c(0.025, 0.975), mean = 10, sd = 2.236)

# 90% credible interval of a posterior Beta interval with Alpha = 2, Beta = 5
qbeta(c(0.05, 0.95), shape1 = 2, shape2 = 5)

# 99% credible interval for a posterior Gamma distribution with alpha = 4, Beta = 8
qgamma(c(0.005, 0.995), shape = 4, rate = 8)


# EDA on sex ration
table(brfss$sex)

n <- length(brfss$sex)
x <- sum(brfss$sex == "Female")

# Let us calculate the proption of females from our sample data: x/n
print(x/n) # we get 0.5172 as the proportion. for this sample.

# Let us do things Baysean style, selection a prior which is a uniform distribution Beta(1,1) <- meaning any proportion from 0 to 1 is equally lickely.

# Alpha = Females, Beta = Males; By conjugact pposterior Beta is (1+alpha, 1+Beta)
qbeta(c(0.025, 0.975), shape1 = 2587, shape2 = 2414)

# By Bayesian we have 95% probability that the true population proportion is within the range: 
## [1] 0.503 0.531



