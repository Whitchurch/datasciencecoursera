
library(lestat)
#prior Normal Gamma posterior for our examole: (we use Monte Carlo simulation, based on parameters of our choice to create this prior)
# mo = 35, n0 = 25, s0^2(variance) = 156.25, vo(degrees of freedom) = n0-1

set.seed(8675309)

m_0 = 35
n_0 = 25
s2_0 = 156.25
v_0 = n_0-1

#Get the sample mean: m, numer of samples: n, variance: s^2
data("tapwater")
Y = tapwater$tthm

ybar = mean(Y)
s2 = var(Y)
n = length(Y)

print(ybar)
print(sqrt(s2))
print(n)

# Using these extracted values, by Normal GAmma conjucacy we get the posterior prior
n_n = n_0+n
m_n = (n*ybar+n_0*m_0)/n_n
v_n = v_0+n
s2_n = ((n-1)*s2 + v_0*s2_0 + n_0*n*(m_0-ybar)^2/n_n)/v_n



L = qt(.025,v_n)*sqrt(s2_n/n_n)+m_n
U = qt(.975,v_n)*sqrt(s2_n/n_n)+m_n

c(L,U)


#Using motecarloe to select parameters to get the prior:
# Prior: expect TTHM betwen 10-60 (our intial assumption)

#Prior mean
m0 <- (60+10)/2

# Prior sigma and variance
s0 <- (60-10)/4
s2_0 <- (s0)^2

# we need V0 (degree of Freedom) & n0 (number of samples), these we get from Monte Carlo
# We do this till we get a prior distribution that matches our intial assumption of 60 - 10
# Doing this we end up with a sample size of no = 25, and Degrees of freedom V0 = n0 -1 

n0 = 25; v0 = n0-1

phi = rgamma(1000, v0/2,s2_0*v0/2)
sigma = 1/sqrt(phi)

mu = rnorm(1000, mean = m0, sd=sigma/sqrt(n_0))
y = rnorm(1000,mu,sigma)
quantile(y,c(.025,.975))



# Using a non-informative prior approach. To get the pred_y, for reference prior
phi = rgamma(10000,(n-1)/2,s2*(n-1)/2)
sigma = 1/sqrt(phi)

post_mu = rnorm(10000, mean = ybar, sd = sigma/sqrt(n))
pred_y = rnorm(10000, post_mu, sigma)

quantile(pred_y,c(.025,.975))

