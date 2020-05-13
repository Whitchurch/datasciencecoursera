
# Create parameter theta to vary from 0 to 1
theta <- seq(from = 0, to = 1, by = 0.01)

#Now create a Beta[1,1], uniform distribution.
plot(theta, dbeta(theta,1,1), type = "l") #This plot is the prior for the Theta distribution
prior <- dbeta(theta,1,1)

#Now visualize what a Beta[1+3,1+0] distribution looks like: This has 1 success in 1 trials and 0 failures.
plot(theta, dbeta(theta,1+1,1+0), type = "l") #The plot that we get is the posterior for Theta distribution.

#Now let us see if we can generate the same programatically without relying on Conjugacy.

#let us plot the likelihood of seeing 3 success and zero fails in 3 trials
plot(theta,dbinom(1,1,prob = theta),type = "l")
likelihood <- dbinom(1,1,prob = theta)

#Calculate the numerator= likelihood X priorBeta
numerator <- likelihood*prior
plot(theta, numerator, type = "l")

#Calculate the denominator = Integral (likilihood X priorBeta)
# We can get away with using summation to see what happens:-
denominator <- sum(likelihood*prior)/100

#Printing out the Posterior distribution
plot(theta, numerator/denominator, type = "l")

sum(numerator/denominator) #Sanity check to see if the total probability sums upto 1.
df <- numerator/denominator


theta <- seq(from = 0, to = 1, by = 0.01)
likelihood1 <- dbinom(1,1,prob = theta)
prior <- dbeta(theta,3,1)
denominator1 <- sum(likelihood1*prior)/100


