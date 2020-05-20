
library(BAS)
cognitive = read.csv("http://bit.ly/dasi_cognitive")

cog_full = lm(kid_score ~ mom_hs+mom_iq+mom_work+mom_age,data = cognitive)

BIC(cog_full) #Result: formula = kid_score ~ mom_hs + mom_iq, data = cognitive

stepAIC(cog_full,direction = "backward",k = log(n)) #This gives us the best parsimonious model

cog_bas = bas.lm(kid_score ~ mom_hs+mom_iq+mom_work+mom_age,prior = "BIC",modelprior = uniform(),data = cognitive)
#Model: The full model used
#prior: what is the prior for each parameter to be used in linear regression of all the models, to calculate likelihood.
#modelPrior: Basically assignms equal priors to all N-Models , meaning, all Models are considered to have equal probabilit.
# before anything is done. After we multiploy by likelihoods , we ofcourse get the posterior probability of most apt
# candidate models. As specified in the EndGame (given below)
#End game: (Model-L Likelihood/ Model-Base Likelihood)*(probabiltiy Prior Model-L/Probability Prior Model-B) = Posterior-L/Posterior-B

# Summary of the BAS function: This shows the posterior updated probabilities. (we could test with ZellNer-SIOW) for regression 
#paratmets while calculating the likelihood, later. BAsically varying these priors might favor, differnt posterior probabilities
# some favoring parsimony, while others favor the predictability. (That is what the last video in the course is all about)

round(summary(cog_bas),3)
image(cog_bas, rotate = F)

diagnostics(cog_bas)
plot(cog_bas, which = 2,add.smooth = F)

coef(cog_bas)
plot(coef(cog_bas))
# Crime and Data
library(MASS)
data(UScrime)

UScrime[,-2] = log(UScrime[,-2])
crime.ZS <- bas.lm(y ~., data = UScrime, prior = "ZS-null", modelprior = uniform(), method = "MCMC")

diagnostics(crime.ZS)
plot(crime.ZS, which = 4)

