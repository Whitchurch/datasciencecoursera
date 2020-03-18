# set seed for reproducability

set.seed(9102015) 


# Load package 
library(statsr)
library(dplyr)
library(ggplot2)

# Load the data
data("ames")

#Set and sample 60 houses
n <- 60
samp <- sample_n(ames,n)

ggplot(samp,aes(x = area))+geom_histogram()
summary(samp$area)
samp%>%summarise(meanvalue = mean(area), stddev = sd(area))

#Go about calculating the confidence interval:-
z_star95 <- qnorm(1-0.05/2) #For a 95% confidence interval, this is the z-value.

#for a 95% CL for Confidence interval we get
samp%>%summarise(upper = mean(area)+z_star95*(sd(area)/sqrt(60)), lower = mean(area)-z_star95*(sd(area)/sqrt(60)))


#Capture the true population mean, and compare it with our sample mean
params <- ames%>%summarise(mu = mean(area))

#Obtain a random sample of size 60, calcualte mean and sd, repeat this 50 times to calculate the confidence intervals
ci <- ames %>%
  rep_sample_n(size = n, reps = 50, replace = TRUE) %>%
  summarise(lower = mean(area) - z_star95 * (sd(area) / sqrt(n)),
            upper = mean(area) + z_star95 * (sd(area) / sqrt(n)))
ci <- ci %>%
  mutate(capture_mu = ifelse(lower < params$mu & upper > params$mu, "yes", "no"))

ci_data <- data.frame(ci_id = c(1:50, 1:50),
                      ci_bounds = c(ci$lower, ci$upper),
                      capture_mu = c(ci$capture_mu, ci$capture_mu))

ggplot(data = ci_data, aes(x = ci_bounds, y = ci_id, 
                           group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +  # add points at the ends, size = 2
  geom_line() +           # connect with lines
  geom_vline(xintercept = params$mu, color = "darkgray") # draw vertical line

#For CL of 99%
z_star_99 <- qnorm(1-0.01/2)
ci <- ames %>%
  rep_sample_n(size = 60, reps = 50, replace = TRUE) %>%
  summarise(lower = mean(area) - z_star_99 * (sd(area) / sqrt(n)),
            upper = mean(area) + z_star_99 * (sd(area) / sqrt(n)))
ci <- ci %>%
  mutate(capture_mu = ifelse(lower < params$mu & upper > params$mu, "yes", "no"))

ci_data <- data.frame(ci_id = c(1:50, 1:50),
                      ci_bounds = c(ci$lower, ci$upper),
                      capture_mu = c(ci$capture_mu, ci$capture_mu))

ggplot(data = ci_data, aes(x = ci_bounds, y = ci_id, 
                           group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +  # add points at the ends, size = 2
  geom_line() +           # connect with lines
  geom_vline(xintercept = params$mu, color = "darkgray") # draw vertical line


