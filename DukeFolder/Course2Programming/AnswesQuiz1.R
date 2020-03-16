library(statsr)
library(dplyr)
library(shiny)
library(ggplot2)

data("ames")
str(ames)

ggplot(data = ames, aes(x = area, y = price))+geom_point() # area vs price
ggplot(data = ames, aes(x = area))+geom_histogram(binwidth = 250) # histogram of area
ggplot(data = ames, aes(x = price))+geom_histogram() # histogram of price

summary(ames$area) # get summart stats for the area of the houses 
summary(ames$price) # get summary stats for the price distribution
summarise(ames, standard_deviation = sd(area))
summarise(ames, standard_deviation = sd(price))

ames %>%
  summarise(mu = mean(area), pop_med = median(area), 
            sigma = sd(area), pop_iqr = IQR(area),
            pop_min = min(area), pop_max = max(area),
            pop_q1 = quantile(area, 0.25),  # first quartile, 25th percentile
            pop_q3 = quantile(area, 0.75))  # third quartile, 75th percentile

#What is false: 50% of the houses in Ames are smaller than 1499.69 sq feet. 
# This is false, as the Median is 1442 sq feet and thus 50% of the houses are smaller than 1422 sq feet. 
# The mean is 1500 sqft, which means, there is an additional % of houses more than the median which is smaller than 50%. since the d
# data is right skewed.
# So more than 50% of the houses are less than 1499.69 Sqfeet.

# Selecting a random sample of 5o homes from the Ames population.
sampl <- ames %>%sample_n(size = 50)

str(sampl)
summary(ames$area)
summary(sampl$area)

sampl %>%
  summarise(sample_mean = mean(area), sample_med = median(area), 
            s = sd(area), sample_iqr = IQR(area),
            sample_min = min(area), sample_max = max(area),
            sample_q1 = quantile(area, 0.25),  # first quartile, 25th percentile
            sample_q3 = quantile(area, 0.75))  # third quartile, 75th percentile
sampl %>% summarise(x_bar = mean(area))

#Increase the sample sizes to 100 and 1000 to see which gets closer to the true population mean:
sampl_100 <- ames %>%sample_n(size = 100)
sampl_100 %>% summarise(x_bar = mean(area))

sampl_1000 <- ames%>%sample_n(size = 1000)
sampl_1000%>%summarise(x_bar = mean(area))

# Take samples of size 50 , to generate a sampling distrubution, of X_bars spread ut over some SE.
ames %>%
  sample_n(size = 50) %>%
  summarise(x_bar = mean(area))

sample_means50 <- ames %>%
  rep_sample_n(size = 50, reps = 15000, replace = TRUE) %>%
  summarise(x_bar = mean(area))

ggplot(data = sample_means50, aes(x = x_bar)) +
  geom_histogram(binwidth = 20)

sample_means50 %>%
  summarise(sample_mean = mean(x_bar), sample_med = median(x_bar), 
            s = sd(x_bar), sample_iqr = IQR(x_bar),
            sample_min = min(x_bar), sample_max = max(x_bar),
            sample_q1 = quantile(x_bar, 0.25),  # first quartile, 25th percentile
            sample_q3 = quantile(x_bar, 0.75))  # third quartile, 75th percentile
str(sample_means50)

#The distribution is normal, it has a smaller SE compared to the population SD, it's mean is centered at the true population mean.

#Create sample_means_small

sample_means_small <- ames%>%rep_sample_n(size = 10, reps = 25, replace = TRUE)%>%summarise(x_bar = mean(area))
str(sample_means_small)

ggplot(sample_means_small, aes(x = x_bar))+geom_histogram()

# There are 25 observations, each observation represents the mean of square footage from a sample of 10 homes.
# Plotting the mean square footage of 10 homes repeated over 25 times, gives us a histogram.
# The center of the histogram could reasonalble be centered at teh true population mean.
# The accuracy of which can e increasesd by increasing sample sizes, which makes the variability around the true mean, narrower 
# and hence more accurate.