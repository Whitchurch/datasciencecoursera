#Load package
library(dplyr)
library(ggplot2)
library(statsr)

#Data: Analyze the structure
data("nycflights")
str(nycflights)
names(nycflights)
head(nycflights, n = 1)

#Departure delay in fligts to RDU

#Only select delays greater than 0, ignore the negative cases as they indicate early departures.
nycflightsRDU <- nycflights%>%filter(dest == "RDU")%>%filter(dep_delay > 0)%>%select(dep_delay)
nycflightsRDUWithEarlyDepartures <- nycflights%>%filter(dest == "RDU")%>%select(dep_delay)



ggplot(data = nycflights, aes(x = dep_delay))+geom_histogram()
ggplot(data = nycflights, aes(x = dep_delay))+geom_histogram(binwidth = 15)

#I decided to plot both early departures to see how they settle into the distribution
ggplot(data = nycflightsRDU, aes(x = dep_delay))+geom_histogram(binwidth = 4)
ggplot(data = nycflightsRDUWithEarlyDepartures, aes(x = dep_delay))+geom_histogram()

nycflightsRDUWithEarlyDepartures%>%summarise(mean_dd = mean(dep_delay), sd_dd = sd(dep_delay),median_dd = median(dep_delay) ,n = n())
summary(nycflightsRDUWithEarlyDepartures)


## Departure delays over months
nycflights%>%group_by(month)%>%summarise(meanMonthDelay = mean(dep_delay))%>%arrange(desc(meanMonthDelay))

#Ans: Month of July.

## Ontime depratrure rate for NYC airport
otD1 <- nycflights%>%mutate(dep_type = ifelse(dep_delay < 5,"ontime","delayed"))%>%group_by(origin)%>%summarise(sum(dep_type == "ontime")/n())


#Quiz -2 questions:- 
# 1) How many flights are headed to SFO in Feb.

ans1 <- nycflights%>%filter(dest == "SFO", month == 2 )%>%summarise(n())
print(ans1)


# 2) Arrival delays for SFO flights in feb

ans2 <- nycflights%>%filter(dest == "SFO", month == 2)
str(ans2)

sp <- ggplot(data = ans2, aes(x = arr_delay))+geom_histogram(binwidth = 7)

sp <- sp+geom_vline(xintercept = mean(ans2$arr_delay), color = "red")
sp+geom_vline(xintercept = median(ans2$arr_delay), color = "green")

ans2%>%summarise(mean_AD = mean(arr_delay), median_AD = median(arr_delay), sd_AD = sd(arr_delay), IQR(arr_delay))
quantile(ans2$arr_delay)

#mean = - 4.5, median = - 11, standard_deviation = 36.3 , IQR = 23.25


# 3) Calculate the median and the IQR for arr_delays of flights, grouped by carrier. Which has highest IQR
ans2%>%group_by(carrier)%>%summarise(median_AD = median(arr_delay), IQR_AD = IQR(arr_delay))%>%arrange(desc(IQR_AD))
ggplot(data = ans2, aes(x = carrier, y = arr_delay))+geom_boxplot()

#Delta and UA

# 4) Which month among all NYC airports has the highest average departure delay
nycflights%>%group_by(month)%>%summarise(avgDD = mean(dep_delay))%>%arrange(desc(avgDD))

# 5) Which month has the highest median depature delay from an NYC airport
nycflights%>%group_by(month)%>%summarise(medianDD = median(dep_delay))%>%arrange(desc(medianDD))

# 6) I would use the median to select the airports based on delays, as the data is skewed due to outliers.
# Median is robust to outliers, unlike averages that tend to be pulled by the skewing gravity.

# 7) Based purely on on-time departure percentage which airport would you prefer:
otD1 <- nycflights%>%mutate(dep_type = ifelse(dep_delay < 5,"ontime","delayed"))%>%group_by(origin)%>%summarise(rate = sum(dep_type == "ontime")/n())
fillrange <- nycflights%>%mutate(dep_type = ifelse(dep_delay < 5,"ontime","delayed"))
ggplot(data = fillrange, aes(x = origin, fill = dep_type ))+geom_bar()

# 8) Create a new variable called avg_speed:- Plane with the fastest average speed
nycflights <- nycflights%>%mutate(airtime_hours = air_time/60)%>%mutate(avg_speed = distance/airtime_hours)
head(nycflights%>%select(tailnum,avg_speed)%>%arrange(desc(avg_speed)), n = 1)

# 9) Make a scatter plot of avg_speed vs distance.
ggplot(data = nycflights, aes(x=avg_speed, y = distance))+geom_point()+geom_smooth()


# There is an overall positive relationship between average speed and distance:
# There is one interesting case of a high speed plane, that covers a short distance.
# The distibution of distances is not uniform. There is a dearth of flights in the gap between 3000 to 5000 distances

# 10) Determine the fraction of flights, which were delayed at destination , but still manage to arrive on-time

nycflights <- nycflights%>%mutate(dep_type = ifelse(dep_delay < 5,"ontime","delayed"), arr_type = ifelse(arr_delay <=0,"ontime","delayed"))%>%group_by(dep_type)

nycflights%>%summarise(ontimearrivalpercentage = sum(arr_type == "ontime")/n())

#Looks like 18% of the flights that were delayed during departure, managed to arrive on time.

