#Quiz -1 
#1
library(lattice)
str(mtcars)

cylinders <- as.factor(mtcars$cyl)
p <- xyplot(mpg~hp|cylinders,data = mtcars,layout = c(3,1))
class(p) # we get back a treillis object, that is the answer
#plotting just for fun
print(p)

#2
library(nlme)
xyplot(weight ~ Time |Diet, strip = TRUE, BodyWeight)
str(BodyWeight)
summary(BodyWeight)

#5
names(p)
grepl("par()",names(p))
trellis.par.set()

#7
library(datasets)
data(airquality)

airquality = transform(airquality, Month = factor(Month)) # this is correct
qplot(Wind, Ozone, data = airquality, facets = . ~ Month) 

qplot(Wind, Ozone, data = airquality, facets = . ~ factor(Month)) # This also is correct


#9
library(ggplot2)
library(ggplot2movies)
str(movies)
g <- ggplot(movies,aes(votes,rating))
g+geom_point()


qplot(votes, rating, data = movies)+stats_smooth("loess")



