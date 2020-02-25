library(dplyr)
library(ggplot2)
library(statsr)

## 1)
data(arbuthnot)

str(arbuthnot)
names(arbuthnot)

## 2)
arbuthnot$girls

## 3)

ggplot(data = arbuthnot, aes(x = year,y = girls))+geom_line(linetype = "dashed")

## 4)
data(present)
str(present)
present$total <- present$boys+present$girls
present$prop_boys <- present$boys/present$total
ggplot(data = present, aes(x = year,y = prop_boys))+geom_line(linetype = "dashed")

## 5) TRUE

## 6)
present[present$boys < present$girls,]

## 7) 
present$prop_boy_girl <- present$boys/present$girls
ggplot(data = present, aes(x = year,y = prop_boy_girl))+geom_line(linetype = "dashed")

## 8)
present%>%arrange(desc(total))
ggplot(data = present, aes(x = year,y = total))+geom_line(linetype = "dashed")
grep(max(total), present$total)

present[grep(max(total), present$total), ]

## 2007 had the maximum births in our observed range





