library(ggplot2)
library(dplyr)
library(statsr)
library(forcats)
library(tidyr)
load("gss.Rdata")

names(gss)
gss%>%select(c("income06","coninc","relig","owngun","race"))
table(gss$owngun)
table(gss$race)
table(gss$relig)
table(gss$income06)
gss$coninc

#Steps to get the required data
# 1) Select the Race and OwnGuns questions , and print a summary
gunsandrace <- gss%>%select(c("race","owngun"))

# 2) Create a Summary statistic of : Race(Categorical) Vs guninfo(Categorical)
percentData <- gunsandrace %>% group_by(race) %>% count(owngun) %>%
  mutate(ratio=scales::percent(n/sum(n)))
percentData

# 3) Plot the data
ggplot(data = gunsandrace, aes(x = race, fill = owngun))+geom_bar(position = "fill")+geom_text(data=percentData, aes(y=n,label=ratio),position=position_fill(vjust=0.5))+
  ggtitle("Distribution of Guns by race")

# 4) Create a contigency table:-
contigencyTable <- gunsandrace%>%group_by(race)%>%count(owngun)

# Our data is in a long format. In order to generate the contigency table of interest and for easy visualization.
# We are going to transform the data into a wide format:  Race will form the columns, and ownguns will form the rows

contigencyTable_WF <- contigencyTable%>%spread(race,n)%>%mutate(Column_Total =  rowSums(.[2:4]))%>%mutate(Row_Total = colSums(.[2:5]))%>%mutate(Expected_White = (Row_Total[1]*Column_Total[1:4])/57061,Expected_Black =(Row_Total[2]*Column_Total[1:4])/57061,Expected_Other =(Row_Total[3]*Column_Total[1:4])/57061)
contigencyTable_WF$Expected_White <- round(contigencyTable_WF$Expected_White,digits = 0)
contigencyTable_WF$Expected_Black <- round(contigencyTable_WF$Expected_Black,digits = 0)
contigencyTable_WF$Expected_Other <- round(contigencyTable_WF$Expected_Other,digits = 0)

contigencyTable_WF$ChiWhite <- ((contigencyTable_WF$White[1:4]-contigencyTable_WF$Expected_White[1:4])^2)/contigencyTable_WF$Expected_White
contigencyTable_WF$ChiBlack <- ((contigencyTable_WF$Black[1:4]-contigencyTable_WF$Expected_Black[1:4])^2)/contigencyTable_WF$Expected_Black
contigencyTable_WF$ChiOther <- ((contigencyTable_WF$Other[1:4]-contigencyTable_WF$Expected_Other[1:4])^2)/contigencyTable_WF$Expected_Other
chisquare <- contigencyTable_WF%>%summarise(ChiSquare = sum(sum(ChiWhite),sum(ChiBlack),sum(ChiOther)))

df <- (4-1)*(3-1) #(row-1)*(Col-1)

pchisq(as.double(chisquare),6,lower.tail = FALSE)

# We get an extremely low p-value, hence we reject the null hypothesis.
# The low p- value provides convincing evidence that there is a relationship between race and gun-ownership .
# 5) Since we are dealing with 2 categorical variables, each with more than 2 levels:
# . We will use the Chi square test for independence.

# We need to check conditions for applying Chi-square test of independence
# Independence: random sample/assignment; if sampling without replacement n<10% of population; each case only contributes to one cell in the table
# Sample size: Each particular scenatio(cell) must have atleast 5 expected cases.
# We then need to generate the table of expected values
# We then need to calculate the Chi-square value
# We need to use the Ch-value to test for independence.

