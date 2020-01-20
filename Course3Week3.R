library(dplyr)

#Question -1
if(file.exists("./DownloadFiles/uscommunitiesidaho.csv"))
{
  print("the file exists")
}else
{
  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
  download.file(fileurl,destfile = "./DownloadFiles/uscommunitiesidaho.csv", method = "curl")
}

filepath <- "./DownloadFiles/uscommunitiesidaho.csv"
dataFrameCSV <- read.csv(filepath)
head(dataFrameCSV, n = 2)

agricultureLogical <- (dataFrameCSV$ACR == 3 & dataFrameCSV$AGS == 6)
which(agricultureLogical)


#Question -2
install.packages("jpeg")
library(jpeg)

if(file.exists("./DownloadFiles/jeffpic.jpeg"))
{
  print("the file exists")
}else
{
  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
  download.file(fileurl,destfile = "./DownloadFiles/jeffpic.jpeg", method = "curl")
}

jpegHandle <- readJPEG("./DownloadFiles/jeffpic.jpeg",native = TRUE)
class(jpegHandle)
quantile(jpegHandle,probs = c(0.30,0.80))

#Just coding to figure out what quantiles is doing.
set.seed(15051)                         # Set seed for reproducibility 
x <- round(runif(10, 0, 100)) 
sort(x)
quantile(x) # It looks like quantiles is breaking up the vector into equal parts, and returning the values at the serparation of each part.
            # THis can be useful for breaking up distributions to find based on quantiles, fo r analysis.

#Question -3

# 1) Load GDP dataset
if(file.exists("./DownloadFiles/gdp.csv"))
{
  print("the file exists")
}else
{
  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
  download.file(fileurl,destfile = "./DownloadFiles/gdp.csv", method = "curl")
}

# 2) Load educational dataset
if(file.exists("./DownloadFiles/education.csv"))
{
  print("the file exists")
}else
{
  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
  download.file(fileurl,destfile = "./DownloadFiles/education.csv", method = "curl")
}

GDP <- read.csv("./DownloadFiles/gdp.csv")
education <- read.csv("./DownloadFiles/education.csv")

# head(GDP)
# head(education)
# 
# names(GDP)
# names(education)
# 
# str(GDP)
# str(education)

#Using dplyr, just for fun to see, how to do this with dpylr
library(dplyr)
# matchedindexes <- match(education$CountryCode,GDP$X,)
# masksForNA <- is.na(match(education$CountryCode,GDP$X,))
# 
# print(masksForNA)
# print(matchedindexes)
#  result <- matchedindexes[!masksForNA] #Gives location where all the matches occur
# 
# str(unique(result))

mergeDAta <- merge(GDP,education,by.x = "X",by.y = "CountryCode",all = FALSE)
head(mergeDAta, n= 100)
str(mergeDAta$Gross.domestic.product.2012)
levels(mergeDAta$Gross.domestic.product.2012)


# Convert the factor to character to numeric (follow this procedure), to go from Factor levels to numeric. Thisis to help sort
mergeDAta$Gross.domestic.product.2012 <- as.numeric(as.character(mergeDAta$Gross.domestic.product.2012))
class(mergeDAta$Gross.domestic.product.2012)

sum(as.numeric(mergeDAta$Gross.domestic.product.2012 == "")) # how many cases of empty spaces "" based on levels for GDP
sum(as.numeric(mergeDAta$Gross.domestic.product.2012 ==".. Not available.  ")) # returns no such occurence
sum(as.numeric(mergeDAta$Gross.domestic.product.2012 == "Ranking")) # returns no such occurence
sum(as.numeric(mergeDAta$Gross.domestic.product.2012 == "a. Includes Former Spanish Sahara.  b. Excludes South Sudan  c. Covers mainland Tanzania only. d. Data are for the area "))
sum(as.numeric(mergeDAta$Gross.domestic.product.2012 == "controlled by the government of the Republic of Cyprus.   e. Excludes Abkhazia and South Ossetia.  f. Excludes Transnistria."))
sum(as.numeric(mergeDAta$Gross.domestic.product.2012 == "Note: Rankings include only those economies with confirmed GDP estimates. Figures in italics are for 2011 or 2010."))
sum(as.numeric(is.na(mergeDAta$Gross.domestic.product.2012))) # NAs check.

#Therefore after investigating based on levels(), we find only "" values need to be ignored while arranging by rank
resultCountries <- mergeDAta%>%filter(!is.na(Gross.domestic.product.2012))%>%arrange(desc(Gross.domestic.product.2012))
nrow(resultCountries) # gives 189 countries that are ranked, descending excluding NAs for ranks, as wel as 13th country is KNA

#Question - 4
str(resultCountries)
names(resultCountries)

levels(resultCountries$Income.Group)
str(resultCountries$Income.Group)

#convert levels to characters
resultCountries$Income.Group <- as.character(resultCountries$Income.Group)
str(resultCountries$Income.Group)
sum(is.na(resultCountries$Income.Group)) # Check for Nas, there are none.

str(resultCountries)
unique(resultCountries$Income.Group)

meanHighIncomeOECD <- resultCountries%>%select(Gross.domestic.product.2012,Income.Group)%>%filter(Income.Group == "High income: OECD")
mean(meanHighIncomeOECD$Gross.domestic.product.2012)

meanHighIncomeOECD <- resultCountries%>%select(Gross.domestic.product.2012,Income.Group)%>%filter(Income.Group == "High income: nonOECD")
mean(meanHighIncomeOECD$Gross.domestic.product.2012)

#Question -5 
# splitting the GDP into 5 quantiles
GDPQuantile <- quantile(resultCountries$Gross.domestic.product.2012)
GDPQuantile

group1 <- resultCountries%>%filter(Gross.domestic.product.2012 <= 1)
group2 <- resultCountries%>%filter(Gross.domestic.product.2012 >1 & Gross.domestic.product.2012 <= 48)
group3 <- resultCountries%>%filter(Gross.domestic.product.2012 > 48 & Gross.domestic.product.2012 <= 95)
group4 <- resultCountries%>%filter(Gross.domestic.product.2012 > 95 & Gross.domestic.product.2012 <= 143)
group5 <- resultCountries%>%filter(Gross.domestic.product.2012 > 143 & Gross.domestic.product.2012 <= 190)


group1$Income.Group
group2$Income.Group
group3$Income.Group
group4$Income.Group
group5$Income.Group

table(group1$Gross.domestic.product.2012,group1$Income.Group)
table(group2$Gross.domestic.product.2012,group2$Income.Group) # Got 5 lower middle income nations in the range of 38 countries.
table(group3$Gross.domestic.product.2012,group3$Income.Group)
table(group4$Gross.domestic.product.2012,group4$Income.Group)
table(group5$Gross.domestic.product.2012,group5$Income.Group)

#create a GDP versus Income group table. # I need help in figuring out how to plot Quantiles versus income, without 
# Use the method required in the answer
GDPQuantile <- quantile(resultCountries$Gross.domestic.product.2012)
class(GDPQuantile)
GDPQuantile
resultCountriesByQuantile <- cut(resultCountries$Gross.domestic.product.2012,c(GDPQuantile))
resultCountriesByQuantile

table(resultCountriesByQuantile, resultCountries$Income.Group)

# Fine tune the quantiles in intervals of 10%
GDPQuantile <- quantile(resultCountries$Gross.domestic.product.2012, probs = c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.0))
class(GDPQuantile)
GDPQuantile
resultCountriesByQuantile <- cut(resultCountries$Gross.domestic.product.2012,breaks = 5)
resultCountriesByQuantile

table(resultCountriesByQuantile, resultCountries$Income.Group)




