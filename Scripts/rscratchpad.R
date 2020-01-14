##Reading input as vectors
x <- c(5, 5, 5, 5)
#Printing out the vector
x
#Performing vector arithmetic
x <- x+5

# Convert vector to logical vector
as.logical(x)
x

yvec <- vector("logical",length = 10)
yvec <- as.logical(yvec+TRUE)
yvec

# Playing with lists
zlist <- list(1:10, "Happy","d","a","y","s")
zlist

#Martices
#Normal creation
mat1 <- matrix(1:10, nrow = 5,ncol = 5)
dim(mat1)
mat1

#From vector
vec1 <- vector("numeric", length = 10)
dim(vec1) <- c(5,2)
vec1

#rbind and c bind
row1 <- c(5,5,5)
row2 <- c(20,25,50)

mat3 <- rbind(row1,row2)
dim(mat3)
mat3

# factors
factordemo <- factor(c("Yes","No","No","Yes"))
factordemo
table(factordemo)

factorunclassed <- unclass(factordemo)
factorunclassed

#explicity set which level is associated with what
factordemo1 <- factor(c("Yes","No","No","Yes"), levels = c("Yes","No"))
factordemo1
table(factordemo1)

factorunclassed1 <- unclass(factordemo1)
factorunclassed1

#dealing with missing values
missingVec <- c(1,2,3,NA,NaN,5)
is.na(missingVec)

#Data frames and Name attribute 
xname <- c(1:3)
names(xname) <- c("Apple","Mango","Orange")
xname

#Load a csv file into a dataframe
dataCars <- read.csv("Data/cars.csv")
dataCars

#Using interfaces to read different data types from urls to files
con <- file("Data/cars.csv",r)
dataCars <- read.csv(con)
close(con)

con1 <- url("https://www.facebook.com")
websiteLines <- readLines(con1,30)
websiteLines
close(con1)

#Subsetting lists
subsetC <- c("a","b","b","c",NA)
u <- subsetC > "a"
u
subsetC[u]

u1 <- is.na(subsetC)
u1
subsetC[!u1]

# applying this subset techinque to get mask for the Cars.csv
notNA <- is.na(dataCars)
notNA
dataCars
dataCars[!notNA]
good <- complete.cases(dataCars)
good

x<- 4
class(x)

x1 <- c(4,TRUE)
class(x1)

x <- c(1,3,5)
y <- c(3,2,10)

result <- cbind(x,y)

result
dim(result)

x <- list(2,"abc","b",TRUE)
x[[1]]
class(x)

x <- 1:4
x
y <- 2:3
y

result <- x+y 
result
class(result)

x <- c(3,5,1,10,12,6)
x[x %in% 1:5] <- 0
x

cube <- function(x,n){
  x^3
}

cube(3)

x <- 1:10
if(x > 5)
{
  x <- 0
}

f <- function(x)
{
  g <- function(y)
  {
    y+z
  }
    z <-4
    x+g(x)
  
}

z <- 10
f(3)

x <- 5
y <- if(x < 3)
{
  NA
}else
{
  10
}
y


func1 <- function(x)
{
  
  func2 <- function(y)
  {
    ZZ <- (y + 3 * x)
    
    func3 <- function(ZZ)
    {
      Z <- 10
      ZZ <- ZZ*Z
      ZZ
    }
    
  }
  
  
  func2(4)
  
}

func1(10)

# # Scratch pad of how caching is done with vectors:-
#
# #The code below is the create an object wiht 4 list items (set,get,setmean,getmean) basically
# # Setters and Getters for the object, as well as getters and setters for the mean value
# makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }
#
# # This function takes in the makevector object and , checks if a cached mean value is present
# # if so it returns that vale; else: it calculates the mean for the object. sets the mean for the
# # object via the setmean setter before displaying the mean
# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }
#
# vec1 <- c(1,2,3,4,5,6)
# result <- makeVector(vec1)
# result$get()
# result$getmean()
# class(result)
# cachemean(result)


#Course 3 Quiz 
if(!file.exists("Downloadfiles"))
{
  dir.create("DownloadFiles")
}


if(file.exists("./DownloadFiles/uscommunities.csv"))
{
  print("the file exists")
}else
{
  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
  download.file(fileurl,destfile = "./DownloadFiles/uscommunities.csv", method = "curl")
}

datafile <- read.csv("./DownloadFiles/uscommunities.csv")
summary(datafile)
str(datafile)
names(datafile)

summary(datafile$VAL)
str(datafile$VAL)
print(datafile$VAL)

maskMillion <- datafile$VAL == 24
print(maskMillion)

onlyMillionhomes <- datafile[maskMillion,"VAL"]
print(onlyMillionhomes)

removeNAs <- is.na(onlyMillionhomes)
print(removeNAs)

onlyMillionhomes <- onlyMillionhomes[!removeNAs]
print(onlyMillionhomes)
ncol(onlyMillionhomes)

print(datafile$FES)
str(datafile$FES)
head(datafile$FES)

if(file.exists("./DownloadFiles/naturalgasacquisitions.xslx"))
{
  print("the file exists")
}else
{
  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
  download.file(fileurl,destfile = "./DownloadFiles/naturalgasacquisitions.xslx", method = "curl")
}


rowIndex1 <- 18:23
colIndex1 <- 7:15
dat <- read.xlsx(fileurl,sheet = 1, cols = colIndex1, rows = rowIndex1)
str(dat)
nrow(dat)
ncol(dat)
sum(dat$Zip*dat$Ext,na.rm=T)

#Dealing with XML files
library(XML)
library(methods)
fileurl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc1 <- xmlTreeParse(fileurl,useInternalNodes = TRUE)

rootNode <- xmlRoot(doc1)
zipcodevector <- xpathSApply(rootNode,"//zipcode",xmlValue)
str(zipcodevector)

zipcode21231 <- zipcodevector == "21231"
print(zipcode21231)
print(sum(as.integer(zipcode21231)))


#datafromXML <- xmlToDataFrame(doc1)
#head(datafromXML)

if(!file.exists("Downloadfiles"))
{
  dir.create("DownloadFiles")
}

if(file.exists("./DownloadFiles/idaho.csv"))
{
  print("the file exists")
}else
{
  fileurl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
  download.file(fileurl,destfile = "./DownloadFiles/idaho.csv", method = "curl")
}

DT <- read.csv("./DownloadFiles/idaho.csv")
head(DT)

system.time({mean(DT[DT$SEX == 1,]$pwgtp15); mean(DT[DT$SEX == 2,]$pwgtp15)})
system.time({sapply(split(DT$pwgtp15,DT$SEX),mean)})
system.time({tapply(DT$pwgtp15,DT$SEX,mean)})
#system.time({rowMeans(DT)[DT$SEX == 1]; rowMeans(DT)[DT$SEX == 2]})
system.time({mean(DT$pwgtp15, by=DT$SEX)})
system.time({DT[,mean(pwgtp15),by=SEX]})

print(mean(DT$pwgtp15, by=DT$SEX))
print(sapply(split(DT$pwgtp15,DT$SEX),mean))
print(rowMeans(DT)[DT$SEX == 1]) 
print(rowMeans(DT)[DT$SEX == 2])

print(tapply(DT$pwgtp15,DT$SEX,mean))
print(sapply(split(DT$pwgtp15,DT$SEX),mean))
mean(DT$pwgtp15,by=DT$SEX)
