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
dataCars
dataCars[!notNA]
