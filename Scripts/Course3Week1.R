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

ucsdb <- dbConnect(MySQL(),user="genome",host="genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucsdb,"show databases;")
print(result)
dbDisconnect(ucsdb)

hg19 <- dbConnect(MySQL(),user="genome",db="hg19",host="genome-mysql.cse.ucsc.edu")
alltables <- dbListTables(hg19)
str(alltables)
dbListFields(hg19,"HInv")
dbGetQuery(hg19,"select count(*) from HInv")
HinvTable <- dbReadTable(hg19,"HInv")
head(HinvTable)
dbDisconnect(hg19)

#query <- dbSendQuery(hg19, "select * from HInv where mrnaAcc")
#fetch(query)
#dbClearResult(query)

