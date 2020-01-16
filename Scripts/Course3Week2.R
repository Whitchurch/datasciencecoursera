#Load the library
library(httr)

#Setup the oauth_enpoint function for giyhub
oauth_endpoints("github")

#create the oauth_app object using the key and secret for our app in question
myapp <- oauth_app("github",
                   key = "f952b788514e3c4e8c6c",
                   secret = "7c7bdc312b8e914c0450253864f66aa5bcac239e"
)

#Generate the git hubtoken.
# Step 1
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

#Step 2: 
gtoken <- config(token = github_token)

# Use token to make a get request to github url of interest
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)
content(req)
print(req)
class(req)

# load JSON lite libraries to manipulate the response received
library(jsonlite)
http_type(req) #Check what kinf of response was received, json, xml etc etc
jsonRespText<-content(req,as="text")  # conver the response to text
jsoninfo = fromJSON(jsonRespText)  # convert the jsontext to dataframe
class(jsoninfo)
names(jsoninfo)
print(jsoninfo$full_name)
dateinfo <- jsoninfo$created_at
print(dateinfo)
print(order(dateinfo,decreasing = FALSE))
print(order(dateinfo,decreasing = TRUE))
dateinfo[26]
dateinfo[15] # some basic exploration of the data frame

print(jsoninfo$full_name)

mask <- jsoninfo[jsoninfo$full_name == "jtleek/datasharing","created_at"]
print(mask) # print the created_at value that has repo name as "jyleek/datasharing" 


#Question 2: 
if(file.exists("./DownloadFiles/Surveydata.csv"))
{
  print("the file exists")
}else
{
  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
  download.file(fileurl,destfile = "./DownloadFiles/Surveydata.csv", method = "curl")
}

install.packages("sqldf")
library(sqldf)
detach("package:RMySQL", unload=TRUE)
acs <- read.csv("./DownloadFiles/Surveydata.csv")
head(acs)

sqldf("select pwgtp1 from acs where AGEP < 50")

#Question(3)
a1 <- unique(acs$AGEP)

a2 <- sqldf("select distinct AGEP from acs")

str(a1)
str(a2)

#Question 4 - Reading from a HTML page
con <- url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlcode <- readLines(con)
close(con)
print(htmlcode)
class(htmlcode)
str(htmlcode)
a<-htmlcode[10]
b<- htmlcode[20]
c <- htmlcode[30]
d <- htmlcode[100]

print(nchar(htmlcode[10]))
print(nchar(htmlcode[20]))
print(nchar(htmlcode[30]))
print(nchar(htmlcode[100]))



