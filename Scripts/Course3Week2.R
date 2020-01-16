library(httr)
oauth_endpoints("github")

myapp <- oauth_app("github",
                   key = "f952b788514e3c4e8c6c",
                   secret = "7c7bdc312b8e914c0450253864f66aa5bcac239e"
)

github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)
content(req)
print(req)
class(req)

library(jsonlite)
http_type(req)
jsonRespText<-content(req,as="text") 
jsoninfo = fromJSON(jsonRespText)
class(jsoninfo)
names(jsoninfo)
print(jsoninfo$full_name)
dateinfo <- jsoninfo$created_at
print(dateinfo)
print(order(dateinfo,decreasing = FALSE))
print(order(dateinfo,decreasing = TRUE))
dateinfo[26]
dateinfo[15]

print(jsoninfo$full_name)

mask <- jsoninfo[jsoninfo$full_name == "jtleek/datasharing","created_at"]
print(mask)
