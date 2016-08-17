## This program is to scrap comments of a specific movie from douban.com

# How to deal with Error in file(con, "r") ?????????

library(XML)

begin = c()
end = c()
id = c()
link = c()
date = c()
rate = c()
comments = c()
film = list()

for (page in seq(from = 8100, to = 11100, by = 20)) {
      
      url <- paste("http://movie.douban.com/subject/26313973/comments?start=", page, 
                  "&limit=20&sort=time", sep = "")
      web <- readLines(url)
      
      begin = grep("<div class=\"comment\">", web)
      end = grep("        <p class=\"\">", web)
      end = end + 1
      
      for (i in 1:length(begin)) {
            film[i] = list(web[begin[i]:end[i]])
      }
      
      for (i in 1:length(begin)) {
            rawIdLink = film[[i]][grep("<span class=\"comment-info\">", film[[i]])+1]
            rawIdLink = strsplit(rawIdLink, "\" class=\"\">")
            rawLink = gsub("                <a href=\"","",rawIdLink[[1]][1])
            link = c(link, rawLink)
            rawId = gsub("</a>", "", rawIdLink[[1]][2])
            id = c(id, rawId)
            
            rawRate = film[[i]][grep("<span class=\"comment-info\">", film[[i]])+2]
            rawRate = gsub("                    <span class=\"allstar", "", rawRate)
            rawRate = substr(rawRate, 1, 1)
            rate = c(rate, rawRate)
            
            rawDate = film[[i]][grep("<span class=\"comment-info\">", film[[i]])+4]
            rawDate = gsub("                    ", "" ,rawDate)
            date = c(date, rawDate)
            
            rawComments = film[[i]][grep("<p class=\"\">", film[[i]])]
            rawComments = gsub("        <p class=\"\"> ", "", rawComments)
            comments = c(comments, rawComments)
      }
}

data <- data.frame(ID = id, date = date, rate = rate, comments = comments, link = link)
data$comments = as.character(data$comments)
data$date = as.Date(data$date, format = "%Y-%m-%d")
data$link = as.character(data$link)
data = data[!duplicated(data$ID),]
location = c()

for (i in 1:5) {
      url2 = data[i,5]
      web2 = readLines(url2)
      if (any(grepl('<div class="user-info">', web2))) {
            rawLocation = web2[grep('<div class="user-info">', web2)+2]
            rawLocation = gsub('常居:&nbsp;<a href=\"http://beijing.douban.com/\">','',rawLocation)
            location = c(location, rawLocation)
      } else {
            location = c(location, NA)
      }
}

new = rbind(new, data)

save(new, file = "woshizhengrenData.Rda")
data = new
rm(new)
subData <- data[(grepl("演",data[,"comments"]) & grepl("幂",data[,"comments"])),]
subData2 <- data[(grepl("演",data[,"comments"]) & grepl("鹿",data[,"comments"])),]
subData3 <- data[!(grepl("幂",data[,"comments"])),]
subData4 <- data[((grepl("好",data[,"comments"])) | (grepl("不错",data[,"comments"])) | 
                        (grepl("进步",data[,"comments"]))),]


data = new
location = c()
for (i in 41:45) {
      url2 = data[i,5]
      web2 = readLines(url2)
      if (any(grepl('<div class="user-info">', web2))) {
            rawLocation = web2[grep('<div class="user-info">', web2)+2]
            splLocation = strsplit(rawLocation, 'douban.com/')
            rawLocation = gsub('</a><br />','',splLocation[[1]][2])
            rawLocation =  strsplit(rawLocation, '\">')[[1]][2]
            location = c(location, rawLocation)
      } else {
            location = c(location, NA)
      }
}

# [3] "常居:&nbsp;<a href=\"http://www.douban.com/location/hangzhou/\">浙江杭州</a><br />"



urltest = 'http://book.douban.com/people/alarice/collect'
webtest = readLines(urltest)


