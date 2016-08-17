library(XML)

begin = c()
end = c()
film = list()
link = c()
id = c()
location = c()
rate = c()
tag = c()
page = 0

for (page in seq(from = 0, to = 180, by = 20)){
      url = paste("http://movie.douban.com/subject/26313973/collections?start=", page, sep = "")
      web <- readLines(url)
      
      begin = grep("<table width=\"100%\" class=\"\">",web)
      end = grep("</table>",web)
      for (i in 1:20) {
            film[i] = list(web[begin[i]:end[i+1]])
      }
      for (i in 1:20) {
            rawIdLink = film[[i]][grep("<div class=\"pl2\">",film[[i]])+1]
            rawIdLink = strsplit(rawIdLink, "\" class=\"\">")
            rawLink = gsub("                        <a href=\"","",rawIdLink[[1]][1])
            link = c(link, rawLink)
            id = c(id, rawIdLink[[1]][2])
            
            rawLocation = film[[i]][grep("<div class=\"pl2\">",film[[i]])+2]
            if (any(grepl("font",rawLocation))) {
                  rawLocation = gsub("                            <span style=\"font-size:12px;\">\\(", "", rawLocation)
                  rawLocation = gsub(")</span>", "", rawLocation)
                  location = c(location, rawLocation)
            } else {
                  location = c(location, NA)
            }
            
            if (any(grepl("&nbsp;<span class=\"allstar", film[[i]]))) {
                  rawRate = film[[i]][grep("&nbsp;<span class=\"allstar",film[[i]])]
                  rawRate = gsub("                            &nbsp;<span class=\"allstar", "", rawRate)
                  rawRate = substr(rawRate,1,1)
                  rate = c(rate, rawRate)
            } else {
                  rate = c(rate, NA)
            }
            
            if (any(grepl("&nbsp;&nbsp;&nbsp;&nbsp;<span class=\"\">tags", film[[i]]))) {
                  rawTag = film[[i]][grep("&nbsp;&nbsp;&nbsp;&nbsp;<span class=\"\">tag",film[[i]])]
                  rawTag = gsub("                            &nbsp;&nbsp;&nbsp;&nbsp;<span class=\"\">tags: ", "", rawTag)
                  rawTag = gsub("</span>", "", rawTag)
                  tag = c(tag,rawTag)
            } else {
                  tag = c(tag, NA)
            }
      }
}

data <- data.frame(ID = id, location = location, rate = rate, tags = tag, link = link)
