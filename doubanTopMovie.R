library(XML)
url<-"http://movie.douban.com/top250?format=text"
web <- readLines(url,encoding="UTF-8")

name <- web[grep('<span class=\"title\">[^&]',web)]
name = gsub("                            <span class=\"title\">", "", name)
name = gsub("</span>","",name)

rate <- web[grep("<span class=\"rating_num\" property=\"v:average\">",web)]
rate = gsub("                                <span class=\"rating_num\" property=\"v:average\">","",rate)
rate = gsub("</span>","",rate)
rate = as.numeric(rate)

number <- web[grep("人评价</span>", web)]
number = gsub("                                <span>", "", number)
number = gsub("人评价</span>", "", number)
number = as.numeric(number)

intro <- web[grep("<span class=\"inq\">", web)]
intro = gsub("                                <span class=\"inq\">", "", intro)
intro = gsub("</span>", "", intro)

director <- web[grep("<p class=\"\">", web)+1]
director = strsplit(director, "&nbsp;&nbsp;&nbsp;")
director = sapply(director, function(x) x[1])
director = gsub("                            导演: ", "", director)

more = web[grep("<p class=\"\">", web)+2]
more = strsplit(more, "&nbsp;/&nbsp;")
year = sapply(more, function(x) x[1])
year = gsub("                            ","",year)
location = sapply(more, function(x) x[2])
type = sapply(more, function(x) x[3])

for (i in seq(from = 25, to = 225, by = 25)){
      urlPlus <- paste("http://movie.douban.com/top250?start=",i,sep = "")
      webPlus <- readLines(urlPlus,encoding="UTF-8")
      
      namePlus <- webPlus[grep('<span class=\"title\">[^&]',webPlus)]
      namePlus = gsub("                            <span class=\"title\">", "", namePlus)
      namePlus = gsub("</span>","",namePlus)
      
      ratePlus <- webPlus[grep("<span class=\"rating_num\" property=\"v:average\">",webPlus)]
      ratePlus = gsub("                                <span class=\"rating_num\" property=\"v:average\">","",ratePlus)
      ratePlus = gsub("</span>","",ratePlus)
      ratePlus = as.numeric(ratePlus)
      
      numberPlus <- webPlus[grep("人评价</span>", webPlus)]
      numberPlus = gsub("                                <span>", "", numberPlus)
      numberPlus = gsub("人评价</span>", "", numberPlus)
      numberPlus = as.numeric(numberPlus)
      
      introPlus <- webPlus[grep("<span class=\"inq\">", webPlus)]
      introPlus = gsub("                                <span class=\"inq\">", "", introPlus)
      introPlus = gsub("</span>", "", introPlus)
      
      directorPlus <- webPlus[grep("<p class=\"\">", webPlus)+1]
      directorPlus = strsplit(directorPlus, "&nbsp;&nbsp;&nbsp;")
      directorPlus = sapply(directorPlus, function(x) x[1])
      directorPlus = gsub("                            导演: ", "", directorPlus)
      
      morePlus = webPlus[grep("<p class=\"\">", webPlus)+2]
      morePlus = strsplit(morePlus, "&nbsp;/&nbsp;")
      yearPlus = sapply(morePlus, function(x) x[1])
      yearPlus = gsub("                            ","",yearPlus)
      locationPlus = sapply(morePlus, function(x) x[2])
      typePlus = sapply(morePlus, function(x) x[3])
      
      name = c(name, namePlus)
      rate = c(rate, ratePlus)
      number = c(number, numberPlus)
      intro = c(intro, introPlus)
      director = c(director, directorPlus)
      year = c(year, yearPlus)
      type = c(type, typePlus)
} 

topMovies <- data.frame(name = name, rate = rate, number = number, 
                        director = director, year = year, type = type)
colnames(topMovies)[3] = "comments"
topMovies[81,5] = NA
save(topMovies, file = "topMovies.Rda")

library(ggplot2)
rateByYear <- aggregate(list(rate = topMovies[,"rate"]), by = list(year = topMovies$year), mean)
ggplot(rateByYear) + theme_bw() +
      geom_histogram(aes(year, rate-8, fill = "mean rate of year"), stat = "Identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_fill_manual(values = "darkgreen")
