
library(httr)
library(rvest)
Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")

text_all = c()

base_url <- 'http://news.naver.com/main/list.nhn?mode=LSD&mid=sec&sid1=103&date=20180707&page='

for (j in 1:10){
  url_news <- paste(base_url, j, sep = '')
  
  html_news <- GET(url_news)
  html_news01 <- read_html(html_news)
  link_news <- html_nodes(html_news01, 'div.list_body a')
  link_news01 <- html_attr(link_news, 'href')
  link_news02 <- unique(link_news01)
  link_news03 <- grep('news.naver.com', link_news02, value = T)
  
  
  for(i in 1:length(link_news03)){
    http_contents <- GET(link_news03[i])
    html_contents <- read_html(http_contents)
    
    contents_area <- html_nodes(html_contents, 'div#articleBodyContents')
    
    text <- html_text(contents_area)
    text_all <- c(text_all, text)
  }
  Sys.sleep(time = 2)
}

write.csv(text_all, 'news.csv')
text_all[1]

clz_news = gsub('^.+\\{\\}', ' ', text_all)
clz_news = gsub('[[:punct:]]', ' ', clz_news)
clz_news = gsub('[A-Za-z0-9]', ' ', clz_news)
clz_news = gsub('[가-힣]{2,5}뉴스+', ' ', clz_news)
clz_news = gsub('[[:space:]]+', ' ', clz_news)

n_news = clz_news[nchar(clz_news) < 1000]
nchar(n_news)
write.csv(n_news, 'n_news.csv')
setwd('/Users/sungjae/desktop')
getwd()
n_news
library(RMySQL)
con = dbConnect(drv = MySQL(), 
                dbname = 'my_news',
                user = 'root',
                password = '1234',
                host = 'localhost',
                port = 3306,
                client.flag = CLIENT_MULTI_RESULTS)

dbListTables(con)
dbListFields(con, 'news')

db_news = dbGetQuery(con, 'SELECT text FROM news where no = 1')
db_news

db_news$text <- repair_encoding(db_news$text)
db_news[1, 2]

Encoding(db_news[1, 2])
repair_encoding(db_news$text)
iconv(db_news$text, from = 'ASCII', to = 'UTF-8')


dbDisconnect(con)

library(rJava)
install.packages('KoNLP')
library(KoNLP)
useNIADic()s
extractNoun("아버지와 엄마")

install.packages('tm')
library(tm)

ko_word = function(input){
  text = as.character(input)
  extractNoun(text)
}


cps = VCorpus(VectorSource(db_news$text))
ko_tdm <- TermDocumentMatrix(cps,
                             control = list(
                               tokenize = ko_word,
                               wordLengths = c(2, 7)
                             ))

as.matrix(ko_tdm)





