# 20180707
# Sungjae Lee

# Import Library and Setting Encoding

library(httr)
library(rvest)
Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")

# Get news data from web site

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

# Edit news data

clz_news = gsub('^.+\\{\\}', ' ', text_all)
clz_news = gsub('[[:punct:]]', ' ', clz_news)
clz_news = gsub('[A-Za-z0-9]', ' ', clz_news)
clz_news = gsub('[가-힣]{2,5}뉴스+', ' ', clz_news)
clz_news = gsub('[[:space:]]+', ' ', clz_news)
clz_news[1]

# Install Korean Natural Language Processing package

library(rJava)
library(KoNLP)
library(tm)
useNIADic()

# Calculate Word Frequency

ko_word = function(input){
  text = as.character(input)
  extractNoun(text)
}

cps = VCorpus(VectorSource(clz_news))
ko_tdm <- TermDocumentMatrix(cps,
                             control = list(
                               tokenize = ko_word,
                               word_Lengths = c(2, 7)
                               )
                             )
ko_mat <- as.matrix(ko_tdm)

word_freq <- rowSums(ko_mat)

str(word_freq)

# Create Word Cloud

word_name = names(word_freq)

df_wc <- data.frame(word = word_name, freq = word_freq)

library(wordcloud2)
library(htmlwidgets)
getwd()
setwd('/Users/sungjae/desktop')
wc <- wordcloud2(df_wc, color = 'random-dark')
saveWidget(wc, 'wc.html', selfcontained = F)

# Create Word Relationship Graph

library(qgraph)

word_order <- order(word_freq, decreasing = T)[1 : 50]
occ_mat <- ko_mat[word_order]
occ_mat2 <- occ_mat %*% t(occ_mat)

png('associ.png', width = 500, height = 500)
quartz(width = 7, height = 7, bg = 'white')

qgraph(occ_mat2, layout = 'spring','circle', color = 'blue',
       vsize = log(diag(occ_mat2)),
       label.color = 'black', labels = colnames(occ_mat2),
       diag = F)
dev.off()
