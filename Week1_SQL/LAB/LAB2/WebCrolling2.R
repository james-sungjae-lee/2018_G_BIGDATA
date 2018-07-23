# 20180720
# Sungjae Lee

# Import Library and Setting Encoding

library(httr)
library(rvest)
Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")

rawHTML <- paste(readLines("idea.html"), collapse="\n")
myhtml <- read_html(rawHTML)

idea_heads <- html_text(myhtml, 'div.idea h4')
idea_contents <- html_text(myhtml, 'div.idea p')

idea_all <- paste(idea_heads, idea_contents)

# Edit idea data

idea_all = gsub('^.+\\{\\}', ' ', idea_all)
idea_all = gsub('[[:punct:]]', ' ', idea_all)
idea_all = gsub('[A-Za-z0-9]', ' ', idea_all)
idea_all = gsub('[[:space:]]+', ' ', idea_all)
idea_all[1]

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

cps = VCorpus(VectorSource(idea_all))
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
df_wc <- subset(df_wc, word != '서비스')
df_wc <- subset(df_wc, word != '플랫폼')
df_wc <- subset(df_wc, word != '시스템')
df_wc <- subset(df_wc, word != '사이트')
df_wc <- subset(df_wc, word != '아이디어')

library(wordcloud2)
library(htmlwidgets)

wc <- wordcloud2(df_wc, color = 'random-dark')
saveWidget(wc, 'wc.html', selfcontained = F)
