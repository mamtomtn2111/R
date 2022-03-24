install.packages("tm")
install.packages(c("SnowballC","wordcloud","RColorBrewer"))

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

text <- readLines(file.choose())
docs <- Corpus(VectorSource(text))

inspect(docs)

# Xây dựng hàm thay đổi ký tự trắng
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ",x))

noidung <- "tôi yêu em"
gsub("tôi","bạn thân tôi cũng",noidung)

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
#docs <- tm_map(docs, content_transformer(tolower))

docs <- tm_map(docs, removeNumbers)

#Xóa từ dừng
#docs <- tm_map(docs, removeWords, stopwords("english"))

#Remove punctuations
docs <- tm_map(docs, removePunctuation)

#Xóa khoảng trắng
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <-  sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d,10)


#
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 200,
          random.order = FALSE, rot.per = 0.35,
          color= brewer.pal(7, "Dark2"))
