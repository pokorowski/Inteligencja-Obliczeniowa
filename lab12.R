library(tm)
library(ggplot2) 
cname <- file.path("/home/LABPK/pokorowski", "articles")   
docs <- VCorpus(DirSource(cname))   
summary(docs) 
docs <- tm_map(docs,removePunctuation)  

docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, PlainTextDocument)
writeLines(as.character(docs[1]))
docs_st <- tm_map(docs, stemDocument)   
docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1])) 
#docs_stc <- tm_map(docs_st, stemCompletion, dictionary = DocsCopy, lazy=TRUE)
#docs_stc <- tm_map(docs_stc, PlainTextDocument)
docs <- tm_map(docs, stripWhitespace)
dtm <- DocumentTermMatrix(docs)   
dtm   
tdm <- TermDocumentMatrix(docs)   
tdm  

freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq)   
m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="DocumentTermMatrix.csv")   
dtms <- removeSparseTerms(dtm, 0.4) # This makes a matrix that is 20% empty space, maximum.   
dtms
freq <- colSums(as.matrix(dtm))

wf <- data.frame(word=names(freq), freq=freq)  
p <- ggplot(subset(wf, freq>20), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p  



