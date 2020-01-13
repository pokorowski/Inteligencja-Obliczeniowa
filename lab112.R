#https://cran.r-project.org/web/packages/SentimentAnalysis/vignettes/SentimentAnalysis.html

library(tm)
library(SentimentAnalysis)

# Analyze a single string to obtain a binary response (positive / negative)
sentiment <- analyzeSentiment("Yeah, this was a great soccer game for the German team!")
convertToBinaryResponse(sentiment)$SentimentQDAP

# Create a vector of strings
documents <- c("Wow, I really like the new light sabers!",
               "That book was excellent.",
               "R is a fantastic language.",
               "The service in this restaurant was miserable.",
               "This is neither positive or negative.",
               "The waiter forget about my dessert -- what poor service!")

# Analyze sentiment
sentiment <- analyzeSentiment(documents)

# Extract dictionary-based sentiment according to the QDAP dictionary
sentiment$SentimentQDAP

# View sentiment direction (i.e. positive, neutral and negative)
convertToDirection(sentiment$SentimentQDAP)
response <- c(+1, +1, +1, -1, 0, -1)

compareToResponse(sentiment, response)
compareToResponse(sentiment, convertToBinaryResponse(response))
plotSentimentResponse(sentiment$SentimentQDAP, response)

#SentimentDictionaryWordlist------------------------
d <- SentimentDictionaryWordlist(c("uncertain", "possible", "likely"))
summary(d)

#SentimentDictionaryBinary--------------------------
d <- SentimentDictionaryBinary(c("increase", "rise", "more"),
                               c("fall", "drop"))
summary(d)

#SentimentDictionaryWeighted------------------------
d <- SentimentDictionaryWeighted(c("increase", "decrease", "exit"),
                                 c(+1, -1, -10),
                                 rep(NA, 3))
summary(d)

#Built-in dictionaries------------

  
# Make dictionary available in the current R environment
  data(DictionarHE)

# Display the internal structure 
str(DictionaryHE)

# Access dictionary as an object of type SentimentDictionary
dict.HE <- loadDictionaryHE()
# Print summary statistics of dictionary
summary(dict.HE)
## Dictionary type:  binary (positive / negative)
## Total entries:    97
## Positive entries: 53 (54.64%)
## Negative entries: 44 (45.36%)
data(DictionaryLM)
str(DictionaryLM)
