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
data(DictionaryLM)
str(DictionaryLM)


#Dictionary generation-----------------------------------------------------------------------------

# Create a vector of strings
documents <- c("This is a good thing!",
               "This is a very good thing!",
               "This is okay.",
               "This is a bad thing.",
               "This is a very bad thing.")
response <- c(1, 0.5, 0, -0.5, -1)

# Generate dictionary with LASSO regularization
dict <- generateDictionary(documents, response)
dict
summary(dict)
write(dict, file="dictionary.dict")
dict <- read("dictionary.dict")



#Performance evaluation------------------------------------------

compareDictionaries(dict,
                    loadDictionaryQDAP())

sentiment <- predict(dict, documents)
compareToResponse(sentiment, response)


test_documents <- c("This is neither good nor bad",
                    "What a good idea!",
                    "Not bad")
test_response <- c(0, 1, 1)

pred <- predict(dict, test_documents)

compareToResponse(pred, test_response)

plotSentimentResponse(pred, test_response)

compareToResponse(analyzeSentiment(test_documents), test_response)



