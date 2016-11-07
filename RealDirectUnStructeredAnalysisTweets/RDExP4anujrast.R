#Reading the 
fileRead = readLines('Total.json', warn = FALSE)
fileRead_df = jsonlite::fromJSON(fileRead)

#Extracting only text
onlyText <- fileRead_df$text


#Counting the number of tweets with keyWord Rent or Buy
rentCases <- lapply(onlyText,function(x) { grep(as.character(x), pattern = "\\<rent")} )
sum(unlist(rentCases))

buyCases <- lapply(onlyText,function(x) { grep(as.character(x), pattern = "\\<buy")} )
sum(unlist(buyCases))


#Installing NLP Library Function
install.packages('tm')
library(tm)

#CLeaning the data
myCorpus = Corpus(VectorSource(onlyText))
myCorpus2 = tm_map(myCorpus, content_transformer(tolower))
myCorpus3 = tm_map(myCorpus2, removeWords, stopwords('English'))
myCorpus4 = tm_map(myCorpus3, removePunctuation)

#Converting The corpus in the form of TermDocumentMatrix 
tdm = TermDocumentMatrix(myCorpus4)

#finding the frequent Terms
maxFreqTerms = findFreqTerms(tdm, lowfreq = 150, highfreq = Inf)
maxFreqTerms

#Finding the closely related words to rent and buy
findAssocs(tdm, 'rent' , 0.2)
findAssocs(tdm, 'buy', 0.2)
#plotting the ggPlot for the most frequent words
library(ggplot2)
termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency >= 100)
df <- data.frame(term = names(termFrequency), freq = termFrequency)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity")+xlab("Terms") + ylab("Frequency") + coord_flip()


#Clustering the terms and drawing the co-relation
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D2")
plot(fit)


#REFERENCES
#Major Learning and Help for this analysis has been drawn from URL: 
#https://sites.google.com/site/miningtwitter/basics/text-mining
#http://www2.rdatamining.com/uploads/5/7/1/3/57136767/rdatamining-slides-text-mining.pdf
