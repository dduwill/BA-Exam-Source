#Exam 2 Question 3
#My Giving Story

library(quanteda)
library(stm)
library(tm)
library(NLP)
library(openNLP)
library(ggplot2)
library(ggdendro)
library(cluster)
library(fpc) 


#load the clean data
setwd("C:/Users/weiyi/Desktop/R/exam2")
precorpus <- read.csv("cleandata.csv", header=TRUE, stringsAsFactors=FALSE)
str(precorpus)
#######################################################################################
#summary of top votes

sort.list <- precorpus[order(precorpus$votes_total, decreasing = T), ]

#first, I want to see insights of the stories, find the top 50 topics
story <- corpus(precorpus$Story_Text) 

#clean corpus: removes punctuation, digits, converts to lower case
corpus.story <- toLower(story, keepAcronyms = F)
corpus.story <- tokenize(corpus.story, 
                              removeNumbers=TRUE,  
                              removePunct = TRUE,
                              removeSeparators=TRUE,
                              removeTwitter=FALSE,
                              ngrams = 2,
                              verbose=TRUE)
dfm.story <- dfm(corpus.story,
                 toLower = TRUE, 
                 ignoredFeatures = c("the", "to", "is", "be", "on", "in", "of", "a", "and", "for", "with", "you",
                                     "it", "but", "these", "will", "that", "â", "this", "who", "i", "or", 
                                     "we", "our", "are", "she", "have", "as", "their", "from", "has", "her",
                                     "not", "was", "at", "my", "been", "them", "ã", "å",
                                     stopwords("SMART")),  
                 verbose = TRUE, 
                 stem = TRUE)
topfeatures.story <- topfeatures(dfm.story, n=50)
topfeatures.story
#search the keyword content - community, life, time 
kwic(corpus.story, "time", window = 2)

#try bigrams
help("tokenize")
cleancorpus <- tokenize(corpus.story, 
                        removeNumbers=TRUE,  
                        removePunct = TRUE,
                        removeSeparators=TRUE,
                        removeTwitter=FALSE, 
                        #ngrams=2, 
                        verbose=TRUE)

dfm.bigram<- dfm(cleancorpus, toLower = FALSE, 
                 ignoredFeatures = c("the", "to", "is", "be", "on", "in", "of", "a", "and", "for", "with", "you",
                                     "it", "but", "these", "will", "that", "â", "this", "who", "i", "or", 
                                     "we", "our", "are", "she", "have", "as", "their", "from", "has", "her",
                                     "not", "was", "at", "my", "been", "them", "ã", "å", 
                                     stopwords("SMART")),
                 verbose=TRUE, 
                 stem=FALSE)
topfeatures.bigram<-topfeatures(dfm.bigram, n=50)
topfeatures.bigram
#search the keyword content - give back
kwic(cleancorpus, "give_back", window = 2)

###############################################################################################
#sentiment analysis
mydict <- dictionary(list(negative = c("detriment*", "bad*", "awful*", "terrib*", "horribl*"),
                          postive = c("good", "great", "super*", "excellent", "wonderful", "nice")))
dfm.sentiment <- dfm(corpus.story, dictionary = mydict)
topfeatures(dfm.sentiment)
View(dfm.sentiment)
help(dfm)
################################################################################################

#topic modeling using stm
library(stm)
#Process the data for analysis.
help("textProcessor")
temp <- textProcessor(documents = precorpus$Story_Text, metadata = precorpus)
names(temp)  # produces:  "documents", "vocab", "meta", "docs.removed" 
meta <- temp$meta
vocab <- temp$vocab
docs <- temp$documents
help("prepDocuments")
out <- prepDocuments(docs, vocab, meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
#running stm for top 3 topics
help("stm")
prevfit <-stm(docs, vocab, 
              K = 3, 
              verbose = TRUE,
              data = meta, 
              max.em.its = 500)
help("labelTopics")
topics <- labelTopics(prevfit, topics=c(1:3))
topics   #shows topics with highest probability words
#explore the topics in context.  Provides an example of the text 
help("findThoughts")
findThoughts(prevfit, texts = trump$content,  topics = 3,  n = 2)
help("plot.STM")
plot.STM(prevfit, type="summary")

###############################################################################################

#Use LDA model
#Cleaning corpus
stop_words <- stopwords("SMART")
## additional junk words showing up in the data
stop_words <- c(stop_words, "just", "get", "will", "can", "also", "much","need")
stop_words <- tolower(stop_words)


cleancorpus <- gsub("'", "", cleancorpus) # remove apostrophes
cleancorpus <- gsub("[[:punct:]]", " ", cleancorpus)  # replace punctuation with space
cleancorpus <- gsub("[[:cntrl:]]", " ", cleancorpus)  # replace control characters with space
cleancorpus <- gsub("^[[:space:]]+", "", cleancorpus) # remove whitespace at beginning of documents
cleancorpus <- gsub("[[:space:]]+$", "", cleancorpus) # remove whitespace at end of documents
cleancorpus <- gsub("[^a-zA-Z -]", " ", cleancorpus) # allows only letters
cleancorpus <- tolower(cleancorpus)  # force to lowercase

## get rid of blank docs
cleancorpus <- cleancorpus[cleancorpus != ""]

# tokenize on space and output as a list:
doc.list <- strsplit(cleancorpus, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)


# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
term.table <- term.table[names(term.table) != ""]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)


# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (1)
W <- length(vocab)  # number of terms in the vocab (8941L)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [46, 27, 106 ...]
N <- sum(doc.length)  # total number of tokens in the data (863558L)
term.frequency <- as.integer(term.table) 

# MCMC and model tuning parameters:
K <- 10
G <- 3000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
## display runtime
t2 - t1  

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

reviews.LDA <- list(phi = phi,
                    theta = theta,
                    doc.length = doc.length,
                    vocab = vocab,
                    term.frequency = term.frequency)

library(LDAvis)
library(servr)

# create the JSON object to feed the visualization:
json <- createJSON(phi = reviews.LDA$phi, 
                   theta = reviews.LDA$theta, 
                   doc.length = reviews.LDA$doc.length, 
                   vocab = reviews.LDA$vocab, 
                   term.frequency = reviews.LDA$term.frequency)

serVis(json, out.dir = 'vis', open.browser = TRUE)
