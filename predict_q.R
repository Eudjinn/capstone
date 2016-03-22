library(quanteda)
library(stringi)
library(tm)

# options(mc.cores=1)
set.seed(4444)

source("utils.R")

###############
## Loading data

# blogs <- textfile("final/en_US/en_US.blogs.txt")
# news <- textfile("final/en_US/en_US.news.txt")
# twitter <- textfile("final/en_US/en_US.twitter.txt")

blogs <- readLines("final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
news <- readLines("final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines("final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)
blogs <- iconv(blogs, "UTF-8", "ascii", sub = " ")
news <- iconv(news, "UTF-8", "ascii", sub = " ")
twitter <- iconv(twitter, "UTF-8", "ascii", sub = " ")

all <- c(blogs, news, twitter)

c.blogs <- corpus(blogs)
c.news <- corpus(news)
c.twitter <- corpus(twitter)

c.all <- c.blogs + c.news + c.twitter

## Sampling and cleaning data
sample.size <- as.integer(5000)

blogs.sample <- sample(blogs, sample.size)
news.sample <- sample(news, sample.size)
twitter.sample <- sample(twitter, sample.size)

c.blogs.sample <- corpus(blogs.sample)
c.news.sample <- corpus(news.sample)
c.twitter.sample <- corpus(twitter.sample)

c.all.sample <- c.blogs.sample + c.news.sample + c.twitter.sample

#trigrams <- tokenize(texts(c.all.sample), 
#                    what = "word", 
#                    removeNumbers = TRUE, 
#                    removePunct = TRUE, 
#                    removeSeparators = TRUE, 
#                    ngrams = 3L,
#                    concatenator = " ")

dtm.sample.unigram <- dfm(c.all.sample)
dtm.sample.bigram <- dfm(c.all.sample, ngrams = 2L, concatenator = " ")
dtm.sample.trigram <- dfm(c.all.sample, ngrams = 3L, concatenator = " ")

dtm.sample.unigram <- trim(dtm.sample.unigram, minCount = 10, minDoc = 1) 
dtm.sample.bigram <- trim(dtm.sample.bigram, minCount = 3, minDoc = 1) 
dtm.sample.trigram <- trim(dtm.sample.trigram, minCount = 2, minDoc = 1) 

### Prediction

dtm.sample.unigram.sorted <- sort(dtm.sample.unigram, margin = "features")
dtm.sample.bigram.sorted <- sort(dtm.sample.bigram, margin = "features")
dtm.sample.trigram.sorted <- sort(dtm.sample.trigram, margin = "features")

uni.key <- features(dtm.sample.unigram.sorted)
uni.word <- features(dtm.sample.unigram.sorted)
uni.freq <- colSums(dtm.sample.unigram.sorted)

bi.key <- getFirstWord(features(dtm.sample.bigram.sorted))
bi.word <- getLastWord(features(dtm.sample.bigram.sorted))
bi.freq <- colSums(dtm.sample.bigram.sorted)

tri.key <- getFirstTwoWords(features(dtm.sample.trigram.sorted))
tri.word <- getLastWord(features(dtm.sample.trigram.sorted))
tri.freq <- colSums(dtm.sample.trigram.sorted)

freq.df.l <- list(unigram = data.frame(Key = factor(uni.key, 
                                                    levels = unique(uni.key)), 
                                       Word = factor(uni.word, 
                                                     levels = unique(uni.word)), 
                                       Freq = uni.freq),
                  bigram = data.frame(Key = factor(bi.key, 
                                                   levels = unique(bi.key)),
                                      Word = factor(bi.word, 
                                                    levels = unique(bi.word)), 
                                      Freq = bi.freq),
                  trigram = data.frame(Key = factor(tri.key, 
                                                    levels = unique(tri.key)), 
                                       Word = factor(tri.word, 
                                                     levels = unique(tri.word)), 
                                       Freq = tri.freq))

freq.df.uni <- data.frame(Key = uni.key, 
                          Word = uni.word, 
                          Freq = uni.freq,
                          Ngram = 1)
freq.df.bi <- data.frame(Key = bi.key, 
                          Word = bi.word, 
                          Freq = bi.freq,
                          Ngram = 2)
freq.df.tri <- data.frame(Key = tri.key, 
                          Word = tri.word, 
                          Freq = tri.freq,
                          Ngram = 3)

freq.df <- rbind(freq.df.uni, freq.df.bi, freq.df.tri)

## NEEDS TO BE OPTIMIZED
# Think about dictionary encoding instead of grepping all the time
predictWord <- function(phrase, ngram.df.l, n = 1) {
    trigram.id <- grep(paste0("^", phrase, "$"), ngram.df.l$trigram$Key)
    predicted.id <- 1:n #which.max(prob)
    if(length(trigram.id) > 0) {
        print("trigram")
        trigram.freq <- ngram.df.l$trigram$Freq[trigram.id]
        bigram.id <- intersect(grep(paste0("^", getFirstWord(phrase), "$"), ngram.df.l$bigram$Key),
                               grep(paste0("^", getLastWord(phrase), "$"), ngram.df.l$bigram$Word))
        # frequency is not used now
        bigram.freq <-  ngram.df.l$bigram$Freq[bigram.id]
        # simple maximum likelihood probability, not used now
        prob <- trigram.freq/bigram.freq
        # since all structures are ordered, the best will be on the top
        predicted.word <- ngram.df.l$trigram$Word[trigram.id[predicted.id]]
    } else {
        bigram.id <- grep(paste0("^", getLastWord(phrase), "$"), ngram.df.l$bigram$Key)
        if(length(bigram.id) > 0) {
            print("bigram")
            # frequency is not used now
            bigram.freq <- ngram.df.l$bigram$Freq[bigram.id]
            # since all structures are ordered, the best will be on the top
            predicted.word <- ngram.df.l$bigram$Word[bigram.id[predicted.id]]
        } else {
            print("unigram")
            unigram.id <- ngram.df.l$unigram$Key[1:n]
            predicted.word <- ngram.df.l$unigram$Key[unigram.id[predicted.id]]
        }
    }
    predicted.word
}

unigram.df <- freq.df[freq.df$Ngram == 1, ]
bigram.df <- freq.df[freq.df$Ngram == 2, ]
trigram.df <- freq.df[freq.df$Ngram == 3, ]

predictWord1 <- function(phrase, uni, bi, tri, n = 1) {
    phrase.first <- getFirstWord(phrase)
    phrase.last <- getLastWord(phrase)
    
    trigram.id <- which(phrase == tri$Key)
    predicted.id <- 1:n #which.max(prob)
    
    if(length(trigram.id) > 0) {
        trigram.freq <- tri$Freq[trigram.id]
        bigram.id <- which(phrase.last == bi$Word)
        bigram.freq <- bi$Freq[bigram.id]
        # since all structures are ordered, the best will be on the top
        predicted.word <- tri$Word[trigram.id[predicted.id]]
    } else {
        bigram.id <- which(phrase.last == bi$Key)
        if(length(bigram.id) > 0) {
            # frequency is not used now
            bigram.freq <- bi$Freq[bigram.id]
            # since all structures are ordered, the best will be on the top
            predicted.word <- bi$Word[bigram.id[predicted.id]]
        } else {
            unigram.id <- 1:n
            predicted.word <- uni$Word[unigram.id[predicted.id]]
        }
    }
    predicted.word
}

# predicted <- sapply(testset$pair, function(x) predictWord(x, ngram.df.l, 1))

###################
predictWord("going to", freq.df.l, n = 1)
predictWord1("going to", unigram.df, bigram.df, trigram.df, n = 1)

# REWRITE
all.sample.test <- sample(all, sample.size)
tm.all.sample.test <- Corpus(VectorSource(all.sample.test), readerControl = list(reader = readPlain, language = "en", load = TRUE))

tm.all.sample.test <- tm_map(tm.all.sample.test, content_transformer(function(x){gsub("[^[:alnum:]['-]", " ", x)}))   
tm.all.sample.test <- tm_map(tm.all.sample.test, removeNumbers)
tm.all.sample.test <- tm_map(tm.all.sample.test, stripWhitespace)
tm.all.sample.test <- tm_map(tm.all.sample.test, content_transformer(tolower))
tm.all.sample.test <- tm_map(tm.all.sample.test, content_transformer(PlainTextDocument))
###
devtest <- lapply(tm.all.sample.test[1:100], function(x) splitStringToSet(x$content))
testpairs <- sapply(devtest, function(x) x$pair)
testpairs <- as.character(unlist(testpairs))
testwords <- sapply(devtest, function(x) x$word)
testwords <- as.character(unlist(testwords))
###

predicted <- sapply(testpairs, function(y) predictWord1(y, unigram.df, bigram.df, trigram.df, 1))
predicted <- as.character(predicted)
###
table(testwords == predicted)


