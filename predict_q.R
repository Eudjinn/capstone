library(quanteda)
library(stringi)
library(data.table)
library(parallel)
library(hash)

options(mc.cores = max(1, detectCores()))

source("utils.R")

sample.size <- as.integer(5000)
sample.percent <- as.numeric(0.05)
test <- 1

###############
## Loading data

blogs <- readLines("final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
news <- readLines("final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines("final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

all <- c(blogs, news, twitter)

## Sampling and cleaning data
if(test) {
    blogs.sample <- sample(blogs, length(blogs) * sample.percent)
    news.sample <- sample(news, length(news) * sample.percent)
    twitter.sample <- sample(twitter, length(twitter) * sample.percent)
} else {
    blogs.sample <- blogs
    news.sample <- news
    twitter.sample <- twitter
}

blogs.sample <- cleandoc(blogs.sample)
news.sample <- cleandoc(news.sample)
twitter.sample <- cleandoc(twitter.sample)

all.sample <- c(blogs.sample, news.sample, twitter.sample)

c.blogs.sample <- corpus(blogs.sample)
c.news.sample <- corpus(news.sample)
c.twitter.sample <- corpus(twitter.sample)

# free space
# rm(blogs, news, twitter, blogs.sample, news.sample, twitter.sample)

c.all.sample <- c.blogs.sample + c.news.sample + c.twitter.sample

# free space
rm(c.blogs.sample, c.news.sample, c.twitter.sample)
   
dtm.sample.unigram <- dfm(c.all.sample)
dtm.sample.bigram <- dfm(c.all.sample, ngrams = 2L, concatenator = " ")
dtm.sample.trigram <- dfm(c.all.sample, ngrams = 3L, concatenator = " ")
dtm.sample.fourgram <- dfm(c.all.sample, ngrams = 4L, concatenator = " ")
dtm.sample.fivegram <- dfm(c.all.sample, ngrams = 5L, concatenator = " ")

# free space
rm(c.all.sample)

#dtm.sample.unigram <- trim(dtm.sample.unigram, minCount = 10, minDoc = 1) 
#dtm.sample.bigram <- trim(dtm.sample.bigram, minCount = 3, minDoc = 1) 
#dtm.sample.trigram <- trim(dtm.sample.trigram, minCount = 2, minDoc = 1) 
#dtm.sample.fourgram <- trim(dtm.sample.fourgram, minCount = 2, minDoc = 1) 
#dtm.sample.fivegram <- trim(dtm.sample.fivegram, minCount = 2, minDoc = 1) 

### Prediction

#dtm.sample.unigram <- sort(dtm.sample.unigram, margin = "features")
#dtm.sample.bigram <- sort(dtm.sample.bigram, margin = "features")
#dtm.sample.trigram <- sort(dtm.sample.trigram, margin = "features")
#dtm.sample.fourgram <- sort(dtm.sample.fourgram, margin = "features")
#dtm.sample.fivegram <- sort(dtm.sample.fivegram, margin = "features")

freq.dt.uni <- data.table(Key = features(dtm.sample.unigram), 
                          Word = features(dtm.sample.unigram), 
                          Freq = docfreq(dtm.sample.unigram, 
                                         scheme = "count"),
                          key = c("Key", "Freq"))
freq.dt.bi <- data.table(Key = getFirstWord(features(dtm.sample.bigram)), 
                         Word = getLastWord(features(dtm.sample.bigram)), 
                         Freq = docfreq(dtm.sample.bigram, 
                                        scheme = "count"),
                         key = c("Key", "Freq"))
freq.dt.tri <- data.table(Key = getFirstTwoWords(features(dtm.sample.trigram)), 
                          Word = getLastWord(features(dtm.sample.trigram)), 
                          Freq = docfreq(dtm.sample.trigram, 
                                         scheme = "count"),
                          key = c("Key", "Freq"))
freq.dt.four <- data.table(Key = getFirstThreeWords(features(dtm.sample.fourgram)), 
                          Word = getLastWord(features(dtm.sample.fourgram)), 
                          Freq = docfreq(dtm.sample.fourgram, 
                                         scheme = "count"),
                          key = c("Key", "Freq"))
freq.dt.five <- data.table(Key = getFirstFourWords(features(dtm.sample.fivegram)), 
                           Word = getLastWord(features(dtm.sample.fivegram)), 
                           Freq = docfreq(dtm.sample.fivegram, 
                                          scheme = "count"),
                           key = c("Key", "Freq"))

setorder(freq.dt.uni, -Freq)
setorder(freq.dt.bi, -Freq)
setorder(freq.dt.tri, -Freq)
setorder(freq.dt.four, -Freq)
setorder(freq.dt.five, -Freq)


predictWord1 <- function(phrase, uni, bi, tri, four, five, n = 1) {
    # stupid protection from phrases with less then 4 words.
    # need to be rewritten
    dummy <- "<notaword> <notaword> <notaword> <notaword>"
    phrase <- paste(dummy, phrase)
    phrase.four <- getLastNWords(phrase, 4)
    phrase.three <- getLastNWords(phrase, 3)
    phrase.two <- getLastNWords(phrase, 2)
    phrase.one <- getLastNWords(phrase, 1)
    predicted.id <- 1:n #which.max(prob)
    
    fivegram.id <- which(phrase.four == five$Key)
    if(length(fivegram.id) > 0) {
        predicted.word <- five$Word[fivegram.id[predicted.id]]
    } else {
        fourgram.id <- which(phrase.three == four$Key)
        if(length(fourgram.id) > 0) {
     #       fourgram.freq <- tri$Freq[trigram.id]
            #        bigram.id <- which(phrase.last == bi$Word)
            #        bigram.freq <- bi$Freq[bigram.id]
            # since all structures are ordered, the best will be on the top
            predicted.word <- four$Word[fourgram.id[predicted.id]]
        } else {
            trigram.id <- which(phrase.two == tri$Key)
            if(length(trigram.id) > 0) {
    #            trigram.freq <- tri$Freq[trigram.id]
        #        bigram.id <- which(phrase.last == bi$Word)
        #        bigram.freq <- bi$Freq[bigram.id]
                # since all structures are ordered, the best will be on the top
                predicted.word <- tri$Word[trigram.id[predicted.id]]
            } else {
                bigram.id <- which(phrase.one == bi$Key)
                if(length(bigram.id) > 0) {
                    # frequency is not used now
        #            bigram.freq <- bi$Freq[bigram.id]
                    # since all structures are ordered, the best will be on the top
                    predicted.word <- bi$Word[bigram.id[predicted.id]]
                } else {
                    unigram.id <- 1:n
                    predicted.word <- uni$Word[unigram.id[predicted.id]]
                }
            }
        }
    }
    predicted.word
}

predictWord2 <- function(phrase, uni, bi, tri, four, five, n = 1) {
    # stupid protection from phrases with less then 4 words.
    # need to be rewritten
    dummy <- "<notaword> <notaword> <notaword> <notaword>"
    phrase <- paste(dummy, phrase)
    phrase.four <- getLastNWords(phrase, 4)
    phrase.three <- getLastNWords(phrase, 3)
    phrase.two <- getLastNWords(phrase, 2)
    phrase.one <- getLastNWords(phrase, 1)
    predicted.id <- 1:n #which.max(prob)
    
    predicted.word <- five$Word[five$Key == phrase.four][predicted.id]
    if(is.na(predicted.word[1])) {
        predicted.word <- four$Word[four$Key == phrase.three][predicted.id]
        if(is.na(predicted.word[1])) {
            predicted.word <- tri$Word[tri$Key == phrase.two][predicted.id]
            if(is.na(predicted.word[1])) {
                predicted.word <- bi$Word[bi$Key == phrase.one][predicted.id]
                if(is.na(predicted.word[1])){
                    predicted.word <- uni$Word[predicted.id]
                }
            }
        }
    }
    predicted.word
}

# predicted <- sapply(testset$pair, function(x) predictWord(x, ngram.df.l, 1))

###################
predictWord1("going to be here", freq.dt.uni, freq.dt.bi, freq.dt.tri, freq.dt.four, freq.dt.five, n = 5)

# REWRITE



