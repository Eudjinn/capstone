library(quanteda)
library(stringi)
library(data.table)
library(parallel)
library(hash)
library(caret)

options(mc.cores = max(1, detectCores()))

source("utils.R")

sample.size <- as.integer(5000)
sample.percent <- as.numeric(0.1)
test <- 1

###############
## Loading data

blogs <- readLines("final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
news <- readLines("final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines("final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

all <- c(blogs, news, twitter)

## Sampling
if(test) {
    blogs.sample <- sample(blogs, length(blogs) * sample.percent)
    news.sample <- sample(news, length(news) * sample.percent)
    twitter.sample <- sample(twitter, length(twitter) * sample.percent)
} else {
    blogs.sample <- blogs
    news.sample <- news
    twitter.sample <- twitter
}

## cleaning
blogs.sample <- cleandoc(blogs.sample)
news.sample <- cleandoc(news.sample)
twitter.sample <- cleandoc(twitter.sample)

all.sample <- c(blogs.sample, news.sample, twitter.sample)
all.sample.length <- length(all.sample)
train.p <- 0.7
## divide into training and test set
inTrain <- sample(1:all.sample.length, all.sample.length * train.p)
all.sample.train <- all.sample[inTrain]
all.sample.test <- all.sample[-inTrain]

# create corpus on training set
c.all.sample.train <- corpus(all.sample.train)

# n-grams
dtm.sample.l <- list(onegram = dfm(c.all.sample.train),
                     twogram = dfm(c.all.sample.train, ngrams = 2L, concatenator = " "),
                     threegram = dfm(c.all.sample.train, ngrams = 3L, concatenator = " "),
                     fourgram = dfm(c.all.sample.train, ngrams = 4L, concatenator = " "),
                     fivegram = dfm(c.all.sample.train, ngrams = 5L, concatenator = " "))

# trim rare terms
dtm.sample.l.t <- lapply(dtm.sample.l, function(x) trim(x, minCount = 2, minDoc = 1))

### Prediction

dt.l <- list(onegram = data.table(Key = features(dtm.sample.l.t$onegram), 
                                  Word = features(dtm.sample.l.t$onegram), 
                                  Freq = docfreq(dtm.sample.l.t$onegram, 
                                                 scheme = "count")),
             twogram = data.table(Key = getFirstNWords(features(dtm.sample.l.t$twogram), 1), 
                                  Word = getLastNWords(features(dtm.sample.l.t$twogram), 1), 
                                  Freq = docfreq(dtm.sample.l.t$twogram, 
                                                 scheme = "count")),
             threegram = data.table(Key = getFirstNWords(features(dtm.sample.l.t$threegram), 2), 
                                    Word = getLastNWords(features(dtm.sample.l.t$threegram), 1), 
                                    Freq = docfreq(dtm.sample.l.t$threegram, 
                                                   scheme = "count")),
             fourgram = data.table(Key = getFirstNWords(features(dtm.sample.l.t$fourgram), 3), 
                                   Word = getLastNWords(features(dtm.sample.l.t$fourgram), 1), 
                                   Freq = docfreq(dtm.sample.l.t$fourgram, 
                                                  scheme = "count")),
             fivegram = data.table(Key = getFirstNWords(features(dtm.sample.l.t$fivegram), 4), 
                                    Word = getLastNWords(features(dtm.sample.l.t$fivegram), 1), 
                                    Freq = docfreq(dtm.sample.l.t$fivegram, 
                                                   scheme = "count")))
# add smoothing
smooth.one <- function(dt, k = 1, v = 5) {
    dt[, Prob := ((Freq + k)/(sum(Freq) + v))]
    setkey(dt, Key)
}

smooth.n <- function(dt, k = 1, v = 5) {
    setkey(dt, Key)
    dt[, Prob := ((Freq + k)/(sum(Freq) + v)), by = Key]
    setkey(dt, Key)
}

smooth.one(dt.l$onegram, k = 1, v = 5)
smooth.n(dt.l$twogram, k = 1, v = 5)
smooth.n(dt.l$threegram, k = 1, v = 5)
smooth.n(dt.l$fourgram, k = 1, v = 5)
smooth.n(dt.l$fivegram, k = 1, v = 5)

# calc probability of each word within the group (i.e. fivegram/fourgram)
# freq.dt.five[, Freq/sum(Freq), by = Key]

predictWord3 <- function(phrase, one, two, three, four, five, n = 1) {
    # stupid protection from phrases with less then 4 words.
    # need to be rewritten
    dummy <- "<notaword> <notaword> <notaword> <notaword>"
    phrase <- paste(dummy, phrase)
    phrase.four <- getLastNWords(phrase, 4)
    phrase.three <- getLastNWords(phrase, 3)
    phrase.two <- getLastNWords(phrase, 2)
    phrase.one <- getLastNWords(phrase, 1)
    predicted.id <- 1:n #which.max(prob)
    
    predicted.word <- five[phrase.four, Word, Prob][order(-Prob)][predicted.id]
    if(is.na(predicted.word$Word[1])) {
        predicted.word <- four[phrase.three, Word, Prob][order(-Prob)][predicted.id]
        if(is.na(predicted.word$Word[1])) {
            predicted.word <- three[phrase.two, Word, Prob][order(-Prob)][predicted.id]
            if(is.na(predicted.word$Word[1])) {
                predicted.word <- two[phrase.one, Word, Prob][order(-Prob)][predicted.id]
                if(is.na(predicted.word$Word[1])){
                    predicted.word <- one[, Word, Prob][order(-Prob)][predicted.id] 
                }
            }
        }
    }
    predicted.word
}

# predicted <- sapply(testset$pair, function(x) predictWord(x, ngram.df.l, 1))

###################
predictWord2("going to be here", dt.l$one, dt.l$two, dt.l$three, dt.l$four, dt.l$five, n = 5)
predictWord3("going to be here", dt.l$one, dt.l$two, dt.l$three, dt.l$four, dt.l$five, n = 1)

# REWRITE



