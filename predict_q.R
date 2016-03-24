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
##############################################
all.sample <- c(blogs.sample, news.sample, twitter.sample)
all.sample.length <- length(all.sample)
train.p <- 0.7

## divide into training and test set
inTrain <- sample(1:all.sample.length, all.sample.length * train.p)
all.sample.train <- all.sample[inTrain]
all.sample.test <- all.sample[-inTrain]

# pass vector of character documents
trainTM <- function(t, trimFeatures = FALSE, smoothK = 1, ...) {
    # create corpus on training set
    c <- corpus(t)
    # n-grams
    dtm.l <- list(onegram = dfm(t),
                  twogram = dfm(t, ngrams = 2L, concatenator = " "),
                  threegram = dfm(t, ngrams = 3L, concatenator = " "),
                  fourgram = dfm(t, ngrams = 4L, concatenator = " "),
                  fivegram = dfm(t, ngrams = 5L, concatenator = " "))

    if(trimFeatures) {
        # trim rare terms
        dtm.l.trimmed <- lapply(dtm.l, function(x) trim(x, minCount = 2, minDoc = 1))
    }
    ### Training

    dt.l <- list(onegram = data.table(Key = features(dtm.l.trimmed$onegram), 
                                      Word = features(dtm.l.trimmed$onegram), 
                                      Freq = docfreq(dtm.l.trimmed$onegram, 
                                                     scheme = "count")),
                 twogram = data.table(Key = getFirstNWords(features(dtm.l.trimmed$twogram), 1), 
                                      Word = getLastNWords(features(dtm.l.trimmed$twogram), 1), 
                                      Freq = docfreq(dtm.l.trimmed$twogram, 
                                                     scheme = "count")),
                 threegram = data.table(Key = getFirstNWords(features(dtm.l.trimmed$threegram), 2), 
                                        Word = getLastNWords(features(dtm.l.trimmed$threegram), 1), 
                                        Freq = docfreq(dtm.l.trimmed$threegram, 
                                                       scheme = "count")),
                 fourgram = data.table(Key = getFirstNWords(features(dtm.l.trimmed$fourgram), 3), 
                                       Word = getLastNWords(features(dtm.l.trimmed$fourgram), 1), 
                                       Freq = docfreq(dtm.l.trimmed$fourgram, 
                                                      scheme = "count")),
                 fivegram = data.table(Key = getFirstNWords(features(dtm.l.trimmed$fivegram), 4), 
                                        Word = getLastNWords(features(dtm.l.trimmed$fivegram), 1), 
                                        Freq = docfreq(dtm.l.trimmed$fivegram, 
                                                       scheme = "count")))
    # add smoothing
    Voc.size <- nrow(dt.l$onegram)

    smooth.one <- function(dt, k = 1, V) {
        N <- sum(dt$Freq)
    
        dt[, Prob := (Freq + k)/(N + k*V)]
        dt[, FreqSmooth := Prob * N]
        setkey(dt, Key)
    }
    
    smooth.n <- function(dt, k = 1, V) {
        setkey(dt, Key)
        dt[, Prob := (Freq + k)/(sum(Freq) + k*V), by = Key]
        dt[, FreqSmooth := Prob * sum(Freq), by = Key]
        setkey(dt, Key)
    }
    
    smooth.one(dt.l$onegram, k = smoothK, V = Voc.size)
    smooth.n(dt.l$twogram, k = smoothK, V = Voc.size)
    smooth.n(dt.l$threegram, k = smoothK, V = Voc.size)
    smooth.n(dt.l$fourgram, k = smoothK, V = Voc.size)
    smooth.n(dt.l$fivegram, k = smoothK, V = Voc.size)
    
    fit <- list(dtm.original = dtm.l,
                dtm.trimmed = dtm.l.trimmed,
                dt = dt.l)
    fit
}

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

predictTM <- function(model, phrase, n = 1) {
    # stupid protection from phrases with less then 4 words.
    # need to be rewritten
    dummy <- "<notaword> <notaword> <notaword> <notaword>"
    phrase <- paste(dummy, phrase)
    phrase.four <- getLastNWords(phrase, 4)
    phrase.three <- getLastNWords(phrase, 3)
    phrase.two <- getLastNWords(phrase, 2)
    phrase.one <- getLastNWords(phrase, 1)
    predicted.id <- 1:n #which.max(prob)
    
    predicted.word <- model$dt$fivegram[phrase.four, Word, Prob][order(-Prob)][predicted.id]
    if(is.na(predicted.word$Word[1])) {
        predicted.word <- model$dt$fourgram[phrase.three, Word, Prob][order(-Prob)][predicted.id]
        if(is.na(predicted.word$Word[1])) {
            predicted.word <- model$dt$threegram[phrase.two, Word, Prob][order(-Prob)][predicted.id]
            if(is.na(predicted.word$Word[1])) {
                predicted.word <- model$dt$twogram[phrase.one, Word, Prob][order(-Prob)][predicted.id]
                if(is.na(predicted.word$Word[1])){
                    predicted.word <- model$dt$onegram[, Word, Prob][order(-Prob)][predicted.id] 
                }
            }
        }
    }
    predicted.word
}

# predicted <- sapply(testset$pair, function(x) predictWord(x, ngram.df.l, 1))

###################

fit <- trainTM(all.sample.train, trimFeatures = TRUE, smoothK = 1)

predictTM(fit, "going to be here", n = 1)

# REWRITE



