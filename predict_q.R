library(quanteda)
library(stringi)
library(tm)

# options(mc.cores=1)
set.seed(4444)

########### Util
getFirstThreeWords <- function(s) {
#    stri_extract_first(s, regex = "^[a-z]* [a-z]* [a-z]*")
    stri_extract_first(s, regex = "^[^ ]* [^ ]* [^ ]*")
}

getFirstTwoWords <- function(s) {
#    stri_extract_first(s, regex = "^[a-z]* [a-z]*")
    stri_extract_first(s, regex = "^[^ ]* [^ ]*")
}

getFirstWord <- function(s) {
 #   stri_extract_first(s, regex = "^[a-z]*")
    stri_extract_first(s, regex = "^[^ ]*")
}

getLastWord <- function(s) {
#    stri_extract_first(s, regex = "[a-z]*$")
        stri_extract_first(s, regex = "[^ ]*$")
}

removeTwoFirstWords <- function(s) {
#    stri_replace_first(s, "", regex = "^[a-z]* [a-z]* *")
    stri_replace_first(s, "", regex = "^[^ ]* [^ ]* *")
}

removeFirstWord <- function(s) {
#    stri_replace_first(s, "", regex = "^[a-z]* *")
    stri_replace_first(s, "", regex = "^[^ ]* *")
}
###############
## Loading data

# blogs <- textfile("final/en_US/en_US.blogs.txt")
# news <- textfile("final/en_US/en_US.news.txt")
# twitter <- textfile("final/en_US/en_US.twitter.txt")

blogs <- readLines("final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
news <- readLines("final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines("final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)
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

# predicted <- sapply(testset$pair, function(x) predictWord(x, ngram.df.l, 1))

splitStringToSet <- function(s) {
    triples <- list()
    triple <- triples
    len <- stri_count_words(s)
    while(len > 2) {
        three <- getFirstThreeWords(s)
        two <- getFirstTwoWords(three)
        triple$pair <- c(triple$pair, two)
        triple$word <- c(triple$word, removeTwoFirstWords(three))
        
        s <- removeFirstWord(s)
        len <- len - 1
    }
    triples <- c(triples, triple)
    triples
}

###################
predictWord("going to", freq.df.l, n = 1)

superpaste <- function(x) { 
    s <- character()
    for(i in 1:length(x)) {
        s <- paste(s, x[i])
    }
    s
}
# REWRITE
all.sample.test <- sample(all, sample.size)
tm.all.sample.test <- Corpus(VectorSource(all.sample.test), readerControl = list(reader = readPlain, language = "en", load = TRUE))

tm.all.sample.test <- tm_map(tm.all.sample.test, removePunctuation, preserve_intra_word_dashes = TRUE)   
tm.all.sample.test <- tm_map(tm.all.sample.test, removeNumbers)
tm.all.sample.test <- tm_map(tm.all.sample.test, stripWhitespace)
tm.all.sample.test <- tm_map(tm.all.sample.test, content_transformer(tolower))
tm.all.sample.test <- tm_map(tm.all.sample.test, content_transformer(PlainTextDocument))
###
devtest <- lapply(tm.all.sample.test[1:10], function(x) splitStringToSet(x$content))
testwords <- sapply(devtest, function(x) x$word)
testwords <- as.character(unlist(testwords))
###
predicted <- sapply(devtest, function(x) {
    sapply(x$pair, function(y) predictWord(y, freq.df.l, 1))
})
predicted <- unlist(predicted)
predicted <- as.character(predicted)
###
table(testwords == predicted)


