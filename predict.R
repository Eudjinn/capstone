library(tm)
library(RWeka)
library(wordcloud)
library(ggplot2)
library(grid)
library(gridExtra)
library(stringi)

# wordcloud/RWeka does not work with parallel processing when NGramTokenizer is used.
options(mc.cores=1)
set.seed(4444)

## Loading data
# this is needed for tokenizer in DocumentTermMatrix to work. For some reason does not with parallel processing.
# Found solution here: http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka

ds <- DirSource(directory = "final/en_US/")
docs <- Corpus(ds, readerControl = list(reader = readPlain, language = "en", load = TRUE))

docs.num <- length(docs)
docs.name <- c("blogs", "news", "twitter")

# gather simple information about documents corpus
docs.info.df <- data.frame(FileName = character(), 
                           NumberOfRows = integer(),
                           NumberOfWords = integer())
for(i in 1:docs.num) {
    docs.info.df <- rbind(docs.info.df, 
                          data.frame(FileName = docs[[i]]$meta$id, 
                                     NumberOfRows = length(docs[[i]]$content),
                                     NumberOfWords = sum(stri_count_words(docs[[i]]$content))))
}

## Sampling and cleaning data
sample.size <- as.integer(10000)
# copy original doc for processing (don't know how to make it more efficient yet)
docs.sample <- docs
# select random sample of sample.size from the original
# iterate through documents in the corpus
for (i in 1:docs.num) {
    # get length of each document (number of lines)
    l <- length(docs[[i]]$content)
    # get random sample of lines in the document and replace the original doc in the corpus with the smaller sample.
    docs.sample[[i]]$content <- docs[[i]]$content[sample(1:l, as.integer(sample.size))]
}

# remove punctuation
docs.sample <- tm_map(docs.sample, removePunctuation)   
# remove numbers
docs.sample <- tm_map(docs.sample, removeNumbers)
# remove unnecessary whitespaces
docs.sample <- tm_map(docs.sample, stripWhitespace)
# to lowecase
docs.sample <- tm_map(docs.sample, content_transformer(tolower))
# finalize preprocessing
docs.sample <- tm_map(docs.sample, content_transformer(PlainTextDocument))

##  create document term matrix for all documents
bigramTokenizer <- function(x) NGramTokenizer(x, 
                                Weka_control(min = 2, max = 2))
trigramTokenizer <- function(x) NGramTokenizer(x, 
                                Weka_control(min = 3, max = 3))

dtm.sample.l <- list(unigram = DocumentTermMatrix(docs.sample),
                     bigram = DocumentTermMatrix(docs.sample, control = list(tokenize = bigramTokenizer)),
                     trigram = DocumentTermMatrix(docs.sample, control = list(tokenize = trigramTokenizer)))

# remove sparse terms
# dtms.sample.l <- lapply(dtm.sample.l, function(x) removeSparseTerms(x, 0.1))
dtms.sample.l <- dtm.sample.l

## Exploratory analysis
### Basic information about the data

# gather information about sample corpus
docs.sample.info.df <- data.frame(FileName = character(), 
                                  NumberOfRows = integer(),
                                  NumberOfWords = integer(),
                                  NumberOfTermsPerDocument = integer())
for(i in 1:docs.num) {
    docs.sample.info.df <- rbind(docs.sample.info.df, 
                                 data.frame(FileName = docs.sample[[i]]$meta$id, 
                                            NumberOfRows = length(docs.sample[[i]]$content),
                                            NumberOfWords = sum(dtm.sample.l$unigram$v[dtm.sample.l$unigram$i == i]),
                                            NumberOfTermsPerDocument = sum(dtm.sample.l$unigram$v != 0 & 
                                                                               dtm.sample.l$unigram$i == i)))
}

# show output of DocumentTermMatrix with sparse terms
dtm.sample.l$unigram

### Questions of the analysis
#### 1. Some words are more frequent than others - what are the distributions of word frequencies?  

# Create matrixes
dtms.sample.m.l <- list(unigram = as.matrix(dtms.sample.l$unigram),
                        bigram = as.matrix(dtms.sample.l$bigram),
                        trigram = as.matrix(dtms.sample.l$trigram))

# sort frequencies of terms in separate documents
# input - document term matrix
getTopTerms <- function(ngram.m, n = 20) {

    # initialize structures for top list
    df <- data.frame(Term = factor(), Freq = integer(), Doc = factor())
    top <- df

    for(i in 1:docs.num) {
        # Unigrams
        doc <- data.frame(Term = dimnames(ngram.m)$Terms, 
                          Freq = ngram.m[i, ], 
                          Doc = factor(docs.name[i], levels = docs.name))
        df <- rbind(df, doc[order(doc$Freq, decreasing = TRUE), ])
    
        top <- rbind(top, df[as.integer(df$Doc) == i, ][1:n, ])
    }
    # return result
    top
}

# get top for all n-grams
ngram.df.top.l <- lapply(dtms.sample.m.l, function(x) getTopTerms(x, 20))

# iterate over (unigram, bigram, trigram) separate dataframe by doc id
ngram.df.top.l.docs <- lapply(ngram.df.top.l, function(x) split(x, x$Doc))

# function that reorders levels according to the order of terms corresponding 
# to ordered frequencies
orderLevels <- function(ngram.docs.l) { 
    lapply(ngram.docs.l, function(y) {
            y$Term <- factor(y$Term, 
                             levels = unique(as.character(y$Term)))
            y
        })
}

# iterate over (unigrams, bigrams, trigrams) and apply orderLevels which iterates 
# over docs.
ngram.df.top.l.docs <- lapply(ngram.df.top.l.docs, orderLevels)

# calculate aggregated frequency
freq.l <- lapply(dtms.sample.m.l, colSums)

# manually remove non-frequent terms
freq.l <- lapply(freq.l, function(x, f) x[x > f], f = 2)

# sort by most frequently used words
freq.l <- lapply(freq.l, function(x) sort(x, decreasing = TRUE))
terms.l <- lapply(freq.l, names)

# dataframe for more convenient use in ggplot
freq.df.l <- list(unigram = data.frame(Term = factor(terms.l$unigram, 
                                                     levels = terms.l$unigram), 
                                       Freq = freq.l$unigram),
                  bigram = data.frame(Term = factor(terms.l$bigram, 
                                                    levels = terms.l$bigram), 
                                      Freq = freq.l$bigram),
                  trigram = data.frame(Term = factor(terms.l$trigram, 
                                                     levels = terms.l$trigram), 
                                       Freq = freq.l$trigram))

#### 3. How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?  
# reusing data calculated in withSparseFreq section
words.total <- sum(frequency)
words.agg <- 0
words.count <- 1

while(words.agg < 0.5 * words.total) {
    words.agg <- words.agg + frequency[words.count]
    words.count <- words.count + 1
}

words.count50 <- words.count

while(words.agg < 0.9 * words.total) {
    words.agg <- words.agg + frequency[words.count]
    words.count <- words.count + 1
}

words.count90 <- words.count

###
### Prediction

########### Util
getFirstThreeWords <- function(s) {
    stri_extract_first(s, regex = "^[a-z]* [a-z]* [a-z]*")
#    stri_extract_first(s, regex = "^[^ ]* [^ ]* [^ ]*")
}

getFirstTwoWords <- function(s) {
    stri_extract_first(s, regex = "^[a-z]* [a-z]*")
#    stri_extract_first(s, regex = "^[^ ]* [^ ]*")
}

getFirstWord <- function(s) {
    stri_extract_first(s, regex = "^[a-z]*")
#    stri_extract_first(s, regex = "^[^ ]*")
}

getLastWord <- function(s) {
    stri_extract_first(s, regex = "[a-z]*$")
#    stri_extract_first(s, regex = "[^ ]*$")
}

removeTwoFirstWords <- function(s) {
    stri_replace_first(s, "", regex = "^[a-z]* [a-z]* *")
}

removeFirstWord <- function(s) {
    stri_replace_first(s, "", regex = "^[a-z]* *")
}

###########
uni.markov.key <- terms.l$unigram
uni.markov.word <- terms.l$unigram

bi.markov.key <- getFirstWord(terms.l$bigram)
bi.markov.word <- getLastWord(terms.l$bigram)

tri.markov.key <- getFirstTwoWords(terms.l$trigram)
tri.markov.word <- getLastWord(terms.l$trigram)


freq.df.markov.l <- list(unigram = data.frame(Key = factor(uni.markov.key, 
                                                           levels = unique(uni.markov.key)), 
                                              Word = factor(uni.markov.word, 
                                                            levels = unique(uni.markov.word)), 
                                              Freq = freq.l$unigram),
                         bigram = data.frame(Key = factor(bi.markov.key, 
                                                          levels = unique(bi.markov.key)),
                                             Word = factor(bi.markov.word, 
                                                           levels = unique(bi.markov.word)), 
                                             Freq = freq.l$bigram),
                         trigram = data.frame(Key = factor(tri.markov.key, 
                                                            levels = unique(tri.markov.key)), 
                                              Word = factor(tri.markov.word, 
                                                           levels = unique(tri.markov.word)), 
                                              Freq = freq.l$trigram))


predictWord <- function(phrase, ngram.markov.l, n = 1) {
    trigram.id <- which(ngram.markov.l$trigram$Key == phrase)

    predicted.id <- 1:n #which.max(prob)
    
    if(length(trigram.id) > 0) {
        trigram.freq <- ngram.markov.l$trigram$Freq[trigram.id]
        
        bigram.id <- which(ngram.markov.l$bigram$Key == getFirstWord(phrase) &
                               ngram.markov.l$bigram$Word == getLastWord(phrase))
        # frequency is not used now
        bigram.freq <-  ngram.markov.l$bigram$Freq[bigram.id]
        
        # simple maximum likelihood probability, not used now
        prob <- trigram.freq/bigram.freq
        # since all structures are ordered, the best will be on the top
        predicted.word <- ngram.markov.l$trigram$Word[trigram.id[predicted.id]]
    } else {
        bigram.id <- which(ngram.markov.l$bigram$Key == getLastWord(phrase))

        if(length(bigram.id) > 0) {
            
            # frequency is not used now
            bigram.freq <- ngram.markov.l$bigram$Freq[bigram.id]
            # since all structures are ordered, the best will be on the top
            predicted.word <- ngram.markov.l$bigram$Word[bigram.id[predicted.id]]
        } else {
            unigram.id <- ngram.markov.l$unigram$Key[1:n]
            predicted.word <- ngram.markov.l$unigram$Key[unigram.id[predicted.id]]
        }
    }
    predicted.word
}

# predicted <- sapply(testset$pair, function(x) predictWord(x, ngram.markov.l, 1))

splitStringToSet <- function(s) {
    triples <- data.frame(pair = character(), word = character(), stringsAsFactors = FALSE)
    while(stri_count_words(s) > 2) {
        three <- getFirstThreeWords(s)
        two <- getFirstTwoWords(three)
        triple <- data.frame(pair = two, 
                             word = removeTwoFirstWords(three), stringsAsFactors = FALSE)
        triples <- rbind(triples, triple)
        
        s <- removeFirstWord(s)
    }
    triples    
}

###################
predictWord("going to", freq.df.markov.l, n = 1)

s <- docs.sample[[1]]$content[1]
devtest <- splitStringToSet(s)
predicted <- sapply(devtest$pair, function(x) predictWord(x, freq.df.markov.l, 1))

predicted 

table(devtest$word == predicted)
