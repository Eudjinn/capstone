# capstone
Evgeniy Zabrodskiy  
8 March 2016  

## Initialize


```r
library(tm)
```

```
## Loading required package: NLP
```

```r
library(RWeka)
library(wordcloud)
```

```
## Loading required package: RColorBrewer
```

```r
library(ggplot2)
```

```
## 
## Attaching package: 'ggplot2'
```

```
## The following object is masked from 'package:NLP':
## 
##     annotate
```

### Loading data

```r
# this is needed for tokenizer in DocumentTermMatrix to work. For some reason does not with parallel processing.
# Found solution here: http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka
options(mc.cores=1)

ds <- DirSource(directory = "final/en_US/")
docs <- Corpus(ds, readerControl = list(reader = readPlain, language = "en", load = TRUE))
summary(docs)
```

```
##                   Length Class             Mode
## en_US.blogs.txt   2      PlainTextDocument list
## en_US.news.txt    2      PlainTextDocument list
## en_US.twitter.txt 2      PlainTextDocument list
```

### Sampling and cleaning data

```r
delim <- 1000
# copy original doc for processing (don't know how to make it more efficient yet)
docs.sample <- docs
# select random sample of size/delim of the original
# iterate through documents in the corpus
for (i in 1:length(docs)) {
    # get length of each document (number of lines)
    l <- length(docs[[i]]$content)
    # get random sample of lines in the document and replace the original doc in the corpus with the smaller sample.
    docs.sample[[i]]$content <- docs[[i]]$content[sample(1:l, as.integer(l/delim))]
}
```


```r
# remove punctuation
docs.sample <- tm_map(docs.sample, removePunctuation)   

# remove numbers
docs.sample <- tm_map(docs.sample, removeNumbers)

# remove some special characters
for(j in seq(docs.sample)) {
    docs.sample[[j]] <- gsub("/", " ", docs.sample[[j]])   
    docs.sample[[j]] <- gsub("@", " ", docs.sample[[j]])   
    docs.sample[[j]] <- gsub("\\|", " ", docs.sample[[j]])
    }   

# remove stopwords
# docs.sample <- tm_map(docs.sample, removeWords, stopwords("english"))

# remove unnecessary whitespaces
docs.sample <- tm_map(docs.sample, stripWhitespace)

# finalize preprocessing
docs.sample <- tm_map(docs.sample, PlainTextDocument)
```


```r
##  create document term matrix
# Singles
dtm.sample <- DocumentTermMatrix(docs.sample)
# remove sparse terms
dtm.sample <- removeSparseTerms(dtm.sample, 0.1)

# Doubles
bigramTokenizer <- function(x) NGramTokenizer(x, 
                                Weka_control(min = 2, max = 2))

dtm.sample.bigram <- DocumentTermMatrix(docs.sample, control = list(tokenize = bigramTokenizer))

# remove sparse terms
dtm.sample.bigram <- removeSparseTerms(dtm.sample.bigram, 0.1)

# Triples
trigramTokenizer <- function(x) NGramTokenizer(x, 
                                Weka_control(min = 3, max = 3))

dtm.sample.trigram <- DocumentTermMatrix(docs.sample, control = list(tokenize = trigramTokenizer))

# remove sparse terms
dtm.sample.trigram <- removeSparseTerms(dtm.sample.trigram, 0.1)
```

## Exploratory analysis

1. Some words are more frequent than others - what are the distributions of word frequencies?


```r
## Single words
# create matrix
dtm.sample.m <- as.matrix(dtm.sample)

# calculate frequency
frequency1 <- colSums(dtm.sample.m)

# histogram of log of word frequencies
# qplot(log(frequency), geom = "histogram", binwidth = 0.5)

# sort by most frequently used words
frequency1 <- sort(frequency1, decreasing = TRUE)
words1 <- names(frequency1)

wordcloud(words = words1[1:100], 
          freq = frequency1[1:100], 
          random.order = FALSE, 
          scale = c(8, 1),
          min.freq = 1,
          rot.per = 0.35, 
          use.r.layout = FALSE,
          colors = brewer.pal(8, "Dark2"))
```

![](capstone_ms_files/figure-html/displayFreq1-1.png)

```r
frequency1.df <- data.frame(w = factor(words1, 
                                      levels = words1), 
                           f = frequency1)

ggplot(data = frequency1.df[1:20, ], 
       aes(w, f, fill = f),
       xlab = "Terms",
       ylab = "Frequency",
       main = "Most frequent terms") + 
    geom_bar(stat = "identity") + 
    theme(axis.text.x=element_text(angle = 60, hjust = 1))
```

![](capstone_ms_files/figure-html/displayFreq1-2.png)

2. What are the frequencies of 2-grams and 3-grams in the dataset?


```r
## Bigrams

dtm.sample.bigram.m <- as.matrix(dtm.sample.bigram)

# calculate frequency
frequency2 <- colSums(dtm.sample.bigram.m)

# sort by most frequently used words
frequency2 <- sort(frequency2, decreasing = TRUE)
words2 <- names(frequency2)

wordcloud(words = words2[1:100], 
          freq = frequency2[1:100], 
          random.order = FALSE, 
          scale = c(5, 0.5),
          min.freq = 1,
          rot.per = 0.35, 
          use.r.layout = FALSE, 
          colors = brewer.pal(8, "Dark2"))
```

![](capstone_ms_files/figure-html/diplayFreq2-1.png)

```r
frequency2.df <- data.frame(w = factor(words2, 
                                      levels = words2), 
                           f = frequency2)

ggplot(data = frequency2.df[1:20, ], 
       aes(w, f, fill = f),
       xlab = "Terms",
       ylab = "Frequency",
       main = "Most frequent terms") + 
    geom_bar(stat = "identity") + 
    theme(axis.text.x=element_text(angle = 60, hjust = 1))
```

![](capstone_ms_files/figure-html/diplayFreq2-2.png)


```r
## Trigrams

dtm.sample.trigram.m <- as.matrix(dtm.sample.trigram)

# calculate frequency
frequency3 <- colSums(dtm.sample.trigram.m)

# sort by most frequently used words
frequency3 <- sort(frequency3, decreasing = TRUE)
words3 <- names(frequency3)

wordcloud(words = words3[1:100], 
          freq = frequency3[1:100], 
          random.order = FALSE, 
          scale = c(3, 0.2),
          min.freq = 1,
          rot.per = 0.35, 
          use.r.layout = FALSE, 
          colors = brewer.pal(8, "Dark2"))
```

![](capstone_ms_files/figure-html/diplayFreq3-1.png)

```r
frequency3.df <- data.frame(w = factor(words3, 
                                      levels = words3), 
                           f = frequency3)

ggplot(data = frequency3.df[1:20, ], 
       aes(w, f, fill = f),
       xlab = "Terms",
       ylab = "Frequency",
       main = "Most frequent terms") + 
    geom_bar(stat = "identity") + 
    theme(axis.text.x=element_text(angle = 60, hjust = 1))
```

![](capstone_ms_files/figure-html/diplayFreq3-2.png)

3. How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
4. How do you evaluate how many of the words come from foreign languages?
5. Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?

## Appendix


```r
#> order(dtms.sample$v, decreasing = TRUE)[1:10]
# [1] 3192 3433 1605 4506 3597 4324 1112 4570  850 4608
#> dtms.sample$v[order(dtms.sample$v, decreasing = TRUE)[1:10]]
# [1] 261 257 175 157 134 125 124 123 108 107
#> dtms.sample$i[order(dtms.sample$v, decreasing = TRUE)[1:10]]
# [1] 2 2 1 3 2 3 1 3 1 3
#> dtms.sample$j[order(dtms.sample$v, decreasing = TRUE)[1:10]]
# [1] 1364 1605 1605  850 1769  668 1112  914  850  952
#> dtms.sample$dimnames$Terms[dtms.sample$j[order(dtms.sample$v, decreasing = TRUE)[1:10]]]
# [1] "said" "the"  "the"  "just" "will" "get"  "one"  "like" "just" "love"
```

https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html

https://deltadna.com/blog/text-mining-in-r-for-term-frequency/


```r
#> dtms.df <- data.frame(i = dtms.sample$i, j = dtms.sample$j, v = ddttms.sample$v)
#> qplot(data = dtms.df, as.factor(i), log(v), geom = c("boxplot", "jitter"), color = as.factor(i))

#> # show barplot of top 10 words frequencies in the Corpus. Find correct way of plotting it with ggplot2.
#> barplot(dtms.top10$v, col = as.factor(dtms.top10$i))
```


```r
# remove punctuation
# docs <- tm_map(docs, removePunctuation)   

# remove some special characters
# for(j in seq(docs)) {   
#     docs[[j]] <- gsub("/", " ", docs[[j]])   
#     docs[[j]] <- gsub("@", " ", docs[[j]])   
#     docs[[j]] <- gsub("\\|", " ", docs[[j]])   
#  }   

# remove numbers
# docs <- tm_map(docs, removeNumbers)

# to lowercase
# docs <- tm_map(docs, tolower)  

# remove stopwords
# For a list of the stopwords, see:   
# length(stopwords("english"))   
# stopwords("english")   
# docs <- tm_map(docs, removeWords, stopwords("english"))

# remove particular words
# docs <- tm_map(docs, removeWords, c("department", "email"))   

# remove word endings
# library(SnowballC)   
# docs <- tm_map(docs, stemDocument)   

# strip unnecessary whitespaces
# docs <- tm_map(docs, stripWhitespace)

# finalize preprocessing
# docs <- tm_map(docs, PlainTextDocument)  

# Create document term matrix
# dtm <- DocumentTermMatrix(docs)
```
