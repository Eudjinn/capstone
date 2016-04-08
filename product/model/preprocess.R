library(qdap) # rm_stopwords
library(stringi)

blogs.path <- file.path("final","en_US","en_US.blogs.txt")
news.path <- file.path("final","en_US","en_US.news.txt")
twitter.path <- file.path("final","en_US","en_US.twitter.txt")
          
## Loading data
readData <- function() {
    cat("Reading files...\n")
    blogs <- readLines(blogs.path, encoding = "UTF-8", skipNul = TRUE)
    news <- readLines(news.path, encoding = "UTF-8", skipNul = TRUE)
#    twitter <- readLines(twitter.path, encoding = "UTF-8", skipNul = TRUE)
    
    all <- c(blogs, news) #, twitter)
#    all <- blogs
    all
}

## Sampling
getSample <- function(text, sample.p = 0.01) {
    cat("Sampling...\n")
    text.length <- length(text)
    sampletext <- text[sample(1:text.length, text.length * sample.p)]
    sampletext
}

## cleaning
cleanData <- function(textdata, ngrams) {
    cat("Cleaning...\n")
    textdata <- cleandoc(textdata, type = "document")
    cat("Removing stopwords to find short meaningless strings that can be deleted...\n")
    nostopwords <- rm_stopwords(textdata, separate = FALSE)
    # identify docs which are noisy and are not good for word prediction
    cat("Counting words in each element of documents vector...\n")
    wordscount <- stri_count_words(nostopwords)
    # remove docs with less than 3 meaningful words (without stopwords)
    textdata <- textdata[wordscount > ngrams]
    textdata
}