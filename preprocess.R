## Loading data
readData <- function() {
    blogs <- readLines("final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
#    news <- readLines("final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
#    twitter <- readLines("final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)
    
    #all <- c(blogs, news, twitter)
    all <- blogs
    all
}

## Sampling
getSample <- function(text, sample.p = 0.01) {
    text.length <- length(text)
    sampletext <- text[sample(1:text.length, text.length * sample.p)]
    sampletext
}

## cleaning
cleanData <- function(textdata) {
    textdata <- cleandoc(textdata)
    textdata
}

addTags <- function(textdata, ngrams = 4) {  
    textdata <- addtags(textdata, ngrams)
}