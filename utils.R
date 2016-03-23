########### Util
getFirstNWordsPattern <- function(n = 1) {
    pattern <- "^[^ ]*"
    if(n > 1) {
        for(i in seq(1, n - 1)) {
            pattern <- paste0(pattern, " [^ ]*")
        }
    }
    pattern
}

getLastNWordsPattern <- function(n = 1) {
    pattern <- "[^ ]*$"
    if(n > 1) {
        for(i in seq(1, n - 1)) {
            pattern <- paste0("[^ ]* ", pattern)
        }
    }
    pattern
}

getFirstNWords <- function(s, n = 1) {
    #    stri_extract_first(s, regex = "^[a-z]* [a-z]* [a-z]* [a-z]*")
    pattern <- "^[^ ]*"
    if(n == 2) 
        pattern <- "^[^ ]* [^ ]*"
    else if(n == 3) 
        pattern <- "^[^ ]* [^ ]* [^ ]*"
    else if(n == 4) 
        pattern <- "^[^ ]* [^ ]* [^ ]* [^ ]*"
    else if(n == 5) 
        pattern <- "^[^ ]* [^ ]* [^ ]* [^ ]* [^ ]*"
    else if(n > 5) {
        pattern <- getFirstNWordsPattern(n)
    }
    
    stri_extract_first(s, regex = pattern)
}

getLastNWords <- function(s, n = 1) {
    #    stri_extract_first(s, regex = "[a-z]*$")
    pattern <- "[^ ]*$"
    if(n == 2) 
        pattern <- "[^ ]* [^ ]*$"
    else if(n == 3) 
        pattern <- "[^ ]* [^ ]* [^ ]*$"
    else if(n == 4) 
        pattern <- "[^ ]* [^ ]* [^ ]* [^ ]*$"
    else if(n == 5) 
        pattern <- "[^ ]* [^ ]* [^ ]* [^ ]* [^ ]*$"
    else if(n > 5) {
        pattern <- getLastNWordsPattern(n)
    }

    stri_extract_first(s, regex = pattern)
}

removeFirstNWords <- function(s, n = 1) {
    #    stri_replace_first(s, "", regex = "^[a-z]* [a-z]* [a-z]* *")
    pattern <- "^[^ ]* *"
    if(n == 2) 
        pattern <- "^[^ ]* [^ ]* *"
    else if(n == 3) 
        pattern <- "^[^ ]* [^ ]* [^ ]* *"
    else if(n == 4) 
        pattern <- "^[^ ]* [^ ]* [^ ]* [^ ]* *"
    else if(n == 5) 
        pattern <- "^[^ ]* [^ ]* [^ ]* [^ ]* [^ ]* *"
    else if(n > 5) {
        pattern <- getFirstNWordsPattern(n)
        pattern <- paste0(pattern, " *")
    }

    stri_replace_first(s, "", regex = pattern)
}

# n = key length (ngram is key + word)
splitStringToSet <- function(s, n = 2) {
    triples <- list()
    triple <- triples
    len <- stri_count_words(s)
    while(len > n) {
        ngram <- getFirstNWords(s, n + 1)
        key <- getFirstNWords(ngram, n)
        triple$key <- c(triple$key, key)
        # get the word
        triple$word <- c(triple$word, getLastNWords(ngram, 1))
        # move the window by one word
        s <- removeFirstNWords(s, 1)
        len <- len - 1
    }
    triples <- c(triples, triple)
    triples
}

superpaste <- function(x) { 
    s <- character()
    for(i in 1:length(x)) {
        s <- paste(s, x[i])
    }
    s
}

cleandoc <- function(doc) {
    # fix '
    doc <- gsub("’", "'", doc)
    doc <- iconv(doc, "UTF-8", "ascii", sub = " ")
    #    doc <- gsub("[.?!]", "<EOS>", doc)
    doc <- gsub("[^[:alnum:]['-]", " ", doc)
    doc <- gsub(" - |- | -| {2,}", " ", doc)
    doc <- gsub("^ | $", "", doc)
    
    doc
}
