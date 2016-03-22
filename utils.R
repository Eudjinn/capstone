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

superpaste <- function(x) { 
    s <- character()
    for(i in 1:length(x)) {
        s <- paste(s, x[i])
    }
    s
}