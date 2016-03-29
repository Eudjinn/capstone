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
    if(n > 0) {
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
    } else 
        s
}

getLastNWords <- function(s, n = 1) {
    #    stri_extract_first(s, regex = "[a-z]*$")
    if(n > 0) {
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
    } else
        s
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
splitStringToSet <- function(s, ngrams = 5) {
    triples <- list()
    if(ngrams > 1)
    {
        n <- ngrams - 1
        triple <- triples
        words <- unlist(strsplit(s, " "))
        pos <- 1
        len <- length(words)
        while((len - pos + 1) > n) {
            triple$key <- c(triple$key, paste(words[pos : (pos + n - 1)], collapse = " "))
            # get the word
            triple$word <- c(triple$word, words[(pos + n)])
            # move the window by one word
            pos <- pos + 1
        }
    triples <- c(triples, triple)
    } else {
        print("ngrams cannot be less than 2")
    }
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

    doc <- tolower(doc)

    # remove urls
    doc <- gsub("https?:\\/\\/(www)?\\.?[a-z0-9\\.\\-]+(\\/?[a-z0-9\\.#&_~\\-]+)+\\/?\\??[a-z0-9=\\.\\/\\+&#_~\\-]+", "", doc)
    
    doc <- gsub(" a\\.m\\. ", " am ", doc)
    doc <- gsub(" p\\.m\\. ", " pm ", doc)
    doc <- gsub(" u\\.s\\. ", " usa ", doc)
    doc <- gsub(" i\\.e\\. ", " ie ", doc)
    doc <- gsub(" e\\.g\\. ", " eg ", doc)
    doc <- gsub(" etc\\. ", " etc ", doc)
    doc <- gsub(" d\\.c\\. ", " dc ", doc)
    doc <- gsub(" dr\\. ", " dr ", doc)
    doc <- gsub(" mr\\. ", " mr ", doc)
    doc <- gsub(" mrs\\. ", " mrs ", doc)
    
    # remove all sorts of numeric values
    doc <- gsub("[$]?[+-]?[0-9]{1,}(?:[0-9]*(?:[.,][0-9]{1,})?|(?:,[0-9]{1,})*(?:\\.[0-9]{1,})?|(?:\\.[0-9]{1,})*(?:,[0-9]{1,})?)[+%]?", " ", doc)
    # remove numbers
    doc <- gsub("[0-9]+", " ", doc)

    # remove multiple dashes
    doc <- gsub("-{2,}", "", doc)
    # remove standalone dashes
    doc <- gsub(" - ", " ", doc)
    doc <- gsub(" -", " ", doc)
    doc <- gsub("- ", " ", doc)
    # remove hash
    doc <- gsub("#", "", doc)
    
    doc <- gsub("[\\(\\),:><\\+/]", " ", doc)
    doc <- gsub("[><\\+]", "", doc)

    # collapse double apostropes in one space
    doc <- gsub("[']{2,}", " ", doc)
    
#---------        
    # remove the dot in the beginning of the string
    doc <- gsub("^[?!.;] ?", "", doc) 
    
    # replace !.?; with ". ", treating multiple as one in the middle too.
    doc <- gsub("( ?[?!.;]+ ?)+", ". ", doc) 

#    doc <- gsub("^", "ss-ss ", doc)
#    doc <- gsub("$", " ee-ee", doc)
#---------
    
    # replace all the unusual chars with space except '-<>
    doc <- gsub("[^[:alnum:]['-<>?!]", " ", doc)

    # collapse spaces in one space
    doc <- gsub("[[:space:]]{2,}", " ", doc)
    # remove spaces in the beginning and at the end
    doc <- gsub("^ | $", "", doc)
    
    doc
}

addtags <- function(doc, ngrams = 5) {
    if(ngrams > 1)
    {
        keylen <- ngrams - 1
        #end of string !.? replace with ee-ee
        e <- paste(rep(" ee-ee", keylen), collapse = "")
        s <- paste(rep("ss-ss ", keylen), collapse = "")
        es <- paste(e,s, collapse = "")
        doc <- gsub("[?!.;]+$", e, doc)
        # replace !.? with ee-ee ss-ss, treating multiple as one in the middle too.
        doc <- gsub("[?.!;]+", es, doc)
        
        doc <- gsub("^", s, doc)
        doc <- gsub("$", e, doc)
    
        # collapse duplicated ee-ee which happens when end of sentence and end of line
        eepattern <- paste0(e, e, "$", collapse = "")
        doc <- gsub(eepattern, e, doc)
        
        # collapse spaces in one space
        doc <- gsub("[[:space:]]{2,}", " ", doc)
    }    
    doc
}

cleanEnds <- function(doc) {
    # replace !.? with " "
    doc <- gsub("[?.!;]+", " ", doc)
    
    # collapse spaces in one space
    doc <- gsub("[[:space:]]{2,}", " ", doc)
    # remove spaces in the beginning and at the end
    doc <- gsub("^ | $", "", doc)
    
    doc
}