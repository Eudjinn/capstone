library(stringi)

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
splitStringToSet <- function(s, ngrams = 4) {
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

# if type == "document" - heavy cleaning.
# if type == "string" - only prepare for prediction
asciify <- function(doc, type = "string") {
    if(type == "document")
        cat("Replacing non-ASCII quotes, dashes, etc. with ASCII symbols...\n")
    doc <- gsub("’|‘|′|´", "'", doc)
    doc <- gsub("\\u0092", "'", doc)  # apostrophe
    doc <- gsub("“|”|″", "\"", doc)
    doc <- gsub("…", "...", doc)
    doc <- gsub("–|—|ー|‑|−|⁃|一|―", "-", doc)
    doc <- gsub("～", "~", doc)
    doc <- gsub("€|£", "$", doc)
    doc <- gsub("：", ":", doc)
    doc <- gsub("、|，", ",", doc)
    doc <- gsub("！", "!", doc)
    doc <- gsub("\\u0095", "", doc) # don't know what it is
    
    # remove all strings containing non-ascii symbols from document
    if(type == "document") {
        cat("Removing strings containing other non-ASCII symbols...\n")
        remove <- grep("I_WAS_NOT_ASCII", iconv(doc, "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))
        if(length(remove) > 0)
            doc <- doc[-remove]
    }
    doc
    # get rid of the rest of unknown symbols
#    doc <- iconv(doc, "UTF-8", "ascii", sub = "")
}

# if type == "document" - heavy cleaning.
# if type == "string" - only prepare for prediction
cleandoc <- function(doc, type = "string") {
    doc <- asciify(doc, type)
    if(type == "document")
        cat("Lowercasing...\n")
    doc <- tolower(doc)

    # replace urls
    if(type == "document")
        cat("Replacing URLS...\n")
    doc <- gsub("https?:\\/\\/(www)?\\.?[a-z0-9\\.\\-]+(\\/?[a-z0-9\\.=\\+#&_~\\-]+)+\\/?\\??[a-z0-9=\\.\\/\\+&#_~\\-]+", " ww-ww ", doc)
    doc <- gsub("www\\.[a-z0-9\\.\\-]+(\\/?[a-z0-9\\.=\\+#&_~\\-]+)+\\/?\\??[a-z0-9=\\.\\/\\+&#_~\\-]+", " ww-ww ", doc)
    
    # replace time
    if(type == "document")
        cat("Replacing recognized time...\n")
    doc <- gsub("[0-9]+:?([0-9]+)? ?(a\\.?m\\.?|p\\.?m\\.?)", " tm-tm ", doc)
    # replace money
    if(type == "document")
        cat("Replacing recognized money...\n")
    doc <- gsub("[$][0-9]*[\\.,]?[0-9]+[k|m]?", " mm-mm ", doc)
    # replace ordinals
    if(type == "document")
        cat("Replacing recognized ordinals...\n")
    doc <- gsub("[0-9]+(rd|th)", " oo-oo ", doc)
    # replace percent
    if(type == "document")
        cat("Replacing recognized percentages...\n")
    doc <- gsub("[0-9]+[\\.,]?[0-9]+%", " pp-pp ", doc)
    # replace some year's
    if(type == "document")
        cat("Replacing recognized years...\n")
    doc <- gsub("[0-9]+'?s", " ys-ys ", doc)
    
    if(type == "document")
        cat("Replacing common short abbreviations...\n")
    doc <- gsub(" u\\.s\\.", " us-us ", doc)
    doc <- gsub(" i\\.e\\.", " ie-ie ", doc)
    doc <- gsub(" e\\.g\\.", " eg-eg ", doc)
    doc <- gsub(" a\\.d\\.", " ad-ad ", doc)
    doc <- gsub(" dr\\. ", " dr-dr ", doc)
    doc <- gsub(" mr\\. ", " mr-mr ", doc)
    doc <- gsub(" mrs\\. ", " mrs-mrs ", doc)
    doc <- gsub(" d\\.c\\.", " dc-dc", doc)
    
    # remove all strange words adjacent to numbers except the ones from above
    if(type == "document")
        cat("Replacing groups of words with adjacent numbers...\n")
    doc <- gsub("([0-9]+[a-z]+[0-9]+)|([a-z]+[0-9]+[a-z]+)|([0-9]+[a-z]+)|([a-z]+[0-9]+)", " nx-nx ", doc)

    # remove all groups with numbers
    if(type == "document")
        cat("Replacing groups of unrecognized numbers...\n")
    doc <- gsub("[ \\.#,!\\+\\*\\-^&]\\(?[\\+\\*\\-]?([0-9]{1,}[, &=/\\+:\\.\\*\\-]*)+\\)?[ \\.,!#\\+\\*\\-^&]?", " nn-nn ", doc)
    # remove numbers if they are left for some strange reason
    doc <- gsub("[0-9]+", "", doc)

    if(type == "document")
        cat("Removing and fixing some other characters...\n")
    doc <- gsub(" [a-z]'s ", " sx-sx ", doc)
    # remove multiple dashes
    doc <- gsub("-{2,}", "", doc)
    # remove standalone dashes, quotes and other stuff
    doc <- gsub(" [-'] ", " ", doc)
    doc <- gsub(" [-']", " ", doc)
    doc <- gsub("[-'] ", " ", doc)
    # remove hash
    doc <- gsub("#", "", doc)
    # remove star
    doc <- gsub("\\*", "", doc)
    
    doc <- gsub("[\\(\\):/]", " ", doc)
    doc <- gsub("[><\\+]", "", doc)

    # collapse double apostrophes in one space
    doc <- gsub("[']{2,}", " ", doc)
    
#---------        
    # remove the dot and other stuff in the beginning of the string
    doc <- gsub("^[?!\\.;,:'\"]+ ?", "", doc) 
    
    # replace !.?; with ". ", treating multiple as one in the middle too.
    doc <- gsub("( ?[?!\\.;,:]+ ?)+", ". ", doc) 

#    doc <- gsub("^", "ss-ss ", doc)
#    doc <- gsub("$", " ee-ee", doc)
#---------

    # this is needed only when cleaning documents for model training
    if(type == "document") {
        #EXPERIMENTAL - divide strings by punctuation
        cat("Dividing strings into substrings at punctuation points...\n")
        doc <- unlist(strsplit(doc, "\\."))
    } else if(type == "string") { # no need to have punctuation for prediction
        doc <- gsub("( ?[?!\\.;,:]+ ?)+", ". ", doc) 
    }

    # replace all the unusual chars with space except '-
    doc <- gsub("[^[:alnum:]['-]", " ", doc)
    
    if(type == "document")
        cat("Collapsing redundant spaces and empty lines after cleaning...\n")
    # collapse spaces in one space
    doc <- gsub("[[:space:]]{2,}", " ", doc)
    # remove spaces in the beginning and at the end
    doc <- gsub("^ | $", "", doc)
    
    # get rid of empty strings
    doc <- doc[nchar(doc) > 0]
    
    doc
}

addtags <- function(doc, ngrams = 4) {
    if(ngrams > 1)
    {
        keylen <- ngrams - 1
        e <- paste(rep(" ee-ee", keylen), collapse = "")
        s <- paste(rep("ss-ss ", keylen), collapse = "")
        doc <- gsub("^", s, doc)
        doc <- gsub("$", e, doc)
    
        # collapse spaces in one space
        doc <- gsub("[[:space:]]{2,}", " ", doc)
    }    
    doc
}