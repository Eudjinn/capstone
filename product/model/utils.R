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

asciifydoc <- function(doc) {
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
    
    doc <- gsub("κ|ύ|ρ|ι|ο|ς", "", doc)
    doc <- gsub("ṇ", "n", doc)
    doc <- gsub("ḍ", "d", doc)
    doc <- gsub("ṁ", "m", doc)
    doc <- gsub("ṣ", "s", doc)
    doc <- gsub("ṭ", "t", doc)
    doc <- gsub("ṛ", "r", doc)
    doc <- gsub("ῦ", "v", doc)
    doc <- gsub("ἐ", "e", doc)
    doc <- gsub("ḥ", "h", doc)
    doc <- gsub("ﬀ", "ff", doc)
    doc <- gsub("ﬁ", "fi", doc)
    doc <- gsub("ᾶ", "a", doc)
    doc <- gsub("ῆ", "n", doc)
    doc <- gsub("ἴ", "i", doc)
    doc <- gsub("ὐ", "v", doc)
    doc <- gsub("ὶ", "i", doc)
    doc <- gsub("ὸ", "o", doc)
    doc <- gsub("ῃ", "n", doc)

    doc <- gsub("·", ".", doc) # ·
    
    doc <- gsub("ü", "u", doc) # ü
    doc <- gsub("é", "e", doc) # é
    doc <- gsub("ø", "o", doc) # ø
    doc <- gsub("Ø", "o", doc) # Ø
    doc <- gsub("é", "e", doc) # é
    doc <- gsub("é", "e", doc) # é
    doc <- gsub("ä", "a", doc) # ä
    doc <- gsub("ô", "o", doc) # ô
    doc <- gsub("ā", "a", doc) # ā
    doc <- gsub("ñ", "n", doc) # ñ
    doc <- gsub("à", "a", doc) # \\u00e0
    doc <- gsub("ő", "o", doc) # \\u0151
    doc <- gsub("á", "a", doc) # \\u00e1
    doc <- gsub("ö", "o", doc) # \\u00f6
    doc <- gsub("Ô", "o", doc) # \\u00d4
    doc <- gsub("â", "a", doc) # \\u00e2
    doc <- gsub("Ä", "a", doc) # \\u00c4
    doc <- gsub("ł", "l", doc) # \\u0142
    doc <- gsub("ğ", "g", doc) # \\u011f
    doc <- gsub("ş", "s", doc) # \\u015f
    
    # \\u0091
    # \\u0093
    # \\u0094
    # \\u0096

    
    ##################
#    doc <- ASCIIfy(doc)
    
#    doc <- gsub("\\u00b4", "'", doc) # ´
#    doc <- gsub("\\u00bb", ">", doc) # »
#    doc <- gsub("\\u00ab", "<", doc) # «
#    doc <- gsub("\\u00b0", ".", doc) # ° # degree
    
#    doc <- gsub("\\u00b7", ".", doc) # ·
    
#    doc <- gsub("\\u00fc", "u", doc) # ü
#    doc <- gsub("\\u00e9", "e", doc) # é
#    doc <- gsub("\\u00f8", "o", doc) # ø
#    doc <- gsub("\\u00d8", "o", doc) # Ø
#    doc <- gsub("\\u00e9", "e", doc) # é
#    doc <- gsub("\\u00e8", "e", doc) # é
#    doc <- gsub("\\u00a3", "$", doc) # £
#    doc <- gsub("\\u00e4", "a", doc) # ä
#    doc <- gsub("\\u00f4", "o", doc) # ô
#    doc <- gsub("\\u0101", "a", doc) # ā
#    doc <- gsub("\\u00f1", "n", doc) # ñ
    
    #   325° F
    #   170° C
    # get rid of the rest of unknown symbols
    doc <- iconv(doc, "UTF-8", "ascii", sub = "")
    
}

cleandoc <- function(doc) {
    doc <- asciifydoc(doc)
    
    doc <- tolower(doc)

    # replace urls
    doc <- gsub("https?:\\/\\/(www)?\\.?[a-z0-9\\.\\-]+(\\/?[a-z0-9\\.=\\+#&_~\\-]+)+\\/?\\??[a-z0-9=\\.\\/\\+&#_~\\-]+", " ww-ww ", doc)
    doc <- gsub("www\\.[a-z0-9\\.\\-]+(\\/?[a-z0-9\\.=\\+#&_~\\-]+)+\\/?\\??[a-z0-9=\\.\\/\\+&#_~\\-]+", " ww-ww ", doc)
    
    # replace time
    doc <- gsub("[0-9]+:?([0-9]+)? ?(a\\.?m\\.?|p\\.?m\\.?)", " tm-tm ", doc)
    # replace money
    doc <- gsub("[$][0-9]*[\\.,]?[0-9]+[k|m]?", " mm-mm ", doc)
    # replace ordinals
    doc <- gsub("[0-9]+(rd|th)", " oo-oo ", doc)
    # replace percent
    doc <- gsub("[0-9]+[\\.,]?[0-9]+%", " pp-pp ", doc)
    # replace some year's
    doc <- gsub("[0-9]+'?s", " ys-ys ", doc)
    
    doc <- gsub(" u\\.s\\.", " us-us ", doc)
    doc <- gsub(" i\\.e\\.", " ie-ie ", doc)
    doc <- gsub(" e\\.g\\.", " eg-eg ", doc)
    doc <- gsub(" a\\.d\\.", " ad-ad ", doc)
    doc <- gsub(" dr\\. ", " dr-dr ", doc)
    doc <- gsub(" mr\\. ", " mr-mr ", doc)
    doc <- gsub(" mrs\\. ", " mrs-mrs ", doc)
    doc <- gsub(" d\\.c\\.", " dc-dc", doc)
    
    # remove all standalone groups with numbers
    doc <- gsub("[ \\.#,!\\+\\*\\-^&]\\(?[\\+\\*\\-]?([0-9]{1,}[, &=/\\+:\\.\\*\\-]*)+\\)?[ \\.,!#\\+\\*\\-^&]?", " nn-nn ", doc)
    # remove 
#    doc <- gsub("[$]?[+-]?[0-9]{1,}(?:[0-9]*(?:[.,][0-9]{1,})?|(?:,[0-9]{1,})*(?:\\.[0-9]{1,})?|(?:\\.[0-9]{1,})*(?:,[0-9]{1,})?)[+%]?", " nn-nn ", doc)
    # remove numbers
    doc <- gsub("[0-9]+", "", doc)

    # remove standalone letters unless they are real words like 'a'
    
    # remove multiple dashes
    doc <- gsub("-{2,}", "", doc)
    # remove standalone dashes
    doc <- gsub(" - ", " ", doc)
    doc <- gsub(" -", " ", doc)
    doc <- gsub("- ", " ", doc)
    # remove hash
    doc <- gsub("#", "", doc)
    # remove star
    doc <- gsub("\\*", "", doc)
    
    doc <- gsub("[\\(\\),:><\\+/]", " ", doc)
    doc <- gsub("[><\\+]", "", doc)

    # collapse double apostropes in one space
    doc <- gsub("[']{2,}", " ", doc)
    
#---------        
    # remove the dot in the beginning of the string
    doc <- gsub("^[?!\\.;]+ ?", "", doc) 
    
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

addtags <- function(doc, ngrams = 4) {
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