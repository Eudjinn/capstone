###############
library(data.table)

# REWRITE
predictTM <- function(model, phrase, n = 1, method = "SBO", alpha = 0.4, lambda = c(0.1, 0.15, 0.3, 0.45)) {
    require(data.table)
    ngrams <- model$ngrams
    # stupid protection from phrases with less then 4 words.
    # need to be rewritten
    dummy <- paste(rep("ss-ss", ngrams), collapse = " ")
    phrase <- tolower(phrase)
    phrase <- paste(dummy, phrase)
    phrase <- cleandoc(phrase, type = "string")
    
    #    s <- unlist(strsplit(phrase, split = " "))
    #    s.length <- length(s)
    
    #    substr <- character(ngrams - 1)
    #    substr <- sapply(1:(ngrams - 1), function(i) {
    #        substr <- s[(s.length - ngrams + i + 1)]
    #        substr
    #    })
    
    key <- sapply(1:(ngrams - 1), function(i) {
        key <- getLastNWords(phrase, i)
        key
    })

    # if not Stupid backoff, don't alpha coefficient should not affect results
    # interpolation has its own coefficients
    if(method != "SBO")
        alpha <- 1

    # ngrams - max ngram level, nwords.window - how many words to fetch
    getWords <- function(ngrams, n) {
        discount <- 1
        # create empty data.table
        selected <- data.table(Word = character(), Prob = numeric())
        for(ngram.i in ngrams:1) {
            if(ngram.i > 1) {
                found <- model$dts[[ngram.i]][key[ngram.i-1], .(Word, Prob)][order(-Prob)][1:n]
#                newwords <- as.character(model$dts[[ngram.i]][key[ngram.i-1]][order(-Prob)][1:n]$Word)
            } else if(ngram.i == 1)
                found <- model$dts[[ngram.i]][, .(Word, Prob)][order(-Prob)][1:n]
            # remove NA values from found
            found <- found[!is.na(found$Word), ]
            if(nrow(found) > 0) { # do something only if found something
                # if higer-level n-gram gave results, apply alpha to lower-level findings
                if(nrow(selected) > 0)
                    found$Prob <- alpha^(ngram.i-1) * found$Prob 
                # bind new results with results from higher n-grams
                l <- list(selected, found)
                selected <- rbindlist(l)
                # remove duplicated words with lower probability and order by Prob desc
                selected <- unique(selected[, Prob := max(Prob), by = Word][order(-Prob)])
            }
            if(nrow(selected) >= n)
                break
        } 
        selected
    }
    
    # run recursion over available n-grams, empty words list for the beginning
    predicted.Words <- getWords(ngrams, n)

    # in case interpolation is needed
    if(method == "I") {
        # non-recursive version
        getIProb <- function(word, ngrams) {
            probs <- numeric(ngrams)
            for(ngram.i in ngrams:1) {
                if(ngram.i > 1)
                    probs[ngram.i] <- as.numeric(model$dts[[ngram.i]][.(key[ngram.i-1],predicted.Words[i]$Word)]$Prob[1])
                else if(ngram.i == 1)
                    probs[ngram.i] <- as.numeric(model$dts[[ngram.i]][.(predicted.Words[i]$Word)]$Prob[1])
            }
            probs[is.na(probs)] <- 0
            probs
        }
        
        # iterate over number of selected words to get their probabilities 
        # in each n-gram model to calculate interpolation
        for(i in 1:nrow(predicted.Words)) {
            # run recursion over all n-gram models for each word
            probs <- getIProb(predicted.Words[i]$Word, ngrams)
            predicted.Words[i]$Prob <- sum(probs * lambda[1:ngrams])
        }
        # data table with results
        predicted.Words <- predicted.Words[order(-Prob)]
    }
    # cut n words from the top only at the end of selection
    predicted.Words[1:n]
}
