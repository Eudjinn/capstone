###############

# REWRITE
predictTM <- function(model, phrase, n = 1, ngrams = 4, interpolate = FALSE, l = c(0.1, 0.15, 0.3, 0.45)) {
    # stupid protection from phrases with less then 4 words.
    # need to be rewritten
    dummy <- "<notaword> <notaword> <notaword> <notaword>"
    phrase <- tolower(phrase)
    phrase <- paste(dummy, phrase)
    
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

    # ngrams - max ngram level, nwords.window - how many words to fetch
    getWords <- function(ngrams, n) {
        words <- character()
        for(ngram.i in ngrams:1) {
            if(ngram.i > 1) {
                newwords <- as.character(model$dts[[ngram.i]][key[ngram.i-1]][order(-Prob)][1:n]$Word)
                words <- unique(c(words, newwords)) # remove words already suggested
            } else if(ngram.i == 1)
                words <- c(words,
                           as.character(model$dts[[ngram.i]][order(-Prob)][1:n]$Word))
            # clear vector of potential predictions from NAs
            # it is ordered by probabilities already during backoff but with higher-level n-gram priority
            # so there is a chance that probability of first words in this vector have lower probability
            # for stupid backoff this is the answer already.
            words <- words[!is.na(words)]
            if(length(words) >= n)
                break
        } 
        # perhaps explicit conversion to character is not needed
        as.character(words)
    }
    
    # run recursion over available n-grams, empty words list for the beginning
    predicted.Words <- getWords(ngrams, n * 2)

    # in case interpolation is needed
    if(interpolate) {
        # create vector to store interpolated probabilities for each Word
        predicted.IProbs <- numeric(length(predicted.Words))
        # create vector for probabilities of a Word in each n-gram model
        
        # non-recursive version
        getIProbs <- function(word, ngrams) {
            probs <- numeric(ngrams)
            for(ngram.i in ngrams:1) {
                if(ngram.i > 1)
                    probs[ngram.i] <- as.numeric(model$dts[[ngram.i]][.(key[ngram.i-1],predicted.Words[i])]$Prob[1])
                else if(ngram.i == 1)
                    probs[ngram.i] <- as.numeric(model$dts[[ngram.i]][.(predicted.Words[i])]$Prob[1])
            }
            probs[is.na(probs)] <- 0
            probs
        }
        
        # iterate over number of selected words to get their probabilities 
        # in each n-gram model to calculate interpolation
        for(i in 1:length(predicted.Words)) {
            # run recursion over all n-gram models for each word
            probs <- getIProbs(predicted.Words[i], ngrams)
            predicted.IProbs[i] <- sum(probs * l)
        }
        # data frame with results
        predicted <- data.frame(Word = predicted.Words, 
                                iprob = predicted.IProbs, 
                                stringsAsFactors = FALSE)
        predicted.Words <- predicted[order(predicted$iprob, decreasing = TRUE), ]$Word
    }
    # cut n words from the top only at the end of selection
    predicted.Words[1:n]
}

# WILL NOT WORK WITH 4-GRAM
probTM <- function(model, phrase, word) {
    # stupid protection from phrases with less then 4 words.
    # need to be rewritten
    dummy <- "<notaword> <notaword> <notaword> <notaword>"
    phrase <- paste(dummy, phrase)
    phrase.four <- getLastNWords(phrase, 4)
    phrase.three <- getLastNWords(phrase, 3)
    phrase.two <- getLastNWords(phrase, 2)
    phrase.one <- getLastNWords(phrase, 1)

    match <- model$dts[[5]][phrase.four][Word == word]
    if(length(match$Word) == 0) {
        match <- model$dts[[4]][phrase.three][Word == word]
        if(length(match$Word) == 0) {
            match <- model$dts[[3]][phrase.two][Word == word]
            if(length(match$Word) == 0) {
                match <- model$dts[[2]][phrase.one][Word == word]
                if(length(match$Word) == 0){
                    match <- data.table(Key = NA, Word = NA, Freq = 0, Prob = 0, FreqSmooth = 0)
                }
            }
        }
    }
    cbind(phrase.four, match)
}
