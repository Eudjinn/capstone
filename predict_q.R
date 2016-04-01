###############
## Loading data
readData <- function() {
    blogs <- readLines("final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
#    news <- readLines("final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
#    twitter <- readLines("final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)
    
#    all <- c(blogs, news, twitter)
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

makeDTMs <- function(t = NULL, ngrams = 4, trimFeatures = FALSE, minCount = 3, minDoc = 2) {
    cat("MAKE DTMs, Trim Features:", trimFeatures, "\n")
    require(quanteda)

    # dtms - document term matrix - several
    dtms <- lapply(1:ngrams, function(i) {
        # add tags for tokenizing
        t.tagged <- addTags(t, ngrams = i)
        dtm <- dfm(t.tagged, ngrams = i, concatenator = " ")
        if(trimFeatures) {
            # trim rare terms
            dtm <- trim(dtm, minCount = minCount, minDoc = minDoc)
        }
        dtm
    })
    dtms
}

# used for testing as trimming is done within makeDTM
trimDTMs <- function(dtms = NULL, ngrams = 4, trimFeatures = FALSE, minCount = 3, minDoc = 2) {
    cat("TRIM dtms, Trim Features: ", trimFeatures, "\n")
    require(quanteda)
    
    # dtms - document term matrix - several
    dtms <- lapply(1:ngrams, function(i) {
        if(trimFeatures) {
            # trim rare terms
            dtm <- trim(dtms[[i]], minCount = minCount, minDoc = minDoc)
        } else {
            dtm <- dtms[[i]]
        }
        dtm
    })
    dtms
}

makeDTs <- function(dtms = NULL, ngrams = 4) {
    cat("MAKE DTs...\n")
    # dts = data table several 
    dts <- lapply(1:ngrams, function(i) {
        dt <- data.table(Key = getFirstNWords(features(dtms[[i]]), i - 1), 
                         Word = getLastNWords(features(dtms[[i]]), 1),
                         # Freq = docfreq(dtm.l$onegram, 
                         # scheme = "count")),
                         Freq = colSums(dtms[[i]]))
    })
    dts
}

smoothDTs <- function(dts = NULL, ngrams = 4, smoothingType = "none", smoothK = 1) {
    cat("SMOOTH DTs, Smoothing Type:", smoothingType, ", k =", smoothK, "\n")
    # Add-k smoothing
    smooth.n <- function(dt, k = 1, ngram = 1, V = 1) {
        N <- sum(dt$Freq)
        V <- nrow(dt)
        if(ngram == 1) {
            dt[, SumFreq := sum(Freq)]
            dt[, Prob := (Freq + k)/(N + k*V)]
            dt[, FreqSmooth := Prob * N]
        } else {
            setkey(dt, Key)
            dt[, SumFreq := sum(Freq), by = Key]
            dt[, Prob := (Freq + k)/(SumFreq + k*V)]
            dt[, FreqSmooth := Prob * SumFreq]
        }
        dt
        setkey(dt, Key, Word)
        dt
    }
    
    # EXPERIMENTAL Good-Turing Smoothing.
    smooth.n.gt <- function(dt, k = 1, ngram = 1, V = 1) {
        # approximation is needed for frequencies that are not present
        require(randomForest)
        gt <- data.frame(Freq = as.integer(names(table(dt$Freq))), 
                         FOfF = as.integer(table(dt$Freq)))
        gt.fit <- randomForest(FOfF ~ Freq, data = gt)
        dt$FofF <- predict(gt.fit, data.frame(Freq = dt$Freq))
        dt$FofF1 <- predict(gt.fit, data.frame(Freq = (dt$Freq + k)))
        
        N <- sum(dt$Freq)
        V <- nrow(dt)
        if(ngram == 1) {
            dt[, FreqSmooth := (Freq + k) * FofF1/FofF]
            dt[, SumFreqSmooth := sum(FreqSmooth)] # not needed actually, added only for consistency to match columns in ngram > 1
            dt[, Prob := FreqSmooth/(N + k*V)]
        } else {
            dt[, FreqSmooth := (Freq + k) * FofF1/FofF]
            setkey(dt, Key)
            dt[, SumFreqSmooth := sum(FreqSmooth), by = Key]
            dt[, Prob := FreqSmooth/(SumFreqSmooth + k*V)]
        }
        setkey(dt, Key, Word)
        dt
    }
    # Add smoothing
    # Vocabulary size is the length of onegram dt, which is the first in the list
    voc.size <- nrow(dts[[1]])

    dts <- lapply(1:ngrams, function(i) {
        if(smoothingType == "Ak")
            dt <- smooth.n(dts[[i]], k = smoothK, V = voc.size)
        else if(smoothingType == "GT")
            dt <- smooth.n.gt(dts[[i]], k = smoothK, V = voc.size)
        else {
            # if no smoothing selected - calculate probability with k = 0.
            dt <- smooth.n(dts[[i]], k = 0)
        }
        dt
    })
    dts
}

cleanDTs <- function(dts = NULL, ngrams = 4) {
    dts <- lapply(1:ngrams, function(i) {
        # EXPERIMENTAL: Remove starts and ends from tables:
        tagskey <- grep("ss-ss|ee-ee", dts[[i]]$Key)
        if(length(tagskey) > 0)
            dts[[i]] <- dts[[i]][-tagskey]
        
        tagsword <- grep("ss-ss|ee-ee", dts[[i]]$Word)
        if(length(tagsword) > 0)
            dts[[i]] <- dts[[i]][-tagsword]
        dts[[i]]
    })
    dts
}


# Full training from beginning to end - when all the parameters needed are known.
# pass vector of character documents and get the model as a result.
# Smoothing: Ak, GT: Ak - add k, GT - Good-Turing
trainTM <- function(t = NULL, 
                    trimFeatures = FALSE, 
                    minCount = 3, 
                    minDoc = 2, 
                    smoothingType = "none", 
                    smoothK = 1, 
                    ngrams = 4,
                    ...) {
    cat("TRAIN TM...", "Trim features:", trimFeatures, "Smoothing:", smoothingType, "\n")
    require(quanteda)
    
    # dtms - document term matrix - several. trimming inside.
    dtms <- makeDTMs(t = t, 
                     ngrams = ngrams, 
                     trimFeatures = trimFeatures, 
                     minCount = minCount, 
                     minDoc = minDoc)

    # make dts from dtms
    dts <- makeDTs(dtms = dtms, 
                   ngrams = ngrams)
        
    # apply smoothing
    dts <- smoothDTs(dts, 
                     smoothingType = smoothingType, 
                     smoothK = smoothK, 
                     ngrams = ngrams)
    
    dts <- cleanDTs(dts,
                    ngrams = ngrams)
    # return the model
    fit <- list(dtms = dtms,
                dts = dts)
    fit
}

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
    
    getWords <- function(ngram.i, words) {
        nwords <- sum(!is.na(words))
        if(nwords < n) {
            if(ngram.i > 1) {
                newwords <- as.character(model$dts[[ngram.i]][key[ngram.i-1]][order(-Prob)][1:n]$Word) # remove to go back to 5-gram and uncomment   
                words <- unique(c(words, newwords)) # remove words already suggested
                words <- words[!is.na(words)] # remove NA to get more suggestions
                words <- getWords(ngram.i = ngram.i-1, words)
            } else if(ngram.i == 1)
                words <- c(words,
                           as.character(model$dts[[ngram.i]][order(-Prob)][1:n]$Word))
            # clear vector of potential predictions from NAs
            # it is ordered by probabilities already during backoff but with higher-level n-gram priority
            # so there is a chance that probability of first words in this vector have lower probability
            # for stupid backoff this is the answer already.
            words <- words[!is.na(words)]
        } 
        # it is important to explicitly convert to character because
        # when sometimes it decides to return list with function, when
        # all values returned are NA. 
        as.character(words)
    }
    
    # run recursion over available n-grams, empty words list for the beginning
    predicted.Words <- getWords(ngram.i = ngrams, character())

    # in case interpolation is needed
    if(interpolate) {
        # create vector to store interpolated probabilities for each Word
        predicted.IProbs <- numeric(length(predicted.Words))
        # create vector for probabilities of a Word in each n-gram model
        
        # recursive function to get probabilities in each n-gram model
        # higher n-gram probability at the last place, lower at the first,
        # to be consistent with straight indexing if needed.
        if(0) { # switch off recursive version of the function
            getIProbs <- function(word, ngram.i) {
                probs <- numeric()
                if(ngram.i > 1) {
                    probs <- as.numeric(model$dts[[ngram.i]][.(key[ngram.i-1], word)]$Prob[1])
                    # place higher order n-gram probabilities at the end
                    probs <- c(getIProbs(word, ngram.i-1), probs)
                }
                else if(ngram.i == 1)
                    probs <- c(as.numeric(model$dts[[ngram.i]][.(word)]$Prob[1]), probs)
                # not all the combinations of phrase and Word were found in all n-gram models
                probs[is.na(probs)] <- 0
                probs
            }
        }
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

#    a <- c(0.02, 0.08, 0.1, 0.3, 0.5)
#    
#    predicted.word <- rbind(model$dts[[5]][phrase.four, nomatch = 0][order(-Prob)][, BOProb := Prob * a[5]][predicted.id],
#                            model$dts[[4]][phrase.three, nomatch = 0][order(-Prob)][, BOProb := Prob * a[4]][predicted.id],
#                            model$dts[[3]][phrase.two, nomatch = 0][order(-Prob)][, BOProb := Prob * a[3]][predicted.id],
#                            model$dts[[2]][phrase.one, nomatch = 0][order(-Prob)][, BOProb := Prob * a[2]][predicted.id],
#                            model$dts[[1]][order(-Prob)][predicted.id, BOProb := Prob * a[1]][predicted.id])
    
#    predicted <- cbind(phrase.four, predicted.word)
