###############
## Loading data
readData <- function() {
    blogs <- readLines("final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
    news <- readLines("final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
    twitter <- readLines("final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)
    
    all <- c(blogs, news, twitter)
#    all <- blogs
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
predictTM <- function(model, phrase, n = 1, ngrams = 4) {
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

#    predicted.word <- model$dts[[5]][key[4]][order(-Prob)][1:n]
#    nres <- sum(!is.na(predicted.word$Word)) 
#    if(nres < n) {
#        predicted.word <- rbind(predicted.word[complete.cases(predicted.word), ],
#                                model$dts[[4]][key[3]][order(-Prob)][1:n])
    predicted.word <- model$dts[[4]][key[3]][order(-Prob)][1:n] # remove and uncomment to go back to 5-gram
        nres <- sum(!is.na(predicted.word$Word))
        if(nres < n) {
            predicted.word <- rbind(predicted.word[complete.cases(predicted.word), ],
                                    model$dts[[3]][key[2]][order(-Prob)][1:n])
            nres <- sum(!is.na(predicted.word$Word))
            if(nres < n) {
                predicted.word <- rbind(predicted.word[complete.cases(predicted.word), ],
                                        model$dts[[2]][key[1]][order(-Prob)][1:n])
                nres <- sum(!is.na(predicted.word$Word))
                if(nres < n){
                    predicted.word <- rbind(predicted.word[complete.cases(predicted.word), ],
                                            model$dts[[1]][order(-Prob)][1:n])
                }
            }
        }
#    } # 5-gram
    predicted.word$Word[1:n]
}

predictTMInt <- function(model, phrase, n = 1, ngrams = 4, l = c(0.1, 0.15, 0.3, 0.45)) {
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
    
#    predicted.Word <- model$dts[[5]][key[4]][order(-Prob)][1:n]
#    nres <- sum(!is.na(predicted.Word$Word)) 
#    if(nres < n) {
#        predicted.Word <- rbind(predicted.Word[complete.cases(predicted.Word), ],
#                                model$dts[[4]][key[3]][order(-Prob)][1:n])
    predicted.Word <- as.character(model$dts[[4]][key[3]][order(-Prob)][1:n]$Word) # remove to go back to 5-gram and uncomment
        nres <- sum(!is.na(predicted.Word))
        if(nres < n) {
            predicted.Word <- c(predicted.Word,
                                as.character(model$dts[[3]][key[2]][order(-Prob)][1:n]$Word))
            nres <- sum(!is.na(predicted.Word))
            if(nres < n) {
                predicted.Word <- c(predicted.Word,
                                    as.character(model$dts[[2]][key[1]][order(-Prob)][1:n]$Word))
                nres <- sum(!is.na(predicted.Word))
                if(nres < n){
                    predicted.Word <- c(predicted.Word,
                                        as.character(model$dts[[1]][order(-Prob)][1:n]$Word))
                }
            }
        }
#    } # 5-gram
    # clear vector of potential predictions from NAs
    predicted.Word <- predicted.Word[!is.na(predicted.Word)]
    # create vector to store interpolated probabilities for each Word
    predicted.iProbs <- numeric(length(predicted.Word))
    # create vector for probabilities of a Word in each n-gram model
    probs <- numeric(ngrams)
    for(i in 1:length(predicted.Word)) {
#        probs[5] <- model$dts[[5]][key[4]][Word == predicted.Word[i]]$Prob[1]
        probs[4] <- as.numeric(model$dts[[4]][.(key[3],predicted.Word[i])]$Prob[1])
        probs[3] <- as.numeric(model$dts[[3]][.(key[2],predicted.Word[i])]$Prob[1])
        probs[2] <- as.numeric(model$dts[[2]][.(key[1],predicted.Word[i])]$Prob[1])
        probs[1] <- as.numeric(model$dts[[1]][.(predicted.Word[i])]$Prob[1])
        # not all the combinations of phrase and Word were found in all n-gram models
        probs[is.na(probs)] <- 0
        
        predicted.iProbs[i] <- sum(probs * l)
    }
    predicted <- data.frame(Word = predicted.Word, 
                            iprob = predicted.iProbs, 
                            stringsAsFactors = FALSE)
    # data frame with results
    predicted[order(predicted$iprob, decreasing = TRUE), ][1:n, ]$Word
}

# WILL NOT WORK WITH 4-GRAM
predictTMbo <- function(model, phrase, n = 1, ngrams = 4,  a = c(1, 1, 1, 1, 1)) {
    # stupid protection from phrases with less then 4 words.
    # need to be rewritten
    dummy <- "<notaword> <notaword> <notaword> <notaword>"
    phrase <- paste(dummy, phrase)
    phrase.four <- getLastNWords(phrase, 4)
    phrase.three <- getLastNWords(phrase, 3)
    phrase.two <- getLastNWords(phrase, 2)
    phrase.one <- getLastNWords(phrase, 1)
    predicted.id <- 1:n #which.max(prob)

    predicted.word5 <- model$dts[[5]][phrase.four, nomatch = 0][, BOProb := Prob][order(-Prob)][predicted.id]
    predicted.word4 <- model$dts[[4]][phrase.three, nomatch = 0][, BOProb := Prob][order(-Prob)][predicted.id]
    predicted.word3 <- model$dts[[3]][phrase.two, nomatch = 0][, BOProb := Prob][order(-Prob)][predicted.id]
    predicted.word2 <- model$dts[[2]][phrase.one, nomatch = 0][, BOProb := Prob][order(-Prob)][predicted.id]
    predicted.word1 <- model$dts[[1]][order(-Prob)][predicted.id, BOProb := Prob][predicted.id]
    
    if(!is.na(predicted.word5$Word[1])) {
        predicted.word4 <- predicted.word4[, BOProb := Prob * a[4]]
        predicted.word <- rbind(predicted.word5, 
                                predicted.word4)
    }
    else if(!is.na(predicted.word4$Word[1])) {
        predicted.word3 <- predicted.word3[, BOProb := Prob * a[3]]
        predicted.word <- rbind(predicted.word4, 
                                predicted.word3)
    }
    else if(!is.na(predicted.word3$Word[1])) {
        predicted.word2 <- predicted.word2[, BOProb := Prob * a[2]]
        predicted.word <- rbind(predicted.word3, 
                                predicted.word2)
    }
    else if(!is.na(predicted.word2$Word[1])) {
        predicted.word1 <- predicted.word1[, BOProb := Prob * a[1]]
        predicted.word <- rbind(predicted.word2, 
                                predicted.word1)
    }
    else 
        predicted.word <- predicted.word1
    
    predicted.word <- predicted.word[!is.na(BOProb)][order(-BOProb)][predicted.id]

    predicted.word$Word
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
