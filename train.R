makeDTMs <- function(t = NULL, ngrams = 4, trimFeatures = FALSE, minCount = 3, minDoc = 2) {
    cat("Making DTMs, Trim Features:", trimFeatures, "\n")
    require(quanteda)
    
    # dtms - document term matrix - several
    dtms <- lapply(1:ngrams, function(i) {
        # add tags for tokenizing
        t.tagged <- addTags(t, ngrams = i)
        dtm <- dfm(t.tagged, ngrams = i, concatenator = " ")
        #       if(trimFeatures) {
        # trim rare terms
        #           dtm <- trim(dtm, minCount = minCount, minDoc = minDoc)
        #       }
        dtm
    })
    dtms
}

# used for testing as trimming is done within makeDTM
trimDTMs <- function(dtms = NULL, ngrams = 4, trimFeatures = FALSE, minCount = 3, minDoc = 2) {
    cat("Triming dtms, Trim Features: ", trimFeatures, "\n")
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
    cat("Making DTs...\n")
    # dts = data table several 
    dts <- lapply(1:ngrams, function(i) {
        dt <- data.table(Key = getFirstNWords(features(dtms[[i]]), i - 1), 
                         Word = getLastNWords(features(dtms[[i]]), 1),
                         # Freq = docfreq(dtm.l$onegram, 
                         # scheme = "count")),
                         Freq = colSums(dtms[[i]]))
        dt
    })
    dts
}

# Smoothing functions ============
# Add-k smoothing
smooth.n <- function(dt, ngram.i = 1, k = 1, V = 1) {
    N <- sum(dt$Freq)
    V <- nrow(dt)
    if(ngram.i == 1) {
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
smooth.n.gt <- function(dt, ngram.i = 1, k = 1, V = 1) {
    # approximation is needed for frequencies that are not present
    #        require(randomForest)
    #        gt <- data.frame(Freq = as.integer(names(table(dt$Freq))), 
    #                         FOfF = as.integer(table(dt$Freq)))
    #        gt.fit <- randomForest(FOfF ~ Freq, data = gt)
    #        dt$FofF <- predict(gt.fit, data.frame(Freq = dt$Freq))
    #        dt$FofF1 <- predict(gt.fit, data.frame(Freq = (dt$Freq + k)))
    FofFt <- tabulate(dt$Freq)
    FofFt.len <- length(FofFt)
    # find a place where to stop like this:
    FofFt.diff <- FofFt[1:FofFt.len-1] - FofFt[2:FofFt.len]
    t <- min(which(FofFt.diff <= 0)) - 2
    # just in case it is even below zero in very unprobable case
    if(t < 2) t <- 1
    # if irregularities start too early compared to k parameter
    if(k > t) k <- t
    
    N <- sum(dt$Freq)
    V <- nrow(dt)
    
    dt[Freq <= k, FreqSmooth := (((Freq+1)*FofFt[Freq+1]/FofFt[Freq])-((Freq*(k+1)*FofFt[k+1])/FofFt[1]))/(1-(k+1)*FofFt[k+1]/FofFt[1])]
    dt[Freq >= k & Freq < t, FreqSmooth := (Freq+1)*FofFt[Freq+1]/FofFt[Freq]]
    dt[Freq >= t, FreqSmooth := as.numeric(Freq)] # for higher frequency just leave as is
    
    if(ngram.i == 1) {
        dt[, SumFreq := sum(Freq)] # not needed actually, added only for consistency to match columns in ngram > 1
        dt[, SumFreqSmooth := sum(FreqSmooth)] # not needed actually, added only for consistency to match columns in ngram > 1
        dt[, Prob := FreqSmooth/N]
    } else {
        setkey(dt, Key)
        dt[, SumFreq := sum(Freq), by = Key]
        dt[, SumFreqSmooth := sum(FreqSmooth), by = Key]
        dt[, Prob := FreqSmooth/SumFreq] # Dividing by SumFreq causes some probabilities to be more than 1
    }
    setkey(dt, Key, Word)
    dt
}
#############

smoothDTs <- function(dts = NULL, ngrams = 4, smoothingType = "none", smoothK = 1) {
    cat("Smoothing DTs, Smoothing Type:", smoothingType, ", k =", smoothK, "\n")
    
    # Add smoothing
    # Vocabulary size is the length of onegram dt, which is the first in the list
    voc.size <- nrow(dts[[1]])
    
    dts <- lapply(1:ngrams, function(i) {
        if(smoothingType == "Ak")
            dt <- smooth.n(dts[[i]], ngram.i = i, k = smoothK, V = voc.size)
        else if(smoothingType == "GT")
            dt <- smooth.n.gt(dts[[i]], ngram.i = i, k = smoothK, V = voc.size)
        else {
            # if no smoothing selected - calculate probability with k = 0.
            dt <- smooth.n(dts[[i]], ngram.i = i, k = 0)
        }
        dt
    })
    dts
}

cleanDTs <- function(dts = NULL, ngrams = 4, trimFeatures = FALSE, minFreq = 2) {
    cat("Cleaning DTs...\n")
    dts <- lapply(1:ngrams, function(i) {
        # EXPERIMENTAL: Remove starts and ends from tables and other tags:
        tagskey <- grep("ss-ss|ee-ee|ww-ww", dts[[i]]$Key)
        if(length(tagskey) > 0)
            dts[[i]] <- dts[[i]][-tagskey]
        
        tagsword <- grep("ss-ss|ee-ee|ww-ww", dts[[i]]$Word)
        if(length(tagsword) > 0)
            dts[[i]] <- dts[[i]][-tagsword]
        
        # get rid of rare features:
        if(trimFeatures)
            dts[[i]] <- dts[[i]][Freq > minFreq]
        
        dts[[i]] # return result
    })
    dts
}

trainTM <- function(t = NULL, 
                    trimFeatures = FALSE, 
                    minCount = 3, 
                    minDoc = 2, 
                    smoothingType = "none", 
                    smoothK = 1, 
                    ngrams = 4,
                    ...) {
    cat("Training TM...", "Trim features:", trimFeatures, "Smoothing:", smoothingType, "\n")
    require(quanteda)
    
    voc.size <- 1 # will be updated from within next function
    # dtms - document term matrix - several. trimming inside.
    dts <- lapply(1:ngrams, function(i) {
        # add tags for tokenizing
        t.tagged <- addTags(t, ngrams = i)
        dtm <- dfm(t.tagged, ngrams = i, concatenator = " ")
        
#        if(trimFeatures)
#            dtm <- trim(dtm, minCount = minCount, minDoc = minDoc)
        
        dt <- data.table(Key = getFirstNWords(features(dtm), i - 1), 
                         Word = getLastNWords(features(dtm), 1),
                         Freq = docfreq(dtm, scheme = "count"))
                         #Freq = colSums(dtm))

 #       unk <- data.table(Key = "uu-nn-kk", # unknown word with frequency 1 after trimming
 #                         Word = "uu-nn-kk",
 #                         Freq = colSums(dtm))
 #       l <- list(dt, unk)
 #       dt <- rbindlist(l) # add unk to dt
        
        rm(dtm) # save some resources, dtm is not needed anymore

        
        # EXPERIMENTAL: Remove starts and ends which came from middle of the 
        # string and mean nothing for prediction from tables:
        tagskey <- grep("ss-ss|ee-ee", dt$Key)
        tagsword <- grep("ss-ss|ee-ee", dt$Word)
        # remove only when both tagskey and tagsword ara tags
        remove <- intersect(tagskey, tagsword)
        if(length(remove) > 0)
            dt <- dt[-remove]

        # Smoothing
        if(i == 1) {
            voc.size <<- nrow(dt) # save to upper context for other i
        }
        if(smoothingType == "Ak") {
            smooth.n(dt, ngram.i = i, k = smoothK, V = voc.size)
        } else if(smoothingType == "GT") {
            smooth.n.gt(dt, ngram.i = i, k = smoothK, V = voc.size)
        } else {
            # if no smoothing selected - calculate probability with k = 0 (MLE)
            dt <- smooth.n(dts[[i]], ngram.i = i, k = 0)
        }
        
        # EXPERIMENTAL: Remove starts and ends from tables and other tags:
        # once smoothing is done, tags are not needed for prediction any more
        tags <- "ss-ss|ee-ee|ww-ww|tm-tm|mm-mm|oo-oo|us-us|ie-ie|eg-eg|ad-ad|dr-dr|mr-mr|mrs-mrs|dc-dc|nn-nn|ys-ys"
        tagskey <- grep(tags, dt$Key)
        tagsword <- grep(tags, dt$Word)
        remove <- union(tagskey, tagsword)
        if(length(remove) > 0)
            dt <- dt[-remove]
        
        # get rid of rare features:
        if(trimFeatures)
            dt <- dt[Freq > minCount]
        
        dt # return result
    })

    # return the model
    fit <- list(#dtms = dtms,
        dts = dts)
    fit
}

# Full training from beginning to end - when all the parameters needed are known.
# pass vector of character documents and get the model as a result.
# Smoothing: Ak, GT: Ak - add k, GT - Good-Turing

if(0) { #fragmented version
    trainTM <- function(t = NULL, 
                        trimFeatures = FALSE, 
                        minCount = 3, 
                        minDoc = 2, 
                        smoothingType = "none", 
                        smoothK = 1, 
                        ngrams = 4,
                        ...) {
        cat("Training TM...", "Trim features:", trimFeatures, "Smoothing:", smoothingType, "\n")
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
                        ngrams = ngrams,
                        trimFeatures = trimFeatures,
                        minFreq = minCount)
        # return the model
        fit <- list(#dtms = dtms,
            dts = dts)
        fit
    }
}