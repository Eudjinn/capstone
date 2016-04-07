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
    
    # remove intermediate columns if not needed for debugging    
    dt[, FreqSmooth := NULL]
    dt[, SumFreq := NULL]

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

    # remove intermediate columns if not needed for debugging    
    dt[, FreqSmooth := NULL]
    dt[, SumFreq := NULL]
    dt[, SumFreqSmooth := NULL]

    setkey(dt, Key, Word)
    dt
}

#############
cleandt <- function(dt, delete = NULL, trimFeatures = FALSE, threshold = 1) {
    
    # get rid of rare features:
    if(trimFeatures) {
        cat("Trimming features at threshold...", threshold, "\n")        
        dt <- dt[Freq > threshold]
    }

    # EXPERIMENTAL: Remove starts and ends from tables and other tags:
    # once smoothing is done, tags are not needed for prediction any more
    cat("Removing tags...\n")        
    tags <- "ss-ss|ee-ee|ww-ww|tm-tm|mm-mm|oo-oo|us-us|ie-ie|eg-eg|ad-ad|dr-dr|mr-mr|mrs-mrs|dc-dc|nn-nn|ys-ys|nx-nx|sx-sx"
    tagskey <- grep(tags, dt$Key)
    tagsword <- grep(tags, dt$Word)
    remove <- union(tagskey, tagsword)
    if(length(remove) > 0)
        dt <- dt[-remove]
    
    # remove bad words
    if(!is.null(delete)) {
        cat("Removing words from deletion list...\n")        
        setkey(dt, Key)
        dt <- dt[!delete]
        setkey(dt, Word)
        dt <- dt[!delete]
    }
    setkey(dt, Key, Word)
    dt
}

# Full training from beginning to end - when all the parameters needed are known.
# pass vector of character documents and get the model as a result.
# Smoothing: Ak, GT: Ak - add k, GT - Good-Turing
trainTM <- function(t = NULL, 
                    trimFeatures = FALSE, 
                    minCount = 3, 
                    minDoc = 2, 
                    smoothingType = "Ak", 
                    smoothK = 1, 
                    ngrams = 4,
                    delete = NULL,
                    ...) {
    cat("Training TM...", "Trim features:", trimFeatures, "Smoothing:", smoothingType, "\n")
    require(quanteda)
    
    voc.size <- 1 # will be updated from within next function
    # dtms - document term matrix - several. trimming inside.
    dts <- lapply(1:ngrams, function(i) {
        # add tags for tokenizing
        cat("Tagging...\n")
        t.tagged <- addtags(t, ngrams = i)
        dtm <- dfm(t.tagged, ngrams = i, concatenator = " ")
        
#        if(trimFeatures)
#            dtm <- trim(dtm, minCount = minCount, minDoc = minDoc)
        cat("Creating data tables...\n")        
        dt <- data.table(Key = getFirstNWords(features(dtm), i - 1), 
                         Word = getLastNWords(features(dtm), 1),
#                         Freq = docfreq(dtm, scheme = "count"))
                         Freq = colSums(dtm))

 #       unk <- data.table(Key = "uu-nn-kk", # unknown word with frequency 1 after trimming
 #                         Word = "uu-nn-kk",
 #                         Freq = colSums(dtm))
 #       l <- list(dt, unk)
 #       dt <- rbindlist(l) # add unk to dt
        
        rm(dtm) # save some resources, dtm is not needed anymore
        
        # Smoothing
        cat("Applying smoothing...\n")        
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
        
        # remove redundant columns - only Prob is needed
        #dt[, Freq := NULL]

        # remove unwanted tags and bad words using wierd formula
        threshold <- as.integer(minCount^(1/i) + 0.5/i)
        dt <- cleandt(dt, delete, trimFeatures, threshold)        
        
        # I have no idea when it resets the key - have to be on the safe side
        setkey(dt, Key, Word)
        dt # return result
    })

    # return the model
    fit <- list(#dtms = dtms,
        ngrams = ngrams,
        smoothing = smoothingType,
        dts = dts)
    fit
}
