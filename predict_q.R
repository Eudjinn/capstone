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

addTags <- function(textdata, ngrams = 5) {  
    textdata <- addtags(textdata, ngrams)
}

# pass vector of character documents
# Smoothing: Ak, GT: Ak - add k, GT - Good-Turing
trainTM <- function(t, trimFeatures = FALSE, smoothingType = NULL, smoothK = 1, ngrams = 5, ...) {
    require(quanteda)
    # dtms - document term matrix - several
    dtms <- lapply(1:ngrams, function(i) {
        # add tags for tokenizing
        t.tagged <- addTags(t, ngrams = i)
        dtm <- dfm(t.tagged, ngrams = i, concatenator = " ")
        if(trimFeatures) {
            # trim rare terms
            dtm <- trim(dtm, minCount = 2, minDoc = 1)
        }
        dtm
    })

    # dts = data table several 
    dts <- lapply(1:ngrams, function(i) {
        dt <- data.table(Key = getFirstNWords(features(dtms[[i]]), i - 1), 
                         Word = getLastNWords(features(dtms[[i]]), 1),
                         # Freq = docfreq(dtm.l$onegram, 
                         # scheme = "count")),
                         Freq = colSums(dtms[[i]]))
    
        # EXPERIMENTAL: Remove starts and ends from tables:
        tagskey <- grep("ss-ss|ee-ee", dt$Key)
        if(length(tagskey) > 0)
            dt <- dt[-tagskey]
        
        tagsword <- grep("ss-ss|ee-ee", dt$Word)
        if(length(tagsword) > 0)
            dt <- dt[-tagsword]
        dt
    })
    
    smooth.n <- function(dt, k = 1, ngram = 1, V = 1) {
        if(ngram == 1) {
            N <- sum(dt$Freq)
            dt[, Prob := (Freq + k)/(N + k*V)]
            dt[, FreqSmooth := Prob * N]
        } else {
            setkey(dt, Key)
            dt[, Prob := (Freq + k)/(sum(Freq) + k*V), by = Key]
            dt[, FreqSmooth := Prob * sum(Freq), by = Key]
        }
        setkey(dt, Key)
        dt
    }
    
    # EXPERIMENTAL Good-Turing Smoothing.
    smooth.n.gt <- function(dt, k = 1, ngram = 1, V = 1) {
        require(randomForest)
        gt <- data.frame(Freq = as.integer(names(table(dt$Freq))), 
                         FOfF = as.integer(table(dt$Freq)))
        gt.fit <- randomForest(FOfF ~ Freq, data = gt)
        dt$FofF <- predict(gt.fit, data.frame(Freq = dt$Freq))
        dt$FofF1 <- predict(gt.fit, data.frame(Freq = (dt$Freq + k)))
        
        if(ngram == 1) {
            N <- sum(dt$Freq)
            dt[, FreqSmooth := (Freq + k) * FofF1/FofF]
            dt[, Prob := FreqSmooth/(N + k*V)]
        } else {
            setkey(dt, Key)
            dt[, FreqSmooth := (Freq + k) * FofF1/FofF]
            dt[, Prob := FreqSmooth/(sum(FreqSmooth) + k*V), by = Key]
        }
        setkey(dt, Key)
        dt
    }
    
    # Add smoothing
    # Vocabulary size is the length of onegram dt, which is the first in the list
    voc.size <- nrow(dts[[1]])
    dts <- lapply(1:ngrams, function(i) {
        if(smoothingType == "Ak")
            dts <- smooth.n(dts[[i]], k = smoothK, V = voc.size)
        else if(smoothingType == "GT")
            dts <- smooth.n.gt(dts[[i]], k = smoothK, V = voc.size)
        dts
    })
            
    fit <- list(dtms = dtms,
                dts = dts)
    fit
}

# REWRITE
predictTM <- function(model, phrase, n = 1) {
    # stupid protection from phrases with less then 4 words.
    # need to be rewritten
    dummy <- "<notaword> <notaword> <notaword> <notaword>"
    phrase <- paste(dummy, phrase)
    phrase.four <- getLastNWords(phrase, 4)
    phrase.three <- getLastNWords(phrase, 3)
    phrase.two <- getLastNWords(phrase, 2)
    phrase.one <- getLastNWords(phrase, 1)
    predicted.id <- 1:n #which.max(prob)
    
    predicted.word <- model$dts[[5]][phrase.four][order(-Prob)][predicted.id]
    if(is.na(predicted.word$Word[1])) {
        predicted.word <- model$dts[[4]][phrase.three][order(-Prob)][predicted.id]
        if(is.na(predicted.word$Word[1])) {
            predicted.word <- model$dts[[3]][phrase.two][order(-Prob)][predicted.id]
            if(is.na(predicted.word$Word[1])) {
                predicted.word <- model$dts[[2]][phrase.one][order(-Prob)][predicted.id]
                if(is.na(predicted.word$Word[1])){
                    predicted.word <- model$dts[[1]][order(-Prob)][predicted.id] 
                }
            }
        }
    }
#    predicted.word
    predicted.word$Word
}

predictTMbo <- function(model, phrase, n = 1, a = c(1, 1, 1, 1, 1)) {
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
