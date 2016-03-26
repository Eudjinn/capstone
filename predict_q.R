###############
## Loading data
readData <- function()
{
    blogs <- readLines("final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
#    news <- readLines("final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
#    twitter <- readLines("final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)
    
#    all <- c(blogs, news, twitter)
    all <- blogs
    all
}

## Sampling
getSample <- function(text, sample.p = 0.01)
{
    text.length <- length(text)
    sampletext <- text[sample(1:text.length, text.length * sample.p)]
    sampletext
}

## cleaning
cleanData <- function(textdata)
{
    textdata <- cleandoc(textdata)
    textdata
}

# pass vector of character documents
trainTM <- function(t, trimFeatures = FALSE, smoothK = 1, ...) {
    require(quanteda)
    # create corpus on training set
    c <- corpus(t)
    # n-grams
    dtm.l <- list(onegram = dfm(t),
                  twogram = dfm(t, ngrams = 2L, concatenator = " "),
                  threegram = dfm(t, ngrams = 3L, concatenator = " "),
                  fourgram = dfm(t, ngrams = 4L, concatenator = " "),
                  fivegram = dfm(t, ngrams = 5L, concatenator = " "))

    if(trimFeatures) {
        # trim rare terms
        dtm.l <- lapply(dtm.l, function(x) trim(x, minCount = 2, minDoc = 1))
    }
    ### Training

    dt.l <- list(onegram = data.table(Key = features(dtm.l$onegram), 
                                      Word = features(dtm.l$onegram), 
                                      Freq = docfreq(dtm.l$onegram, 
                                                     scheme = "count")),
                 twogram = data.table(Key = getFirstNWords(features(dtm.l$twogram), 1), 
                                      Word = getLastNWords(features(dtm.l$twogram), 1), 
                                      Freq = docfreq(dtm.l$twogram, 
                                                     scheme = "count")),
                 threegram = data.table(Key = getFirstNWords(features(dtm.l$threegram), 2), 
                                        Word = getLastNWords(features(dtm.l$threegram), 1), 
                                        Freq = docfreq(dtm.l$threegram, 
                                                       scheme = "count")),
                 fourgram = data.table(Key = getFirstNWords(features(dtm.l$fourgram), 3), 
                                       Word = getLastNWords(features(dtm.l$fourgram), 1), 
                                       Freq = docfreq(dtm.l$fourgram, 
                                                      scheme = "count")),
                 fivegram = data.table(Key = getFirstNWords(features(dtm.l$fivegram), 4), 
                                        Word = getLastNWords(features(dtm.l$fivegram), 1), 
                                        Freq = docfreq(dtm.l$fivegram, 
                                                       scheme = "count")))
    # add smoothing
    voc.size <- nrow(dt.l$onegram)

    smooth.one <- function(dt, k = 1, V) {
        N <- sum(dt$Freq)
    
        dt[, Prob := (Freq + k)/(N + k*V)]
        dt[, FreqSmooth := Prob * N]
        setkey(dt, Key)
    }
    
    smooth.n <- function(dt, k = 1, V) {
        setkey(dt, Key)
        dt[, Prob := (Freq + k)/(sum(Freq) + k*V), by = Key]
        dt[, FreqSmooth := Prob * sum(Freq), by = Key]
        setkey(dt, Key)
    }
    
    smooth.one(dt.l$onegram, k = smoothK, V = voc.size)
    smooth.n(dt.l$twogram, k = smoothK, V = voc.size)
    smooth.n(dt.l$threegram, k = smoothK, V = voc.size)
    smooth.n(dt.l$fourgram, k = smoothK, V = voc.size)
    smooth.n(dt.l$fivegram, k = smoothK, V = voc.size)
    
    fit <- list(dtm = dtm.l,
                dt = dt.l)
    fit
}

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
    
    predicted.word <- model$dt$fivegram[phrase.four][order(-Prob)][predicted.id]
    if(is.na(predicted.word$Word[1])) {
        predicted.word <- model$dt$fourgram[phrase.three][order(-Prob)][predicted.id]
        if(is.na(predicted.word$Word[1])) {
            predicted.word <- model$dt$threegram[phrase.two][order(-Prob)][predicted.id]
            if(is.na(predicted.word$Word[1])) {
                predicted.word <- model$dt$twogram[phrase.one][order(-Prob)][predicted.id]
                if(is.na(predicted.word$Word[1])){
                    predicted.word <- model$dt$onegram[order(-Prob)][predicted.id] 
                }
            }
        }
    }
    cbind(phrase.four, predicted.word)
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

    predicted.word5 <- model$dt$fivegram[phrase.four, nomatch = 0][, BOProb := Prob][order(-Prob)][predicted.id]
    predicted.word4 <- model$dt$fourgram[phrase.three, nomatch = 0][, BOProb := Prob][order(-Prob)][predicted.id]
    predicted.word3 <- model$dt$threegram[phrase.two, nomatch = 0][, BOProb := Prob][order(-Prob)][predicted.id]
    predicted.word2 <- model$dt$twogram[phrase.one, nomatch = 0][, BOProb := Prob][order(-Prob)][predicted.id]
    predicted.word1 <- model$dt$onegram[order(-Prob)][predicted.id, BOProb := Prob][predicted.id]
    
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

    cbind(phrase.four, predicted.word)
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

    match <- model$dt$fivegram[phrase.four][Word == word]
    if(length(match$Word) == 0) {
        match <- model$dt$fourgram[phrase.three][Word == word]
        if(length(match$Word) == 0) {
            match <- model$dt$threegram[phrase.two][Word == word]
            if(length(match$Word) == 0) {
                match <- model$dt$twogram[phrase.one][Word == word]
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
#    predicted.word <- rbind(model$dt$fivegram[phrase.four, nomatch = 0][order(-Prob)][, BOProb := Prob * a[5]][predicted.id],
#                            model$dt$fourgram[phrase.three, nomatch = 0][order(-Prob)][, BOProb := Prob * a[4]][predicted.id],
#                            model$dt$threegram[phrase.two, nomatch = 0][order(-Prob)][, BOProb := Prob * a[3]][predicted.id],
#                            model$dt$twogram[phrase.one, nomatch = 0][order(-Prob)][, BOProb := Prob * a[2]][predicted.id],
#                            model$dt$onegram[order(-Prob)][predicted.id, BOProb := Prob * a[1]][predicted.id])
    
#    predicted <- cbind(phrase.four, predicted.word)
