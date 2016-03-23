library(tm)
library(profr)

no_cores <- max(1, detectCores() - 1)
options(mc.cores = no_cores)
sample.size <- as.integer(10)

all.sample.test <- sample(all, sample.size)
all.sample.test <- cleandoc(all.sample.test)

tm.all.sample.test <- Corpus(VectorSource(all.sample.test), readerControl = list(reader = readPlain, language = "en", load = TRUE))

tm.all.sample.test <- tm_map(tm.all.sample.test, removeNumbers)
tm.all.sample.test <- tm_map(tm.all.sample.test, stripWhitespace)
tm.all.sample.test <- tm_map(tm.all.sample.test, content_transformer(tolower))
tm.all.sample.test <- tm_map(tm.all.sample.test, content_transformer(PlainTextDocument))
###

#cl <- makeForkCluster(no_cores)

#devtest <- parLapply(cl, tm.all.sample.test, function(x) splitStringToSet(x$content, n = 4))
#testpairs <- parSapply(cl, devtest, function(x) x$pair)
devtest <- lapply(tm.all.sample.test, function(x) splitStringToSet(x$content, n = 4))
testpairs <- sapply(devtest, function(x) x$key)
testpairs <- as.character(unlist(testpairs))
#testwords <- parSapply(cl, devtest, function(x) x$word)
testwords <- sapply(devtest, function(x) x$word)
testwords <- as.character(unlist(testwords))
###
#predicted <- parSapply(cl, testpairs, function(y) predictWord1(y, freq.dt.uni, freq.dt.bi, freq.dt.tri, freq.dt.four, freq.dt.five, 1))
profr(predicted <- sapply(testpairs, function(y) predictWord2(y, freq.dt.uni, freq.dt.bi, freq.dt.tri, freq.dt.four, freq.dt.five, 1)), 
      interval= 0.005)
predicted <- as.character(predicted)
###
result <- table(testwords == predicted)
result
accuracy = result["TRUE"]/(result["TRUE"] + result["FALSE"])
accuracy

res <- cbind(testpairs, testwords, predicted)
#stopCluster(cl)

