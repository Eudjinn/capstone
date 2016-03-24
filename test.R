library(tm)
library(profr)

no_cores <- max(1, detectCores() - 1)
options(mc.cores = no_cores)
validation.ndocs <- as.integer(50)
validation.size <- as.integer(1000)

tm.all.sample.test <- Corpus(VectorSource(all.sample.test[1:validation.ndocs]), readerControl = list(reader = readPlain, language = "en", load = TRUE))

tm.all.sample.test <- tm_map(tm.all.sample.test, removeNumbers)
tm.all.sample.test <- tm_map(tm.all.sample.test, stripWhitespace)
tm.all.sample.test <- tm_map(tm.all.sample.test, content_transformer(tolower))
tm.all.sample.test <- tm_map(tm.all.sample.test, content_transformer(PlainTextDocument))
###

#cl <- makeForkCluster(no_cores)

#devtest <- parLapply(cl, tm.all.sample.test, function(x) splitStringToSet(x$content, n = 4))
#testpairs <- parSapply(cl, devtest, function(x) x$pair)
devtest <- lapply(tm.all.sample.test[1:validation.ndocs], 
                  function(x) splitStringToSet(x$content, n = 4))
testpairs <- sapply(devtest, function(x) x$key)
testpairs <- as.character(unlist(testpairs))
testpairs <- testpairs[1:validation.size]
#testwords <- parSapply(cl, devtest, function(x) x$word)
testwords <- sapply(devtest, function(x) x$word)
testwords <- as.character(unlist(testwords))
testwords <- testwords[1:validation.size]
###
#predicted <- parSapply(cl, testpairs, function(y) predictWord1(y, freq.dt.uni, freq.dt.bi, freq.dt.tri, freq.dt.four, freq.dt.five, 1))
predicted <- sapply(testpairs, 
                    function(y) predictWord3(y, dt.l$one, dt.l$two, dt.l$three, dt.l$four, dt.l$five, n = 1))
predicted <- unlist(predicted)
predicted <- predicted[rep(c(FALSE, TRUE), validation.size)]
###
result <- table(testwords == predicted)
result
accuracy = result["TRUE"]/(result["TRUE"] + result["FALSE"])
accuracy

res <- cbind(testpairs, testwords, predicted)
#stopCluster(cl)

