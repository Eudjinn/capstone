makeTestList <- function(testset, maxdocs) {
    ndocs <- min(length(testset), maxdocs)
    testlist <- lapply(testset[1:ndocs], 
                      function(x) splitStringToSet(x, n = 4))
    testlist
}

testTM <- function(model, testlist, maxitems) {
    #cl <- makeForkCluster(no_cores)
    #testlist <- parLapply(cl, tm.all.sample.test, function(x) splitStringToSet(x$content, n = 4))
    #testpairs <- parSapply(cl, testlist, function(x) x$pair)
    testpairs <- sapply(testlist, function(x) { if(length(x$key) > 0) x$key })
    testpairs <- as.character(unlist(testpairs))
    nitems <- min(length(testpairs), maxitems)
    
    testpairs <- testpairs[1:nitems]
    #testwords <- parSapply(cl, testlist, function(x) x$word)
    testwords <- sapply(testlist, function(x) x$word)
    testwords <- as.character(unlist(testwords))
    testwords <- testwords[1:nitems]
    ###
    #predicted <- parSapply(cl, testpairs, function(y) predictWord1(y, freq.dt.uni, freq.dt.bi, freq.dt.tri, freq.dt.four, freq.dt.five, 1))
    predicted <- sapply(testpairs, 
                        function(y) predictTM(fit, y, n = 1))
    predicted <- unlist(predicted)
    predicted <- predicted[rep(c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE), nitems)]
    ###
    matches <- table(testwords == predicted)
    result <- list(predictions <- cbind(testpairs, testwords, predicted),
                   matches = matches,
                   accuracy = matches["TRUE"]/(matches["TRUE"] + matches["FALSE"]))
    #stopCluster(cl)
    result
}

testTMbo <- function(model, testlist, maxitems) {
    #cl <- makeForkCluster(no_cores)
    #testlist <- parLapply(cl, tm.all.sample.test, function(x) splitStringToSet(x$content, n = 4))
    #testpairs <- parSapply(cl, testlist, function(x) x$pair)
    testpairs <- sapply(testlist, function(x) { if(length(x$key) > 0) x$key })
    testpairs <- as.character(unlist(testpairs))
    nitems <- min(length(testpairs), maxitems)
    
    testpairs <- testpairs[1:nitems]
    #testwords <- parSapply(cl, testlist, function(x) x$word)
    testwords <- sapply(testlist, function(x) x$word)
    testwords <- as.character(unlist(testwords))
    testwords <- testwords[1:nitems]
    ###
    #predicted <- parSapply(cl, testpairs, function(y) predictWord1(y, freq.dt.uni, freq.dt.bi, freq.dt.tri, freq.dt.four, freq.dt.five, 1))
    predicted.df <- data.frame(numeric(), 
                               numeric(),
                               numeric(),
                               numeric(),
                               numeric(),
                               numeric())
    predicted.list <- list()
    i <- 1
                predicted <- sapply(testpairs, 
                    function(y) predictTMbo(fit, y, n = 1, a = c(0.05, 0.4, 0.4, 0.4, 1)))
                predicted.v <- unlist(predicted)
                predicted.v <- predicted[rep(c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE), nitems)]
                ###
                matches <- table(testwords == predicted.v)
                #stopCluster(cl)
                predicted.df <- data.frame(a1,a2,a3,a4,a5, 
                                           accuracy = matches["TRUE"]/(matches["TRUE"] + matches["FALSE"]))
                predicted.list[[i]] <- list(df = predicted.df) #,
  #                                       matches = matches,
 #                                        predictions = cbind(testpairs, testwords, predicted.v),
#                                         raw = predicted)
                i <- i + 1
    predicted.list
}