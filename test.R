makeTestList <- function(testset, maxdocs) {
    ndocs <- min(length(testset), maxdocs)
    testlist <- lapply(testset[1:ndocs], 
                      function(x) splitStringToSet(x, n = 4))
    testlist
}

testTM <- function(model, testlist, maxitems, n = 1, a = NULL) {
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
    if(is.null(a))
        predicted <- sapply(testpairs, 
                            function(y) predictTM(fit, y, n))
    else if(length(a) == 5)
        predicted <- sapply(testpairs, 
                            function(y) predictTMbo(fit, y, n, a))
        
    predicted <- t(as.matrix(predicted, n))
    predicted[is.na(predicted)] <- "<unk>"

    rownames(testpairs) <- NULL
    rownames(testwords) <- NULL
    rownames(predicted) <- NULL
    
    equal <- logical(nitems)
    for (i in 1:n) {
        equal <- (equal | testwords == predicted[, i])
    }
    matches <- table(equal)
    result <- list(predictions = cbind(testpairs, 
                                        testwords, 
                                        predicted,
                                        equal),
                   matches = matches,
                   accuracy = matches["TRUE"]/(matches["TRUE"] + matches["FALSE"]))
    #stopCluster(cl)
    result
}
