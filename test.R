makeTestList <- function(testset, maxdocs, ngrams = 4) {
    ndocs <- min(length(testset), maxdocs)
    testlist <- lapply(testset[1:ndocs], 
                      function(x) splitStringToSet(x, ngrams))
    testlist
}

testTM <- function(model, testlist, maxitems, n = 1, ngrams = 4, a = NULL, interpolate = FALSE, l = c(0.1, 0.15, 0.3, 0.45), parallel = FALSE) {
    if(parallel) {
        no_cores <- max(1, detectCores() - 1)
        cl <- makeForkCluster(no_cores)
    }
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
    if(!interpolate & is.null(a)) {
        cat("Running predictTM...\n")
        if(parallel)
            predicted <- parSapply(cl, testpairs, 
                                function(y) predictTM(model = model, phrase = y, n = n, ngrams = ngrams))
        else
            predicted <- sapply(testpairs, 
                                   function(y) predictTM(model = model, phrase = y, n = n, ngrams = ngrams))
    }
    else if(!is.null(a) & ! interpolate) {
        cat("Running predictTMbo...\n")
        if(parallel)
            predicted <- parSapply(cl, testpairs, 
                            function(y) predictTMbo(model = model, phrase = y, n = n, ngrams = ngrams, a))
        else
            predicted <- sapply(testpairs, 
                                   function(y) predictTMbo(model = model, phrase = y, n = n, ngrams = ngrams, a))
        
    }
    else if(interpolate & is.null(a)) {
        cat("Running predictTMInt...\n")
        if(parallel)
            predicted <- parSapply(cl, testpairs, 
                                   function(y) predictTMInt(model = model, phrase = y , n = n, ngrams = ngrams, l = l))
        else
            predicted <- sapply(testpairs, 
                                function(y) predictTMInt(model = model, phrase = y , n = n, ngrams = ngrams, l = l))
        
    }
    else
        cat("Wrong test parameters!")
    
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
    if(parallel) 
        stopCluster(cl)
    result
}
