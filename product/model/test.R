makeTestList <- function(testset, maxdocs, ngrams = 4) {
    ndocs <- min(length(testset), maxdocs)
    
    tags <- "ss-ss|ee-ee|ww-ww|tm-tm|mm-mm|oo-oo|us-us|ie-ie|eg-eg|ad-ad|dr-dr|mr-mr|mrs-mrs|dc-dc|nn-nn|ys-ys"
    testset <- gsub(tags, " ", testset)
    
    # collapse spaces in one space
    testset <- gsub("[[:space:]]{2,}", " ", testset)
    # remove spaces in the beginning and at the end
    testset <- gsub("^ | $", "", testset)
    
    testlist <- lapply(testset[1:ndocs], 
                      function(x) splitStringToSet(x, ngrams))
    testlist
}

testTM <- function(model, testlist, maxitems, n = 1, method = "none", alpha = 1, l = c(0.1, 0.15, 0.3, 0.45)) {
    testpairs <- sapply(testlist, function(x) { if(length(x$key) > 0) x$key })
    testpairs <- as.character(unlist(testpairs))
    nitems <- min(length(testpairs), maxitems)
    
    testpairs <- testpairs[1:nitems]
    testwords <- sapply(testlist, function(x) x$word)
    testwords <- as.character(unlist(testwords))
    testwords <- testwords[1:nitems]

    cat("Running predictTM...\n")
    predicted <- sapply(testpairs, 
                           function(y) {
                               p <- predictTM(model = model, 
                                                 phrase = y, 
                                                 n = n, 
                                                 method = method,
                                                 alpha = alpha,
                                                 l = l)
                               as.character(p$Word)
                           })
        
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
    result
}
