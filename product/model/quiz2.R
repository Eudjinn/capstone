q2.keys <- c("and a case of",
             "it would mean the",
             "and make me the",
             "still struggling but the",
             "romantic date at the",
             "and be on my",
             "it in quite some",
             "eyes with his little",
             "the faith during the",
             "then you must be")

q2.words <- c("beer",
              "world",
              "happiest",
              "defense",
              "beach",
              "way",
              "time",
              "fingers",
              "bad",
              "insane")

q3.keys <- c("I'd live and I'd",
             "telling me about his",
             "see arctic monkeys this",
             "and helps reduce your",
             "time to take a",
             "jury to settle the",
             "of groceries in each",
             "the bottom to the",
             "and bruises from playing",
             "all of Adam Sandler's")

q3.words <- c("die",
              "marital",
              "weekend",
              "stress",
              "picture",
              "matter",
              "hand",
              "top",
              "outside",
              "movies")

quizTest <- function(fit = NULL, 
                     testkeys = NULL, 
                     testwords = NULL, 
                     n = 3, 
                     method = "none", 
                     alpha = 1,
                     lambda = c(0.1, 0.15, 0.3, 0.45)) {

    nitems <- length(testwords)
        
    predicted <- sapply(testkeys, 
                        function(y) {
                            p <- predictTM(model = fit, 
                                      phrase = y, 
                                      n = n,
                                      method = method,
                                      alpha = alpha,
                                      lambda = lambda)
                            as.character(p$Word)
                            
                        })
    
    predicted <- t(as.matrix(predicted, n))
    predicted[is.na(predicted)] <- "<unk>"
    
    rownames(testkeys) <- NULL
    rownames(testwords) <- NULL
    rownames(predicted) <- NULL
    
    equal <- logical(nitems)
    for (i in 1:n) {
        equal <- (equal | testwords == predicted[, i])
    }
    matches <- table(equal)
    result <- list(predictions = cbind(testkeys, 
                                       testwords, 
                                       predicted,
                                       equal),
                   matches = matches,
                   accuracy = matches["TRUE"]/(matches["TRUE"] + matches["FALSE"]))
    result
}
