# legasy function, will not work, needs to be fixed.
quiz2 <- function(fit) {
    predicted <- data.table(rbind(
        predictTM(fit, "The guy in front of me just bought a pound of bacon, a bouquet, and a case of", n = 5),
        predictTM(fit, "You're the reason why I smile everyday. Can you follow me please? It would mean the", n = 5),
        predictTM(fit, "Hey sunshine, can you follow me and make me the", n = 5),
        predictTM(fit, "Very early observations on the Bills game: Offense still struggling but the", n = 5),
        predictTM(fit, "Go on a romantic date at the", n = 5),
        predictTM(fit, "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", n = 5),
        predictTM(fit, "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", n = 5),
        predictTM(fit, "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", n = 5),
        predictTM(fit, "Be grateful for the good times and keep the faith during the", n = 5),
        predictTM(fit, "If this isn't the cutest thing you've ever seen, then you must be", n = 5))
    )
    probs <- data.table(rbind(
        probTM(fit, "The guy in front of me just bought a pound of bacon, a bouquet, and a case of", "pretzels"),
        probTM(fit, "The guy in front of me just bought a pound of bacon, a bouquet, and a case of", "beer"),
        probTM(fit, "The guy in front of me just bought a pound of bacon, a bouquet, and a case of", "cheese"),
        probTM(fit, "The guy in front of me just bought a pound of bacon, a bouquet, and a case of", "soda"),
        
        probTM(fit, "You're the reason why I smile everyday. Can you follow me please? It would mean the", "best"),
        probTM(fit, "You're the reason why I smile everyday. Can you follow me please? It would mean the", "universe"),
        probTM(fit, "You're the reason why I smile everyday. Can you follow me please? It would mean the", "world"),
        probTM(fit, "You're the reason why I smile everyday. Can you follow me please? It would mean the", "most"),
        
        probTM(fit, "Hey sunshine, can you follow me and make me the", "saddest"),
        probTM(fit, "Hey sunshine, can you follow me and make me the", "bluest"),
        probTM(fit, "Hey sunshine, can you follow me and make me the", "happiest"),
        probTM(fit, "Hey sunshine, can you follow me and make me the", "smelliest"),
        
        probTM(fit, "Very early observations on the Bills game: Offense still struggling but the", "crowd"),
        probTM(fit, "Very early observations on the Bills game: Offense still struggling but the", "defense"),
        probTM(fit, "Very early observations on the Bills game: Offense still struggling but the", "referees"),
        probTM(fit, "Very early observations on the Bills game: Offense still struggling but the", "players"),
        
        probTM(fit, "Go on a romantic date at the", "beach"),
        probTM(fit, "Go on a romantic date at the", "grocery"),
        probTM(fit, "Go on a romantic date at the", "movies"),
        probTM(fit, "Go on a romantic date at the", "mall"),
        
        probTM(fit, "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", "way"),
        probTM(fit, "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", "phone"),
        probTM(fit, "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", "horse"),
        probTM(fit, "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", "motorcycle"),
        
        probTM(fit, "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", "years"),
        probTM(fit, "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", "thing"),
        probTM(fit, "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", "time"),
        probTM(fit, "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", "weeks"),
        
        probTM(fit, "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", "eyes"),
        probTM(fit, "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", "ears"),
        probTM(fit, "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", "toes"),
        probTM(fit, "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", "fingers"),
        
        probTM(fit, "Be grateful for the good times and keep the faith during the", "bad"),
        probTM(fit, "Be grateful for the good times and keep the faith during the", "hard"),
        probTM(fit, "Be grateful for the good times and keep the faith during the", "worse"),
        probTM(fit, "Be grateful for the good times and keep the faith during the", "sad"),
        
        probTM(fit, "If this isn't the cutest thing you've ever seen, then you must be", "asleep"),
        probTM(fit, "If this isn't the cutest thing you've ever seen, then you must be", "insensitive"),
        probTM(fit, "If this isn't the cutest thing you've ever seen, then you must be", "insane"),
        probTM(fit, "If this isn't the cutest thing you've ever seen, then you must be", "callous"))
    )
    result <- list(predicted = predicted,
                   predictedbo = predictedbo,
                   probs = probs)
    result
}

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
              "2",
              "3",
              "stress",
              "picture",
              "6",
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
