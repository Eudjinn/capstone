# remove everything from previous sessions
rm(list = ls())

library(parallel)
library(quanteda)
library(stringi)
library(data.table)
library(tm)
library(randomForest)

############################################
# options 
options(mc.cores = max(1, detectCores()))
# number of rows from original docs to use
sample.percent <- 0.5
# proportion of training set
train.percent <- 0.7

ngrams <- 5
############################################

source("utils.R")
source("predict_q.R")
source("test.R")

############################################

texts <- readData()
sampletexts <- getSample(texts, 
                         sample.p = sample.percent)

cleansample <- cleanData(sampletexts)
## divide into training and test set
samplelength <- length(cleansample)
inTrain <- sample(1:samplelength, samplelength * train.percent)

train <- cleansample[inTrain]
test <- cleansample[-inTrain]

# test full training cycle
fit.Ak <- trainTM(t = train, 
                  smoothingType = "Ak", 
                  smoothK = 1)
#fit.GT <- trainTM(t = train, 
#                  smoothingType = "GT", 
#                  smoothK = 1, 
#                  lambda = c(0.02, 0.08, 0.1, 0.3, 0.5))
fit.Ak.trim <- trainTM(t = train, 
                       trimFeatures = TRUE, 
                       minCount = 2, 
                       minDoc = 2, 
                       smoothingType = "Ak", 
                       smoothK = 1)
#fit.GT.trim <- trainTM(t = train, 
#                       trimFeatures = TRUE, 
#                       minCount = 3, 
#                       minDoc = 2, 
#                       smoothingType = "GT", 
#                       smoothK = 1, 
#                       lambda = c(0.02, 0.08, 0.1, 0.3, 0.5))

# remove end of sentence chars after model was trained.
train <- cleanEnds(train)
test <- cleanEnds(test)

# test
testlist <- makeTestList(test, maxdocs = 500, ngrams = 5)
#tr.Ak <- testTM(fit.Ak, testlist, n = 3, maxitems = 1000, ngrams = 5)
#tr.GT <- testTM(fit.GT, testlist, n = 3, maxitems = 1000, ngrams = 5)
#tr.Aki <- testTM(fit.Ak, testlist, n = 3, maxitems = 1000, ngrams = 5, interpolate = TRUE)
#tr.GTi <- testTM(fit.GT, testlist, n = 3, maxitems = 1000, ngrams = 5, interpolate = TRUE)

#tr.Ak.trim <- testTM(fit.Ak.trim, testlist, n = 3, maxitems = 1000, ngrams = 5)
#tr.GT.trim <- testTM(fit.GT.trim, testlist, n = 3, maxitems = 1000, ngrams = 5)
#tr.Aki.trim <- testTM(fit.Ak.trim, testlist, n = 3, maxitems = 1000, ngrams = 5, interpolate = TRUE)
#tr.GTi.trim <- testTM(fit.GT.trim, testlist, n = 3, maxitems = 1000, ngrams = 5, interpolate = TRUE)

# trbo <- testTM(fit, testlist, n = 3, maxitems = 500, ngrams = 5, a = c(1,1,1,1,1))

for(l1 in seq(0.005, 0.1, 0.005))
    for(l2 in seq(0.095, 0.15, 0.005))
        for(l3 in seq(0.01, 0.3, 0.005))
            for(l4 in seq(0.05, 0.3, 0.05))
                for(l5 in seq(0.05, 0.5, 0.05)) {
                    l <- c(l1, l2, l3, l4, l5)
                    if(sum(l) == 1) {
                        cat(l, "\n")
#                        tr <- testTM(fit.Ak, testlist, n = 3, maxitems = 100, interpolate = TRUE, l = c(l1, l2, l3, l4, l5))
                        tr <- quiz2s(fit.Ak, interpolate = TRUE, l = c(l1, l2, l3, l4, l5))
                        cat("Interpolate:", l, "Accuracy:", tr$accuracy, "\n")
                        cat("Predictions:", rbind(tr$predictions[,2], tr$predictions[,6]), "\n")
                        s <- paste("Interpolate:", paste(l, collapse = ", "), "Accuracy:", tr$accuracy)
                        write(s, "quiztest.txt", append = TRUE, sep = "\r\n")
                    }
                }
#for(a4 in seq(1, 0.1, -0.1))
 #   for(a3 in seq(1, 0.1, -0.1))
  #      for(a2 in seq(1, 0.1, -0.1)){
   #         aa <- c(0.05, a2, a3, a4, 1)
    #        trbo <- testTM(fit, testlist, n = 3, maxitems = 200, a = aa)
     #       cat("Backoff:", aa, "Accuracy:", trbo$accuracy, "\n")
      #  }
#trbo <- testTM(fit, testlist, n = 3, maxitems = 500, a = c(0.05,0.4,0.4,0.4,1))

#predictTM(fit, "going to be here", n = 1)

# system.time(predictTMbo(fit, "going to be here", n = 1, a = c(0.05,0.4,0.4,0.4,1)))

