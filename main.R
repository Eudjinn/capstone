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
sample.percent <- 0.05
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
fit.Ak <- trainTM(t = train, smoothingType = "Ak", smoothK = 1)
fit.GT <- trainTM(t = train, smoothingType = "GT", smoothK = 1)
fit.Ak.trim <- trainTM(t = train, trimFeatures = TRUE, minCount = 3, minDoc = 2, smoothingType = "Ak", smoothK = 1)
fit.GT.trim <- trainTM(t = train, trimFeatures = TRUE, minCount = 3, minDoc = 2, smoothingType = "GT", smoothK = 1)

# remove end of sentence chars after model was trained.
train <- cleanEnds(train)
test <- cleanEnds(test)

# test
testlist <- makeTestList(test, maxdocs = 500, ngrams = 5)
tr.Ak <- testTM(fit.Ak, testlist, n = 3, maxitems = 5000, ngrams = 5)
tr.GT <- testTM(fit.GT, testlist, n = 3, maxitems = 5000, ngrams = 5)
tr.Ak.trim <- testTM(fit.Ak.trim, testlist, n = 3, maxitems = 5000, ngrams = 5)
tr.GT.trim <- testTM(fit.GT.trim, testlist, n = 3, maxitems = 5000, ngrams = 5)

# trbo <- testTM(fit, testlist, n = 3, maxitems = 500, ngrams = 5, a = c(1,1,1,1,1))

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

