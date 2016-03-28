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
sample.percent <- 0.01
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

fit <- trainTM(train, trimFeatures = FALSE, smoothingType = "Ak", smoothK = 1)

# remove end of sentence chars after model was trained.
train <- cleanEnds(train)
test <- cleanEnds(test)

# test
testlist <- makeTestList(test, maxdocs = 100, ngrams = 5)
tr <- testTM(fit, testlist, n = 3, maxitems = 500, ngrams = 5)
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

