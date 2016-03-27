# remove everything from previous sessions
rm(list = ls())

library(parallel)
library(quanteda)
library(stringi)
library(data.table)
library(tm)

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

fit <- trainTM(train, trimFeatures = TRUE, smoothK = 1)

# remove end of sentence chars after model was trained.
train <- cleanEnds(train)
test <- cleanEnds(test)

# test
testlist <- makeTestList(test, maxdocs = 100)
tr <- testTM(fit, testlist, n = 3, maxitems = 500)
trbo <- testTM(fit, testlist, n = 3, maxitems = 500, a = c(0.05,0.4,0.4,0.4,1))

predictTM(fit, "going to be here", n = 1)

system.time(predictTMbo(fit, "going to be here", n = 1, a = c(0.05,0.4,0.4,0.4,1)))

