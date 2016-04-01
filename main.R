# remove everything from previous sessions
rm(list = ls())

library(parallel)
library(quanteda)
library(stringi)
library(data.table)
library(tm)
library(randomForest)

############################################
no_cores <- max(1, detectCores() - 1)
# options 
options(mc.cores = no_cores)
#options(datatable.verbose=TRUE)
# number of rows from original docs to use
sample.percent <- 0.01
# proportion of training set
train.percent <- 0.7
#parallel processing
parallel = TRUE

ngrams <- 4

############################################

source("utils.R")
source("predict_q.R")
source("test.R")
source("quiz2.R")

############################################

texts <- readData()
sampletexts <- getSample(texts, 
                         sample.p = sample.percent)

# cleaning does not remove all punctuation.
# some of it is used to add tokens.
cleansample <- cleanData(sampletexts)
## divide into training and test set
samplelength <- length(cleansample)
inTrain <- sample(1:samplelength, samplelength * train.percent)

train <- cleansample[inTrain]
test <- cleansample[-inTrain]

fit.Ak.trim <- trainTM(t = train, 
                       trimFeatures = TRUE, 
                       minCount = 3, 
                       minDoc = 2, 
                       smoothingType = "Ak", 
                       smoothK = 1,
                       ngrams = 4)

# remove end of sentence chars after model was trained.
train <- cleanEnds(train)
test <- cleanEnds(test)

# test
testlist <- makeTestList(test, maxdocs = 500, ngrams = ngrams)
tr.Ak.trim <- testTM(fit.Ak.trim, testlist, n = 3, maxitems = 1000, ngrams = ngrams, interpolate = TRUE, l = c(0.1, 0.15, 0.3, 0.45))

q2 <- quizTest(fit = fit.Ak.trim, testkeys = q2.keys, testwords = q2.words, n = 3, ngrams = 4, interpolate = FALSE, l = c(0.005, 0.095, 0.1, 0.8))
q3 <- quizTest(fit = fit.Ak.trim, testkeys = q3.keys, testwords = q3.words, n = 3, ngrams = 4, interpolate = FALSE, l = c(0.005, 0.095, 0.1, 0.8))

predictTM(model = fit.Ak.trim, phrase = "see arctic monkeys this", n = 5, ngrams = 4, interpolate = TRUE, l = c(0.0005, 0.1495, 0.35, 0.5))

