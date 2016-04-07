# remove everything from previous sessions
rm(list = ls())

library(quanteda)
library(stringi)
library(data.table)
library(parallel)

############################################
no_cores <- max(1, detectCores() - 1)
# options 
options(mc.cores = no_cores)
#options(datatable.verbose=TRUE)
# number of rows from original docs to use
sample.percent <- 1
# proportion of training set
train.percent <- 0.95

ngrams <- 4

############################################

source(file.path("product","model","utils.R"))
source(file.path("product","model","preprocess.R"))
source(file.path("product","model","train.R"))
source(file.path("product","model","predict.R"))
source(file.path("product","model","test.R"))
source(file.path("product","model","quiz2.R"))

model.path <- file.path("product", "data", "model.rds")
cleansample.path <- file.path("cache", "cleansample.txt")
badwords.path <- file.path("product","data","remove.txt")

############################################

if(!file.exists(cleansample.path)) {
    texts <- readData()
    sampletexts <- getSample(texts, 
                             sample.p = sample.percent)
    
    # cleaning does not remove all punctuation.
    # some of it is used to add tokens.
    cleansample <- cleanData(sampletexts)
    writeLines(cleansample, cleansample.path)
} else {
    cleansample <- readLines(cleansample.path, encoding = "UTF-8", skipNul = TRUE)
}

# get file with bad words
if(file.exists(badwords.path)) {
    badwords <- readLines(badwords.path)
}    

## divide into training and test set
samplelength <- length(cleansample)
inTrain <- sample(1:samplelength, samplelength * train.percent)

train <- cleansample[inTrain]
test <- cleansample[-inTrain]

fit<- trainTM(t = train, 
              trimFeatures = TRUE, 
              minCount = 5, 
              minDoc = 3, 
              smoothingType = "GT", 
              smoothK = 5,
              ngrams = 4,
              delete = badwords)

saveRDS(fit, model.path)

# test
testlist <- makeTestList(test, maxdocs = 500, ngrams = ngrams)
#tr.Ak.trim <- testTM(fit.Ak.trim, testlist, n = 3, maxitems = 1000, ngrams = ngrams, method = "I", l = c(0.1, 0.15, 0.3, 0.45))
tr.GT <- testTM(fit, testlist, n = 3, maxitems = 1000, method = "none")
tr.GTsbo <- testTM(fit, testlist, n = 3, maxitems = 1000, method = "SBO", alpha = 0.4)
tr.GTi <- testTM(fit, testlist, n = 3, maxitems = 1000, method = "I", l = c(0.1, 0.15, 0.3, 0.45))

q2 <- quizTest(fit = fit, testkeys = q2.keys, testwords = q2.words, n = 3, method = "none")
q2sbo <- quizTest(fit = fit, testkeys = q2.keys, testwords = q2.words, n = 3, method = "SBO", alpha = 0.4)
q2i <- quizTest(fit = fit, testkeys = q2.keys, testwords = q2.words, n = 3, method = "I", l = c(0.1, 0.15, 0.3, 0.45))

q3 <- quizTest(fit = fit, testkeys = q3.keys, testwords = q3.words, n = 3, method = "none")
q3sbo <- quizTest(fit = fit, testkeys = q3.keys, testwords = q3.words, n = 3, method = "SBO", alpha = 0.4)
q3i <- quizTest(fit = fit, testkeys = q3.keys, testwords = q3.words, n = 3, method = "I", l = c(0.1, 0.15, 0.3, 0.45))

#predictTM(model = fit, phrase = "see arctic monkeys this", n = 5, method = "I", l = c(0.1, 0.15, 0.3, 0.45))

