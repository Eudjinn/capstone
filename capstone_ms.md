# Text prediction - milestone assignment of the capstone project
Evgeniy Zabrodskiy  
8 March 2016  

# Synopsis
The goal of this analysis is to understand the distribution and relationship between the words, tokens, and phrases in the text in order to build a predictive model.
The distributions of frequencies of words, word pairs and word triplets are analised and shown in form of word clouds and barplots.

## Initialization


## Loading data

```
##                   Length Class             Mode
## en_US.blogs.txt   2      PlainTextDocument list
## en_US.news.txt    2      PlainTextDocument list
## en_US.twitter.txt 2      PlainTextDocument list
```

Here are the lengths of files in number of rows:

```
##            FileName NumberOfRows
## 1   en_US.blogs.txt       899288
## 2    en_US.news.txt      1010242
## 3 en_US.twitter.txt      2360148
```

Since the files are pretty big (more than 4 million rows in total), words count would be computationally expensive, expecially taking into account that the documents should be cleaned first. For the text prediction algorithms we'll use smaller sample size which will be described below.  

## Sampling and cleaning data






## Exploratory analysis

### Basic information about the data


```
## <<DocumentTermMatrix (documents: 3, terms: 51682)>>
## Non-/sparse entries: 76681/78365
## Sparsity           : 51%
## Maximal term length: 69
## Weighting          : term frequency (tf)
```

From the output we can see some useful information, including total number of terms.  
Here are the lengths of samples, number of words instances, number of terms (unique words) per each document:  

```
##            FileName NumberOfRows NumberOfWords NumberOfTermsPerDocument
## 1   en_US.blogs.txt        10000        319063                    31141
## 2    en_US.news.txt        10000        270902                    30419
## 3 en_US.twitter.txt        10000         95707                    15121
```

### 1. Some words are more frequent than others - what are the distributions of word frequencies?  



Below is the barplots showing unigram terms frequencies per each document (Blogs, News and Twitter) separately and aggregated for all three documents (lower-right graph).  

![](capstone_ms_files/figure-html/displayFreq1-1.png)

For quick perception of the terms frequencies one can have a look at words cloud:  

![](capstone_ms_files/figure-html/displayWC1-1.png)

### 2. What are the frequencies of 2-grams and 3-grams in the dataset?  
Similar to unigram terms, here are the barplots for bigrams:  

![](capstone_ms_files/figure-html/displayFreq2-1.png)



Same way of presentation for trigram term frequencies. The difference of most frequent terms between different documents (types of sources) is significant especially copared to unigram frequencies above.  

![](capstone_ms_files/figure-html/displayFreq3-1.png)



### 3. How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?  


Number of frequent words covering half of the language: **330**  
Number of frequent words covering 90% of the language: **9960**  

### 4. How do you evaluate how many of the words come from foreign languages?  

One of the ways to identify foreign words is by looking up for a word in a language dictionary. This approach is quite straighforward and has disadvantages such as incorrect classification of misspelled words.  
Another possible way is using machine learning algorithms with language profiles. This approach is used in *langid* library.  

### 5. Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?  

One of the ways of identifying words that may not be in the corpora can be possible if there is an external dictionary with linguistic markers such as type of the word (noun, verb, adjective, adverb, etc.)  
Using a smaller number of words in the dictionary to cover the same number of phrases can be done by stemming words and suggesting endings based on some algorithm.

## References

https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html

https://deltadna.com/blog/text-mining-in-r-for-term-frequency/

## Appendix
