# Text prediction - milestone assignment of the capstone project
Evgeniy Zabrodskiy  
18 March 2016  

## Synopsis
The goal of this analysis is to understand the distribution and relationship between the words, tokens, and phrases in the text in order to build a predictive model.
The distributions of frequencies of words, word pairs and word triplets are analised and shown in form of histograms, word clouds and barplots.



## Loading data


Here are the loaded file names, their lenghts in number of rows and total number of words:

```
##            FileName NumberOfRows NumberOfWords
## 1   en_US.blogs.txt       899288      37546246
## 2    en_US.news.txt      1010242      34762395
## 3 en_US.twitter.txt      2360148      30093369
```

## Sampling and cleaning data
The files are quite big (more than 4 millions of rows in total) and building document term matrix using original files is computationally expensive and may take a lot of time. Besides, for the word prediction task we're interested in frequent unigrams, bigrams and trigrams which can be obtained from a smaller sample.  



We randomly selected **10000** rows from each document and all further cleaning and transformations are done using the sample.  



Cleaning procedures and transformations that were applied to the sample are:  
- remove punctuation,  
- remove numbers,  
- convert to lowercase,  
- remove unnecessary whitespaces.  



## Exploratory analysis

### Basic information about the data
Once the data is clean, the document term matrix can be created. It contains the frequncies of terms in each document. The frequencies of terms can be converted to probabilities for prediction modelling at later stages of the project.  

Here is the default output of the document term matrix created from the data sample:  


```
## <<DocumentTermMatrix (documents: 3, terms: 51141)>>
## Non-/sparse entries: 75758/77665
## Sparsity           : 51%
## Maximal term length: 253
## Weighting          : term frequency (tf)
```

From the output we can see some useful information, including total number of terms.  
Here are the lengths of samples, number of words instances, number of terms (unique words) per each document:  


```
##            FileName NumberOfRows NumberOfWords NumberOfTermsPerDocument
## 1   en_US.blogs.txt        10000        311806                    30489
## 2    en_US.news.txt        10000        272044                    30132
## 3 en_US.twitter.txt        10000         96150                    15137
```

### Questions of the analysis
#### 1. Some words are more frequent than others - what are the distributions of word frequencies?  





Here are the histograms to understand the distribution of word frequencies. Due to the distribution properties, it does not look good without transformations and in order to get a better looking histograms, the distribution is shown on the **log scale**.  
The histogram on the left shows the distribition of word frequencies including sparse terms, the histogram on the right shows the same kind of distribution but with sparse terms removed from the matrix.  
We can see that the more frequent the word is (to the right on the histogram), the less number of words with such a high frequency there are.  

![](capstone_ms_files/figure-html/displayHist1-1.png)

Below are the barplots showing unigram terms frequencies per each document (Blogs, News and Twitter) separately and aggregated for all three documents (lower-right graph).  

![](capstone_ms_files/figure-html/displayFreq1-1.png)

For quick perception of the term frequencies one can have a look at the word cloud:  

![](capstone_ms_files/figure-html/displayWC1-1.png)

#### 2. What are the frequencies of 2-grams and 3-grams in the dataset?  
Similar to unigram terms, here are the barplots for bigrams:  

![](capstone_ms_files/figure-html/displayFreq2-1.png)



Same way of presentation for trigram term frequencies. The difference of most frequent terms between different documents (types of sources) is significant especially copared to unigram frequencies above.  

![](capstone_ms_files/figure-html/displayFreq3-1.png)



#### 3. How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?  


Number of frequent words covering half of all word instances in the language: **325**  
Number of frequent words covering 90% of all word instances in the language: **9760**  

#### 4. How do you evaluate how many of the words come from foreign languages?  

One of the ways to identify foreign words is by looking up for a word in a language dictionary. This approach is quite straighforward and has disadvantages such as incorrect classification of misspelled words.  
Another possible way is using machine learning algorithms with language profiles. Similar approach is used in *langid* library.  

#### 5. Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?  

Identifying words that may not be in the corpora can be done by an algorithm which uses external dictionary with linguistic markers such as type of the word (noun, verb, adjective, adverb, etc.) together with some machine learning algorithms that can suggest the word.  
Using a smaller number of words in the dictionary to cover the same number of phrases can be possible by stemming words and suggesting endings based on some algorithm which takes into account grammar rules of the language.  

## References
1. [Text Mining Infrastructure in R](https://www.jstatsoft.org/article/view/v025i05)
2. [Basic Text Mining in R](https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html)
3. [Natural Language Processing](https://www.coursera.org/course/nlp)
