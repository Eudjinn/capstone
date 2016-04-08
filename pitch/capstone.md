Data Science Capstone Project: Word Prediction
========================================================
author: Evgeniy Zabrodskiy 
date: 8th of April, 2016 

Task description
========================================================
The goal of the project was to develop Natural Language Processing application for word prediction.
It involved the following tasks:
- Learning algorithms and methods used in NLP for word prediction;
- Exploring data set to understand what real data looks like;
- Building the text model using N-Grams of several orders;
- Run a series of model training/testing to achieve higher word prediction accuracy;
- Develop Shiny application with word prediction capability using trained text model;
- Present the results.

Natural Language Processing
========================================================
A model with 4-grams, 3-grams, bigrams and unigrams was built.  

**Creating the text model involved the following processing steps:**  

1. Loading and cleaning text to build and train the model;  
2. Tokenizing documents to get counts for n-grams (from 1 to 4);  
3. Calculating probabilities of tokens in each n-gram;  
4. Applying smoothing (Good-Turing).

**Prediction:**  
- Stupid Backoff (switching to lower-level n-gram if a word wasn't found using n-grams of higher order)
- Interpolation (combining results from n-grams of different orders with coefficients)


Tools and packages
========================================================
For development of the text model the following tools and packages were used:
- RStudio/R
- quanteda package (building document term matixes)
- data.table (main storage for the n-gram model)
- stringi (for some operations with strings)

Tried to use but droped during development:
- tm package (tokenizing is extremely slow)
- parallel (encountered issues with parallel processing, perhaps due to other packages incompatibility)  

The Shiny Word Prediction Application
========================================================
The application allows one to choose the prediction method (Stupid Backoff/Interpolation). Both methods give the same predictions most of the time but Interpolation is computationally more expensive. 
Each method has its own settings:  
- Stupid backoff allows one to change **alpha** coefficient which does not affect the prediction result in most cases;
- Interpolation doesn't allow to change anything for simplicity but shows which **lambda** coefficients are used.
Aslo user is free to choose the number of words he wants to see in prediction results from 1 to 10.  

When user enters a sentence in the input text box, the application looks for the word that may follow the sentence. The first prediction is added to the sentence and shown below the input text box in red.  

Conclusions
========================================================
**Lessons learned:**
- Cleaning data is very important. Different NLP algorithms did not affect the accuracy as much as cleaning the training data;  
- **data.table** is very fast and efficient data structure in R;

**What needs to be done to improve the model:**
- Try different backoff strategies like Katz Backoff;  
- Try Kneser-Ney smoothing;
- Try combination of different smoothing methods with interpolation;
- Find ways to reduce the size of the model.