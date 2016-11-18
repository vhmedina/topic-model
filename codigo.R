# Packages
library(readr)
library(tm)
library(topicmodels)

# Load texts 
data_latercera <- read_csv("data_latercera.csv", 
                           locale = locale(encoding = "UTF8"))

# Transform the text to corpus
news=Corpus(VectorSource(data_latercera$contenido))

# Checking
as.character(news[[22]])

## Some preprocessing transformations
# To lower case
news=tm_map(news,content_transformer(tolower))
# Discard punctuation
news=tm_map(news,removePunctuation)
# comillas
news=tm_map(news,chartr,old='“',new=' ')
news=tm_map(news,chartr,old='”',new=' ')
# The numbers
news = tm_map(news, removeNumbers)
# the stopwords
news=tm_map(news, removeWords, stopwords("spanish"))
# The whitespace
news=tm_map(news, stripWhitespace)
# Step needed before stemming
news = tm_map(news, PlainTextDocument)
# Stemming
news=tm_map(news, stemDocument, "spanish")

# Checking
as.character(news[[22]])

## Now that we have processed the texts we can create the document term matrix
dtmatrix=DocumentTermMatrix(news)

## We may have rows with empty temrs, so we delete them
total_row= apply(dtmatrix , 1, sum) 
dtmatrix=dtmatrix[total_row> 0, ]           #remove all docs without words

## Frequency of terms
freq = colSums(as.matrix(dtmatrix))

# See which terms are the most frequent
ord = order(freq,decreasing=TRUE)
head(freq[ord])

##### Topic modeling
# Setting parameters for Gibbs sampling
k= 50 # Number of topics
seed= 2^(5:14) # seed
nstart= 10 # Number of repeated random starts. 
best= TRUE # if TRUE only the model with the maximum (posterior) likelihood is returned
iter= 1000 # number of Gibbs iterations, by default equals 2000
thin= 1000 # number of omitted in-between Gibbs iterations, by default equals iter.
burnin=1000 #  number of omitted Gibbs iterations at beginning, by default equals 0

# LDA using Gibbs sampling
results=LDA(dtmatrix,k, method="Gibbs", 
            control=list(seed = seed,nstart=nstart, 
                         best=best, 
                         iter = iter, 
                         thin=thin,
                         burnin = burnin))

