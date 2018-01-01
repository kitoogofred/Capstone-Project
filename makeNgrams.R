## Building the n-Grams and Frequencies
library(tm)
library(slam)
library(qdap)
library(wordcloud)
library(RWekajars)
library(RWeka)
library(rJava)
# library(data.table)
# library(kableExtra)
library(stringr)
library(stringi)

## Read the Data Files
file.list = c("final/en_US/en_US.blogs.txt", "final/en_US/en_US.news.txt", "final/en_US/en_US.twitter.txt")
text <- list(blogs = "", news = "", twitter = "")

en_US.summary <- matrix(0, nrow = 3, ncol = 3, dimnames = list(c("Blogs", "News", "Twitter"),c("File size, Mb", "No. of Lines", "No. of Words")))
for (i in 1:3) {
  con <- file(file.list[i], "rb")
  text[[i]] <- readLines(con, encoding = "UTF-8",skipNul = TRUE)
  close(con)
  en_US.summary[i,1] <- round(file.info(file.list[i])$size / 1024^2, 2)
  en_US.summary[i,2] <- length(text[[i]])
  en_US.summary[i,3] <- sum(stri_count_words(text[[i]]))
}

# kable(en_US.summary, "html", caption = "Engilsh Data Sets Summary") %>%
#   kable_styling(bootstrap_options = c("striped", "hover"))

set.seed(12345)

## Get population sizes (lines)
blogs_pop <- length(text$blogs)
news_pop <- length(text$news)
twitter_pop <- length(text$twitter)

## Create the sample 
blogs_sample <- sample(text$blogs, 0.01*length(text$blogs))
news_sample <- sample(text$news, 0.01*length(text$news))
twitter_sample <- sample(text$twitter, 0.01*length(text$twitter))
sample_corpus_data <- c(blogs_sample, news_sample, twitter_sample)

## Write the file to disc
writeLines(sample_corpus_data,
           "final/sample_corpus_data.txt")

# sample_corpus_data <- 
#   readLines("final/sample_corpus_data.txt")

corpusNew <- VCorpus(VectorSource
            (as.data.frame(sample_corpus_data,
            na.rm=TRUE, stringsAsFactors = FALSE))) 

## Functions for preprocessing the Corpous
removeURL<-function(x) gsub("http[[:alnum:]]*","",x)
removeSign<-function(x) gsub("[[:punct:]]","",x)
removeNum<-function(x) gsub("[[:digit:]]","",x)
removeapo<-function(x) gsub("'","",x)
removeNonASCII<-function(x) iconv(x, "latin1", "ASCII", sub="")
removerepeat<- function(x) gsub("([[:alpha:]])\\1{2,}", "\\1\\1", x)
toLowerCase <- function(x) sapply(x,tolower)
removeSpace<-function(x) gsub("\\s+"," ",x)
removeTh<-function(x) gsub(" th", "",x)

## Funtion to clean the Corpus
clean_corpus <- function(corpus){
  corpus<-tm_map(corpus,content_transformer(removeapo))#remove apostrophe
  corpus<-tm_map(corpus,content_transformer(removeNum))#remove numbers
  corpus<-tm_map(corpus,content_transformer(removeURL)) #remove web url
  corpus<-tm_map(corpus,content_transformer(removeSign)) #remove number and punctuation except apostrophe
  corpus<-tm_map(corpus,content_transformer(removeNonASCII)) #remove non-ASCII
  corpus<-tm_map(corpus,content_transformer(toLowerCase))# convert uppercase to lowercase
  corpus<-tm_map(corpus,content_transformer(removerepeat))# remove repeated alphabets in a words
  # corpus<-tm_map(corpus,removeWords,c(stopwords("english"), "bv", "e","b","c","d","ha","blah")) #remove common english words and letters
  corpus<-tm_map(corpus,content_transformer(removeTh)) #remove th from words
  corpus<-tm_map(corpus,content_transformer(removeSpace)) #remove multiple space
}

## Clean the Corpus
corpusNew <- clean_corpus(corpusNew)

## Tokenize
### Unigram Tokenizer
Uni_Tokenizer <- function(x){
  NGramTokenizer(x, Weka_control(min = 1, max = 1))
}
corpus_Uni_tdm <- TermDocumentMatrix(corpusNew, 
                  control = list(tokenize = Uni_Tokenizer))
### Bigram Tokenizer
Bi_Tokenizer <- function(x){
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

corpus_Bi_tdm <- TermDocumentMatrix(corpusNew,
                 control = list(tokenize = Bi_Tokenizer))
### Trigram Tokenizer
Tri_Tokenizer <- function(x){
  NGramTokenizer(x, Weka_control(min = 3, max = 3))
}

corpus_Tri_tdm <- TermDocumentMatrix(corpusNew,
                  control = list(tokenize = Tri_Tokenizer))
### Tetragram Tokenizer
Tet_Tokenizer <- function(x){
  NGramTokenizer(x, Weka_control(min = 4, max = 4))
}

corpus_Tet_tdm<- TermDocumentMatrix(corpusNew, control = list(tokenize = Tet_Tokenizer))

# function to filter information and create a data frame with the most common (highest frequency) words
freq_df <- function(tdm){
  freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
  freq_df <- data.frame(word=names(freq), freq=freq)
  return(freq_df)
}

## nGram Frequencies
freqUni <- rowapply_simple_triplet_matrix(corpus_Uni_tdm,sum)
freqBi <- rowapply_simple_triplet_matrix(corpus_Bi_tdm,sum)
freqTri <- rowapply_simple_triplet_matrix(corpus_Tri_tdm,sum)
freqTet <- rowapply_simple_triplet_matrix(corpus_Tet_tdm,sum)

firstname <- sapply(strsplit(names(freqBi), ' '), function(a) a[1])
secname <- sapply(strsplit(names(freqBi), ' '), function(a) a[2])
firsttriname <- sapply(strsplit(names(freqTri), ' '),function(a) a[1])
sectriname <- sapply(strsplit(names(freqTri), ' '),function(a) a[2])
tritriname <- sapply(strsplit(names(freqTri), ' '),function(a) a[3])
firsttetname <- sapply(strsplit(names(freqTet), ' '),function(a) a[1])
sectetname <- sapply(strsplit(names(freqTet), ' '),function(a) a[2])
tritetname <- sapply(strsplit(names(freqTet), ' '),function(a) a[3])
tettetname <- sapply(strsplit(names(freqTet), ' '),function(a) a[4])

# Generate the final n-gram dataframe 
## Has the ngram, frequency, split middle words in the ngram
## the last word in the ngram
unigramDF <- data.frame(names(freqUni),freqUni,
                        stringsAsFactors = F)
bigramDF <- data.frame(names(freqBi),freqBi,firstname,
                       secname,stringsAsFactors = F)
trigramDF <- data.frame(names(freqTri),
                        freqTri,paste(firsttriname,sectriname),
                        tritriname,stringsAsFactors = F)
tetgramDF <- data.frame(names(freqTet),
                        freqTet,paste(firsttetname, sectetname,tritetname),
                        tettetname,stringsAsFactors = F)

names(unigramDF) <- c('unigram','freq')
names(bigramDF) <- c('bigram','freq','unigram','name')
names(trigramDF) <- c('trigram','freq','bigram','name')
names(tetgramDF) <- c('tetgram','freq','trigram','name')
save(unigramDF,bigramDF,trigramDF, tetgramDF,
     file = 'final/ngram.RData')

