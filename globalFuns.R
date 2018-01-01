# R Scipt loading data and also functions
library(data.table)
library(stringr)
library(stringi)
library(tm)
# library(slam)
# library(quanteda)

## Function to clean the input text
cleanInput <- function(input) { 
  
  input <- tolower(input)
  input <- gsub("[^[:alnum:][:space:]\']", "",input)
  input <- removePunctuation(input)
  input <- removeNumbers(input)
  input <- gsub("\\s+$", "", input)
  input <- rev(unlist(strsplit(input," ")))
  input <- paste(input[2],input[1],sep = ' ')
  return(input)
}
# Back-off Katz Prediction Model

## Here, When trying to find the probability of a word
## appearing in a sentence it will first look for 
## context for the word at the n-gram level and 
## if there is no n-gram of that size it will recurse
## to the (n-1)-gram and multiply its score 
## with 0.4. The recursion stops at unigrams. 

predBKO <-function(input,badwords,unigramDF, bigramDF, trigramDF, maxResults = 3) {
  ## Clean the input
  input <- cleanInput(input)
  
  if(stri_isempty(input)|input == "NA"|is.null(input)) 
    return('Warning: Just input something')
  
  seektri<-grepl(paste0("^",input,"$"),trigramDF$bigram)
  subtri<-trigramDF[seektri,]
  input2 <- unlist(strsplit(input," "))[2]
  seekbi <- grepl(paste0("^",input2,"$"),bigramDF$unigram)
  subbi <- bigramDF[seekbi,]
  unigramDF$s <- unigramDF$freq/nrow(unigramDF)*0.16
  useuni <- unigramDF[order(unigramDF$s,decreasing = T),]
  useunia <- useuni[1:maxResults,]
  
  if (sum(seektri) == 0) {
    if(sum(seekbi)==0){
      return(head(unigramDF[order(unigramDF$freq,decreasing = T),1],
                  maxResults))
    }
    subbi$s <- 0.4*subbi$freq/sum(seekbi)
    names <- c(subbi$name,useunia$unigram)
    score <- c(subbi$s,useunia$s)
    predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
    predictWord <- predictWord[order(predictWord$score,decreasing = T),]
    # in case replicated
    final <- unique(predictWord$next_word)
    final <- setdiff(final,badwords)
    final <- final[grepl('[[:alpha:]]',final)]
    return(final[1:maxResults])
  } 
  subbi$s <- 0.4*subbi$freq/sum(seekbi)
  subtri$s <- subtri$freq/sum(subtri$freq)
  names <- c(subtri$name,subbi$name,useunia$unigram)
  score <- c(subtri$s,subbi$s,useunia$s)
  predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
  predictWord <- predictWord[order(predictWord$score,decreasing = T),]
  # in case replicated
  final <- unique(predictWord$next_word)
  final <- final[1:maxResults]
  final <- setdiff(final,badwords)
  final <- final[grepl('[[:alpha:]]',final)]        
  return(final)
}

## This Back-off Katz Smoothing Model uses Tetragrams
## to be completed##

predBKO4 <-function(input,badwords,unigramDF, bigramDF,
              trigramDF, tetragramDF, maxResults = 3) {
  ## Clean the input
  input <- cleanInput(input)
  
  if(input == ''|input == "na na") return('Warning: Just input something')
  
  seektri<-grepl(paste0("^",input,"$"),trigramDF$bigram)
  subtri<-trigramDF[seektri,]
  input2 <- unlist(strsplit(input," "))[2]
  seekbi <- grepl(paste0("^",input2,"$"),bigramDF$unigram)
  subbi <- bigramDF[seekbi,]
  unigramDF$s <- unigramDF$freq/nrow(unigramDF)*0.16
  useuni <- unigramDF[order(unigramDF$s,decreasing = T),]
  useunia <- useuni[1:maxResults,]
  
  if (sum(seektri) == 0) {
    if(sum(seekbi)==0){
      return(head(unigramDF[order(unigramDF$freq,decreasing = T),1],
                  maxResults))
    }
    subbi$s <- 0.4*subbi$freq/sum(seekbi)
    names <- c(subbi$name,useunia$unigram)
    score <- c(subbi$s,useunia$s)
    predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
    predictWord <- predictWord[order(predictWord$score,decreasing = T),]
    # in case replicated
    final <- unique(predictWord$next_word)
    final <- setdiff(final,badwords)
    final <- final[grepl('[[:alpha:]]',final)]
    return(final[1:maxResults])
  } 
  subbi$s <- 0.4*subbi$freq/sum(seekbi)
  subtri$s <- subtri$freq/sum(subtri$freq)
  names <- c(subtri$name,subbi$name,useunia$unigram)
  score <- c(subtri$s,subbi$s,useunia$s)
  predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
  predictWord <- predictWord[order(predictWord$score,decreasing = T),]
  # in case replicated
  final <- unique(predictWord$next_word)
  final <- final[1:maxResults]
  final <- setdiff(final,badwords)
  final <- final[grepl('[[:alpha:]]',final)]        
  return(final)
}



# Kneser-Ney Smoothing Model 
## Function for Bigrams
predictKN2 <- function(input,badwords,D2,P,subbi,cw2,nw2,unigram,bigram,maxResults = 3){
  # kick off to unigram if no bigram
  if(nw2 == 0) {
    return(head(unigram[order(unigram$freq,decreasing = T),1],maxResults))
  }
  cp <- unique(subbi$name)
  pkn <- rep(NA,length(cp))
  for(i in 1:length(cp)){
    # get nw3 cw3 for smooth
    nw3 <- sum(grepl(cp[i],bigram$name))
    cw3 <- subbi[subbi$name == cp[i],2]
    pkn[i] <- max((cw3-D2),0)/cw2 + P*nw3
  }
  predictWord <- data.frame(next_word=cp,probability=pkn,stringsAsFactors=FALSE)
  predictWord <- predictWord[order(predictWord$probability,decreasing = T),]
  final <- predictWord$next_word[!is.na(predictWord$next_word)]
  final <- final[1:maxResults]
  final <- unique(final)
  final <- setdiff(final,badwords)
  final <- final[grepl('[[:alpha:]]',final)]        
  return(final)
}

## Function for Trigrams
predictKN <- function(input,badwords,unigramDF,bigramDF,trigramDF,maxResults = 3){
  # Generate freqencies of n-grams for smoothing
  uni.freqfreq <- data.frame(uni=table(unigramDF$freq))
  bi.freqfreq <- data.frame(Bi=table(bigramDF$freq))
  tri.freqfreq <- data.frame(Tri=table(trigramDF$freq))
  
  # get Distributions by Ney et al. by the total 
  ## number of n-grams occurring exactly once 
  ## (n1) and twice (n2)
  D1 <- uni.freqfreq[1,2]/(uni.freqfreq[1,2]+2*uni.freqfreq[2,2])
  D2 <- bi.freqfreq[1,2]/(bi.freqfreq[1,2]+2*bi.freqfreq[2,2])
  D3 <- tri.freqfreq[1,2]/(tri.freqfreq[1,2]+2*tri.freqfreq[1,2])
  
  ## Clean the input
  input <- cleanInput(input)
  
  if(input == ''|input == "na na") return('WARNING: Just input something')
  input2 <- unlist(strsplit(input," "))[2]
  
  seekcw1w2<-grepl(paste0("^",input,"$"),trigramDF$bigram)
  subtri<-trigramDF[seekcw1w2,]
  cw1w2 <- sum(subtri$freq)
  nw1w2 <- sum(seekcw1w2)
  seekW2<-grepl(paste0(input2,"$"),trigramDF$bigram)
  W2 <- sum(seekW2)
  p3 <- D3*nw1w2/cw1w2
  
  seekcw2 <- grepl(input2,bigramDF$unigram)
  subbi <- bigramDF[seekcw2,]
  cw2 <- sum(subbi$freq)
  nw2 <- sum(seekcw2)
  nw <- nrow(bigramDF)
  p2 <- D3*nw2/cw2/nw
  p1 <- D2*nw2/cw2/nw
  if(cw1w2 == 0){
    
    return(predictKN2(input2,badwords,D2,p1,subbi,cw2,nw2,unigramDF,bigramDF,maxResults = maxResults))
  }
  cp <- unique(subbi$name)
  pkn <- rep(NA,length(cp))
  for(i in 1:length(cp)){
    # get nw3 nw2w3 and cw1w2w3 for smooth
    nw3 <- sum(grepl(cp[i],bigramDF$name))
    nw2w3 <- sum(grepl(paste0(input2,' ',cp[i],'$'),trigramDF$trigram))
    cw1w2w3 <- subtri[subtri$name == cp[i],2]
    pkn[i] <- max((cw1w2w3-D3),0)/cw1w2 + p3*(max((nw2w3-D3),0)/W2+ p2*nw3)
  }
  predictWord <- data.frame(next_word=cp,probability=pkn,stringsAsFactors=FALSE)
  predictWord <- predictWord[order(predictWord$probability,decreasing = T),]
  final <- predictWord$next_word[!is.na(predictWord$next_word)]
  final <- final[1:maxResults]
  final <- unique(final)
  final <- setdiff(final,badwords)
  final <- final[grepl('[[:alpha:]]',final)]        
  return(final)
}


