## Capstone Project
## Data Science Specialization
## Fredrick Edward Kitoogo

library(shiny)
library(stringr)
library(tm)

source("globalFuns.R")

## Load the ngram and badword files
load('final/ngram.RData')
badwords <- read.csv("final/badwords.csv",
                     header = FALSE, sep = ",")

# ShineServer code 
shinyServer(
  function(input, output) {
    
      model1pred <- reactive({
        toPredictNext <- as.character(input$inputText)
        predicted <- predBKO(toPredictNext, 
                               badwords, unigramDF, 
                               bigramDF, trigramDF,
                               maxResults = input$n)
      })
        
      output$predictedWord1 <- renderText({
        firstPredicted <- c('')
        firstPredicted <- model1pred()
        firstPredicted <- firstPredicted[1] ## first predicted word
      })
      
      output$view <- renderTable({
        otherPredicted <- c('')
        otherPredicted <- model1pred()
        otherPredicted <- otherPredicted[-1]## Other predicted
      }, colnames = FALSE) 
      
  })
   
    
  



