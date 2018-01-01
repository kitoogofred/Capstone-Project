## Final Capstone Project
## Data Science Specialization
## Fredrick Edward Kitoogo

library(shiny)
library(markdown)

shinyUI(fluidPage(

        titlePanel("Next Word Text Predicting App"), 
        sidebarLayout(
        
        sidebarPanel(
          h4('Instructions'),
          h5('1. Enter a word or sentence.'),
          h5('2. Then get the next word predicted (best option and additional options)'),
          textInput("inputText", NULL, value = "",
                    width = "180px", label = "Text for Prediction"),
          numericInput("n",
                       h5("Number of options of predicted words"), 
                       value = 1),
          submitButton("Predict"),
          br(),
          "The app is created by ", 
          a("Kitoogo Fredrick Edward", href = "mailto:kitoogofred@gmail.com")
          ),
        mainPanel(
          
          tabsetPanel(type = "tabs", 
            tabPanel("Prediction",
                     h3("Best Predicted Word"),
                     span(h4(textOutput('predictedWord1')),style = "color:red"),
                     h3('Additional Predictions'),
                     h5('The other predictions based on the input and analysis are:'),
                     tableOutput("view")
                     ),
            # tabPanel("Dataset"),
            # tabPanel("Algorithm"),
            # tabPanel("Algorithm", includeMarkdown("algorithm.md")),
            tabPanel("About", includeMarkdown("about.Rmd"))
                )
        
      
    ))
  
))