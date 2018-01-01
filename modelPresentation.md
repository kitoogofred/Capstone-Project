Predict Next Word - Model and Shiny App - Capstone Project
========================================================
author: Fredrick Edward Kitoogo
date: 31st December 2017
autosize: true

The application is the outcome of the fianl capstone project for the Coursera Data Science specialization course

Introduction
========================================================
The purpose of this project is to build a text prediction model that bases on a user's text input to suggest the next most likely word.

- The model uses three types of data including twitter, news and blogs for training and testing before being subjected to user input. 

- Appropriate data cleaning and sampling techniques were applied to prepare the training and testing data. 

- A predictive model was built based on N-Grams and a katz stupid backoff model, optimized and buit into a final  interactive Shiny application.

Model Building
========================================================
The data source was provided by SwiftKey:  https://web-beta.archive.org/web/20160930083655/http://www.corpora.heliohost.org/aboutcorpus.html and processed as follows:
- The data was downloaded, basic exploratory analysis, cleaning, sampling (1%) and a Corpus developed. For this work only the Eglish Data Sets were used
- The Corpus was tokeninized into n-grams (n - 1,2,3), frequencies and probabilities computed and saved for model building
- A model is built and tested using the n-Grams, Reequencies & Probabiltieis and katz backoff algorithm (the model backoffs to lower order N-gram if we have zero count for the higher order N-gram

Shiny App
========================================================
- To use the application, simply type in a phrase in the text box located in the Left Pane. Also chosse the number of possible predictions of the next word.
- In the right Pane, a predicted word will appear in red. Additionally, underneath the predicted word (with the highest probability) is a table of other predicted suggestions in order of probabilty. 

The Shinny App can be tested using the link below:
https://kitoogofred.shinyapps.io/text_prediction_shinny_app/

The App Source Code and other files can be accessed here:
https://github.com/kitoogofred/Capstone-Project

Additional Information and Future Improvements
========================================================
**Tools Used**
- R Version 3.4.2 & RStudio Version 1.0.153 
- R Libraries (tm, RWeka, etc)

**Future Propose Improvements**
There were some limitations on the app but they can be improved with additional development as follows:
- Learning from the Users Input (grammar, sentence construction, etc)
- Improved Model Testing and Optimization (Accuracy, Perplexity, etc.)
- Utilization of supplementary data sources for training, development and testing
