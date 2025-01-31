
#### Dependencies of the server

# Load necessary libraries and the pre-trained model

library(shiny)
library(tidyverse)
library(quanteda)
library(quanteda.textstats)

# Load the pre-trained full word probability list
fullword_prob_list <- readRDS("fullword_prob_list")


#####################
#### Load Required Functions
#####################

# Load the function to predict the next full word
predict_next_fullword <- function(input_text, fullword_prob_list, result_n = 3,
                                  print_ngram_model_use = FALSE) { 
    #Tokenize the input text
    input_text <- filter_profanity(clean_text(input_text))
    
    #Find the n-gram length
    ngram_length <- length(input_text[[1]])
    
    #n-gram models
    models_n <- length(fullword_prob_list)
    
    #Initialise a variable to store the prediction
    prediction <- NULL
    
    ### Loop over models in descending order of n-gram size
    # Loop over models in descending order of n-gram size
    for (i in models_n:1) { 
        
        # Check if sentence length is sufficient for the current model
        if (ngram_length >= i) { 
            
            # Create the n-gram string
            ngram <- str_c(tail(input_text[[1]], i), collapse = "_")
            
            # Check if n-gram exists in the model
            if (ngram %in% fullword_prob_list[[i]]$prev_seq) {
                if (print_ngram_model_use == T){
                    print(paste("N-gram Model:", i))}
                # Get the probabilities for the n-gram
                prob_df <- fullword_prob_list[[i]] %>% 
                    filter(prev_seq == ngram) %>% 
                    select(next_word, probability)
                
                # Sort the probabilities in descending order
                prob_df <- prob_df %>% 
                    arrange(desc(probability))
                
                # Get the top n predictions
                prediction <- prob_df %>% 
                    head(result_n) %>% 
                    pull(next_word)
                
                # Break the loop
                break
            }
            
            
            
        }
        
        
    }
    
    # Return the prediction
    return(prediction)
}

#load clean text and filter profanity functions
clean_text <- function(sentence_tokens){
    
    #Create Tokens
    word_tokens <- sentence_tokens %>% 
        tokens(split_tags = T,
               split_hyphens = T,
               remove_punct = T,
               remove_numbers = T,
               remove_symbols = T,
               remove_url = T,
               remove_separators = T) %>%
        tokens_tolower()
    
    return(word_tokens)
}

filter_profanity <- function(tokens_0){
    
    #get profanity words/dict
    profane_alvarez <- tibble(word = lexicon::profanity_alvarez)
    profane_arr_bad <- tibble(word = lexicon::profanity_arr_bad)
    profane_banned <- tibble(word = lexicon::profanity_banned)
    profane_racist <- tibble(word = lexicon::profanity_racist)
    profane_zac_anger <- tibble(word = lexicon::profanity_zac_anger)
    
    #combine all
    profane_combined <- rbind(profane_alvarez, profane_arr_bad,
                              profane_banned, profane_racist,
                              profane_zac_anger)
    #apply and remove
    tokens <- tokens_0 %>% tokens_remove(profane_combined$word)
    
    #print messages
    
    #tokens before and after profanity filter
    before_prof <- length(featnames(dfm(tokens_0)))
    after_prof <- length(featnames(dfm(tokens)))
    
    #no. of profane words found in data
    if (before_prof - after_prof > 0){
        print(paste("Text contains profane words.",
                    "Number of profane words removed are",
                    before_prof - after_prof, "."))}
    return(tokens)
}


#########################
##### Shiny Server
#########################

library(shiny)


#define server logic
shinyServer(function(input, output) {
    
    # Observe changes in input text and number of predictions
    observe({
        req(input$input_text)
        
        # Predict the next full word(s)
        predicted_words <- predict_next_fullword(input$input_text,
                                                 fullword_prob_list = fullword_prob_list,
                                                 result_n = input$num_predictions)
        
        # Display the predictions
        output$prediction_output <- renderText({
            if (length(predicted_words) == 0) {
                "No prediction available."
            } else {
                paste(predicted_words, collapse = ", ")
            }
        })
    })
})

