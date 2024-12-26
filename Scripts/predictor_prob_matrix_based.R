

#############################################
###### Predict using Probability Matrix
#############################################

predict_prob <- function(sentence, prob_matrix) {
        
        #clean and tokenise sentence
        sentence <- filter_profanity(clean_text(sentence))
        
        #sentence length
        sen_n <- length(sentence[[1]])
        
        #n-grams length
        n <- str_count(rownames(prob_matrix)[1], "_") + 1
        
        if (sen_n > n){
                ngram <- str_c(tail(sentence[[1]], n), collapse = "_")
                
                prediction <- tail(sort(prob_matrix[ngram, ]), 4)
                
        } else { 
                print("Last N Gram Not found")
        }
        
   return(prediction)     
}



### Create Probability Matrices for various N-grams
bigram_prob_matrix <- create_prob_matrix(create_count_matrix(bigrams))
trigram_prob_matrix <- create_prob_matrix(create_count_matrix(trigrams))
#fourgram_prob_matrix <- create_prob_matrix(create_count_matrix(fourgrams)) #time-consuming

sentence <- "My name is Sahil I like to the"

predict_prob("a case of", prob_matrix)

probMatList <- list(bigram_prob_matrix, trigram_prob_matrix)

predict_next_prob <- function(sentence, prob_matrix_list, n_result = 4) {
        
        #clean and tokenise sentence
        sentence <- filter_profanity(clean_text(sentence))
        
        #sentence length
        sen_n <- length(sentence[[1]])
        
        #n-gram models
        models_n <- length(prob_matrix_list)
        
        
        #Initialise a variable to store the prediction
        prediction <- NULL
        
        ##### Loop Over Models ####
        # Loop over models in descending order of n-gram size
        for (i in models_n:1) {
                
                # Check if sentence length is sufficient for the current model
                if (sen_n >= i) {
                        
                        # Create the n-gram string
                        ngram <- str_c(tail(sentence[[1]], i), collapse = "_")
                        
                        # Check if ngram exists in the model
                        if (ngram %in% rownames(prob_matrix_list[[i]])) {
                                
                                # Get prediction from the current model
                                prediction <- tail(sort(prob_matrix_list[[i]][ngram, ]),
                                                   n_result)
                                
                                # Break the loop if a prediction is found
                                break
                        }
                }
        }
        
        # Return the prediction
        return(prediction)
}

predict_next_prob("name", prob_matrix_list = probMatList, n_result = 4)
