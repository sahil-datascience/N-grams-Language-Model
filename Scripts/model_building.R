
######################################################################################
################# Building N gram Model ##############################################
######################################################################################

#################################
##### Count N-grams #############
#################################


count_n_grams <- function(tokens_data, n = 2) {
        
        #Add Start of Sentence and End of Sentence Tokens
        start_token <- 
        #function
        add_s <- function(doc){
                c(replicate(n-1, "<s>"), doc, "</s>")
        }
        
        #update_tokens
        updated_tokens <- lapply(tokens_data, add_s)
        updated_tokens <- tokens(updated_tokens)
        
        #Generate Tokens
        tokens <- tokens_ngrams(updated_tokens, n = n)
        
        #Tokens Frequency
        freq <- textstat_frequency(dfm(tokens)) %>% select(feature, frequency)
        
        #Print Message
        print("Task Complete")
        
        return(list(freq, updated_tokens, tokens))
}

#apply 
sample <- count_n_grams(test$tokens_unk, n = 3)


