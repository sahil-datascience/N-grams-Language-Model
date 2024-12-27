
#################################
##### Count N-grams #############
#################################


count_n_grams <- function(tokens_data, n = 2) {
        
        #Add Start of Sentence and End of Sentence Tokens
        
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
        freq <- textstat_frequency(dfm(tokens)) %>% select(feature, frequency) %>%
                arrange(-frequency)
        
        #Print Message
        print("Task Complete")
        
        return(freq)
}


#Calculate N-grams for project

unigrams <- count_n_grams(tokens_data = tokens_unk, n = 1)
head(unigrams)

bigrams <- count_n_grams(tokens_data = tokens_unk, n = 2)
head(bigrams)

trigrams <- count_n_grams(tokens_data = tokens_unk, n = 3)
head(trigrams)

fourgrams <- count_n_grams(tokens_unk, n = 4)
head(fourgrams)