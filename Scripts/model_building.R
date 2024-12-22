
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
        
        return(freq)
}

#apply 
sample <- count_n_grams(test$tokens_unk, n = 3)
head(sample)


#####################################################
######### Probability Estimator #####################
#####################################################

#Estimate Probability
estimate_probability <- function(word, previous_n_gram, n_gram_counts, n_plus1_gram_counts,
                                 vocabulary_size, k=1.0){
        
        #Create Feature Lookup function
        feature_lookup <- function(ngram, counts){ if (ngram %in% counts$feature){
                
                return(counts$frequency[counts$feature == ngram])
        } else {
                return(0)}
        }
        
        #get freq count
        freq_previous_n_gram <- feature_lookup(ngram = word, counts = n_gram_counts)
        
        #calculate denominator
        denominator <- freq_previous_n_gram + k * vocabulary_size
        
        #define nplus1gram
        n_plus1_gram <- str_c(previous_n_gram, word, sep = "_")
        
        #get nplu1gram counts
        
        freq_nplus1gram <- feature_lookup(ngram = n_plus1_gram, counts = n_plus1_gram_counts)
        
        #set numerator
        numerator <- freq_nplus1gram + k
        
        #calculate probability
        probability <- numerator / denominator
        
        
        return(probability)
        
}

#apply function
n_gram_counts = count_n_grams(test$tokens_unk, n = 1)
n_plus1_gram_counts = count_n_grams(test$tokens_unk, n = 2)
word <- "view"
previous_n_gram <- "sea"

estimate_probability(word, "window", n_gram_counts, n_plus1_gram_counts,
                     vocabulary_size = length(vocabulary),
                     k = 1.0)


##########################################################################################
################## Estimate Probabilities for All Words ##################################
##########################################################################################

count_all_prob <- function(previous_n_gram, n_gram_counts, n_plus1_gram_counts,
                           vocabulary, k = 1.0){
        
        #Add end-of-sentence and unk tokens to vocab
        up_vocab <- tibble(vocabulary) %>% rbind(c("</s>"), c("<unk>"))
        
        #Vocab size
        vocabulary_size <- length(up_vocab)
        
        #Estimate Probability for all words 
        for (word in up_vocab$vocabulary){
                probability = estimate_probability(word, previous_n_gram,
                                                   n_gram_counts, n_plus1_gram_counts,
                                                   vocabulary_size, k=k)
        up_vocab_prob <- up_vocab %>% mutate(prob = probability)
        
        return(up_vocab_prob)}
}

#apply
count_all_prob("my", n_gram_counts, n_plus1_gram_counts,
               vocabulary, k = 1.0) %>% arrange(-prob)

