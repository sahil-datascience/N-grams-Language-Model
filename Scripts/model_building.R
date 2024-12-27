
######################################################################################
################# Building N gram Model ##############################################
######################################################################################


#list_ngram_counts <- list(unigrams, bigrams, trigrams, fourgrams)

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
        freq_previous_n_gram <- feature_lookup(ngram = previous_n_gram,
                                               counts = n_gram_counts)
        
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

word <- "cat"
previous_n_gram <- "a"

estimate_probability(word, previous_n_gram, unigrams, bigrams,
                     vocabulary_size = length(vocabulary),
                     k = 1.0)

#Sample Exercise
sentences = rbind(c("i like a cat"),
                   c("this dog is like a cat"))

colnames(sentences) <- "Text"
rownames(sentences) <- c("1", "2")

sentences

sentences <- pre_process(sentences, coverage_threshold = 100)

unique_words <- sentences$vocabulary

sentences <- sentences$tokens_unk

unigram_counts = count_n_grams(sentences, 1)
bigram_counts = count_n_grams(sentences, 2)

estimate_probability("cat", "a",
                                unigram_counts,
                                bigram_counts,
                                length(unique_words), k=1)

##########################################################################################
################## Estimate Probabilities for All Words ##################################
##########################################################################################

estimate_probabilities <- function(previous_n_gram, n_gram_counts, n_plus1_gram_counts,
                           vocabulary, k = 1.0){
        
        #Add end-of-sentence and unk tokens to vocab
        up_vocab <- tibble(vocabulary) %>% rbind(c("</s>"), c("unk"))
        
        #Vocab size
        vocabulary_size <- length(up_vocab)
        
        # Initialize an empty data frame to store the results
        results_df <- data.frame(word = character(), prob = numeric())
        #Estimate Probability for all words 
        for (i in seq(nrow(up_vocab))){
                
                word <- up_vocab$vocabulary[i]
                
                #estimate the prob for next word
                #from the vocabuluary
                prob <- estimate_probability(word, previous_n_gram,
                                                   n_gram_counts,
                                                   n_plus1_gram_counts,
                                                   vocabulary_size, k=k)
                
                print(i)
                
        #attach prob score to each word in vocab
       
        # Add the word and its probability to the results data frame
        results_df <- rbind(results_df, data.frame(word = word, prob = prob))
        
        } 
        
return(results_df)
        
}

#apply
estimate_probabilities("case_of", bigrams, trigrams,
               vocabulary, k = 1.0) %>% arrange(-prob)


#Sample Exercise

#note: the data for this exercise is dependent on the above exercise below the 
# estimate probability function

estimate_probabilities("a", unigram_counts, bigram_counts, unique_words, k=1)

