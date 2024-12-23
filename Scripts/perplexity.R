
##########################################################################################
##################### Perplexity #########################################################
##########################################################################################

calculate_perplexity <- function(sentence, n_gram_counts, n_plus1_gram_counts,
                                 vocabulary_size, k = 1.0) {
        
        #length of previous words
        n <- length(str_split(n_gram_counts$feature, "_")[[1]])
        
        # SENTENCE prepend <s> and append <e>
        sentence <- corpus(sentence)
        sentence <- clean_text(sentence)
                #function
                add_s <- function(doc){
                        c(replicate(n-1, "<s>"), doc, "</s>")}
        #updated sentence
        sentence <- lapply(sentence, add_s)
        sentence <- tokens(sentence)
        
        # length of sentence (after adding <s> and <e> tokens)
        N <- length(sentence[[1]])
        
        # The variable p will hold the product
        # that is calculated inside the n-root
        # Update this in the code below
        product_pi <- 1.0
        
        #Loop Over Sentence
        for (t in seq(n, N)){
                
                #get the ngram 
                n_gram <- str_c(sentence[[1]][(t-n):t], collapse = "_")
                
                #get the word at position t
                word <- sentence[[1]][t]
                
                # Estimate the probability of the word given the n-gram
                # using the n-gram counts, n-plus1-gram counts,
                # vocabulary size, and smoothing constant
                probability <- estimate_probability(word, n_gram,
                                                   n_gram_counts,
                                                   n_plus1_gram_counts,
                                                   vocabulary_size, k)
                
                # Update the product of the probabilities
                # This 'product_pi' is a cumulative product 
                # of the (1/P) factors that are calculated in the loop
                product_pi <- product_pi * 1/probability
                }
                
        # Take the Nth root of the product
        perplexity <- (product_pi) ^ (1/N)
        
        
        return (perplexity)
}


