

##############################################################################
########### Probability Matrices #############################################
##############################################################################

#libraries
library(tidyverse)



#load data
unigrams <- readRDS("Output/n-grams/unigrams")
bigrams <- readRDS("Output/n-grams/bigrams")
trigrams <- readRDS("Output/n-grams/trigrams")
fourgrams <- readRDS("Output/n-grams/fourgrams")



##########################################################################################
############## Creat a function to Make Count Matrices from Token Counts ################
##########################################################################################

library(Matrix)
library(dplyr)
library(tidyr)

create_sparse_count_matrix <- function(ngram_counts) {
        # Extract n-gram order (number of words)
        n <- str_count(ngram_counts$feature[1], "_") + 1
        
        # Split the feature column into separate words
        ngram_counts <- ngram_counts %>%
                separate(feature, into = paste0("word", 1:n), sep = "_")
        
        # Create row names by joining previous words
        if (n > 2) {
                row_names <- apply(ngram_counts[, 1:(n-1)], 1, paste, collapse = "_")
        } else {
                row_names <- ngram_counts$word1
        }
        
        # Create column names (last word)
        col_names <- ngram_counts[, n]
        
        # Create unique row and column names
        unique_row_names <- unique(row_names)
        unique_col_names <- unique(col_names)
        
        # Create lookup tables for row and column indices
        row_indices <- match(row_names, unique_row_names)
        col_indices <- match(col_names, unique_col_names)
        
        # Create a sparse matrix
        sparse_matrix <- sparseMatrix(
                i = row_indices,
                j = col_indices,
                x = ngram_counts$frequency,
                dims = c(length(unique_row_names), length(unique_col_names)),
                dimnames = list(unique_row_names, unique_col_names)
        )
        
        return(sparse_matrix)
}






##########################################################################################
########### Transform Count into Probability Matrices ####################################
##########################################################################################

create_prob_matrix <- function(count_matrix) {
        
        row_sums <- rowSums(count_matrix) 
        
        options(scipen = 9999) #prevent R from forming results in Scientific Notations
        
        prob_matrix <- count_matrix / row_sums #compute in vectorised format
        
        return(prob_matrix)
}

#Testing
count_matrix <- create_count_matrix(head(trigrams, 50))

sample <- create_prob_matrix(count_matrix)

sample

#Create Count Matrices for Project
bigram_counts_matrix <- create_count_matrix(bigrams)
trigram_counts_matrix <- create_count_matrix(trigrams)
fourgram_counts_matrix <- create_sparse_count_matrix(fourgrams)

dim(bigram_counts_matrix)
dim(trigram_counts_matrix)
dim(fourgram_counts_matrix)

#Create Probability Matrices for Project
bigram_prob <- create_prob_matrix(bigram_counts_matrix)
trigram_prob <- create_prob_matrix(trigram_counts_matrix)
fourgram_prob <- create_prob_matrix(fourgram_counts_matrix)

dim(bigram_prob)
dim(trigram_prob)
dim(fourgram_prob)

# Save Probabilities

saveRDS(bigram_prob, "Output/Probabilities/matrices/bigram_prob_matrix")
saveRDS(trigram_prob, "Output/Probabilities/matrices/trigram_prob_matrix")
saveRDS(fourgram_prob, "Output/Probabilities/matrices/fourgram_prob_matrix")
