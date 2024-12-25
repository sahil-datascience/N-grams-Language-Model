

##############################################################################
########### Probability Matrices #############################################
##############################################################################

##########################################################################################
############## Creat a function to Make Count Matrices from Tokens Counts ################
##########################################################################################

create_count_matrix <- function(ngram_counts) {
        # Extract n-gram order (number of words)
        n <- str_count(ngram_counts$feature[1], "_") + 1 
        
        # Create ngram combinations
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
        
        # Get unique row and column names
        unique_row_names <- unique(row_names)
        unique_col_names <- unique(col_names)
        
        # Create empty matrix with unique row and column names
        prob_m <- matrix(0, nrow = length(unique_row_names), ncol = length(unique_col_names), 
                         dimnames = list(unique_row_names, unique_col_names))
        
        # Create lookup tables for row and column indices
        row_indices <- setNames(seq_along(unique_row_names), unique_row_names)
        col_indices <- setNames(seq_along(unique_col_names), unique_col_names)
        
        # Fill prob_m with frequencies
        for (i in 1:nrow(ngram_counts)) {
                # Get row and column names for the current row
                current_row_name <- row_names[i] 
                current_col_name <- col_names[i]
                
                # Get row and column indices
                row_idx <- row_indices[[current_row_name]]
                col_idx <- col_indices[[current_col_name]]
                
                prob_m[row_idx, col_idx] <- ngram_counts$frequency[i]
        }
        
        return(prob_m)
}

create_count_matrix(head(trigrams, 50)) 

##########################################################################################
########### Transform Count into Probability Matrices ####################################
##########################################################################################

create_prob_matrix <- function(count_matrix) {
        
        # Normalize each row of the count matrix to obtain probabilities
        prob_matrix <- t(apply(count_matrix, 1, function(x) x / sum(x))) 
        
        return(prob_matrix)
}

create_prob_matrix(create_count_matrix(head(trigrams, 50)))



