
##################
### New Full word Model
################## 

# Model: 07 January 2024

library(tidyverse)
library(quanteda)
library(quanteda.textstats)

# Load the data
training_data <- readRDS("Output/data_split/training_data_main")
nrow(training_data)

data <- training_data$Text[1:200000]

length(data)

#Split into Sentence
sentences <- tokens(data, what = "sentence")
sentences <- unlist(sentences) %>% as.character()
length(sentences)

#Split into words
words <- tokens(sentences, what = "word",
                remove_punct = T, remove_symbols = T, remove_numbers = T,
                remove_url = T, remove_separators = T, split_hyphens = T,
                split_tags = T, include_docvars = F, padding = F) %>% 
        tokens_tolower()

#Replace Profane Words
words <- filter_profanity(words)

total_words <- nrow(textstat_frequency(dfm(words)))
total_words

#How many words cover
vocab <- select_feat_vocab(words, coverage_threshold = 95)

words %>% dfm() %>% textstat_frequency() %>% #example testing
        filter(frequency == 1) %>% nrow()

#Trim infrequent words
words <- tokens_trim(words, min_termfreq = 2)
total_words <- nrow(textstat_frequency(dfm(words)))
total_words

#####
# Fullword n-grams
#####
unigrams <- tokens_ngrams(words, n = 1)
bigrams <- tokens_ngrams(words, n = 2)
trigrams <- tokens_ngrams(words, n = 3)
quadgrams <- tokens_ngrams(words, n = 4)
pentagrams <- tokens_ngrams(words, n = 5)

#Find rare instances
bigram_vocab <- select_feat_vocab(bigrams, coverage_threshold = 65)
trigram_vocab <- select_feat_vocab(trigrams, coverage_threshold = 25)
quadgram_vocab <- select_feat_vocab(quadgrams, coverage_threshold = 8)
pentagram_vocab <- select_feat_vocab(pentagrams, coverage_threshold = 3)

#Trim infrequent n-grams
bigrams <- tokens_trim(bigrams, min_termfreq = 8)
trigrams <- tokens_trim(trigrams, min_termfreq = 6)
quadgrams <- tokens_trim(quadgrams, min_termfreq = 3)
pentagrams <- tokens_trim(pentagrams, min_termfreq = 2)

############
### Count Matrices and Probabilities
############

unigrams_df <- unigrams %>% dfm() %>% textstat_frequency() %>% select(feature, frequency)
bigrams_df <- bigrams %>% dfm() %>% textstat_frequency() %>% select(feature, frequency) 
trigrams_df <- trigrams %>% dfm() %>% textstat_frequency() %>% select(feature, frequency)
quadgrams_df <- quadgrams %>% dfm() %>% textstat_frequency() %>% select(feature, frequency)
pentagrams_df <- pentagrams %>% dfm() %>% textstat_frequency() %>% select(feature, frequency)


#Mutate Probability

unigrams_df <- unigrams_df %>% mutate(probability = frequency/sum(frequency))
bigrams_df <-  mutate_prob(bigrams_df)
trigrams_df <- mutate_prob(trigrams_df)
quadgrams_df <- mutate_prob(quadgrams_df)
pentagrams_df <- mutate_prob(pentagrams_df)

#Enlist and save the data
fullword_prob_list <- list(bigrams_df,
                           trigrams_df, quadgrams_df,
                           pentagrams_df)

format(object.size(fullword_prob_list), units = "Mb")

saveRDS(fullword_prob_list, "Output/Probabilities/fullwords/fullword_prob_list")


##################################
######### Prediction
##################################


#predict next full word
predict_next_fullword("How are you doing",
                      fullword_prob_list = fullword_prob_list,
                      result_n = 5)


#Test on assignment questions
lapply(question_list, predict_next_fullword, fullword_prob_list = fullword_prob_list, result_n = 3)



##################################
########## Accuracy
##################################

#Load the data
test_data <- training_data$Text[200000:200010]
test_data

#Split into Sentence
test_sentences <- tokens(test_data, what = "sentence")
test_sentences <- unlist(test_sentences) %>% as.character()
test_sentences


#Apply test accuracy function
accuracy <- test_accuracy_fullword(test_sentences, fullword_prob_list,
                                   print_messages = FALSE)
print(paste("Accuracy:", accuracy))






