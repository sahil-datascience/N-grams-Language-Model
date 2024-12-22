
####################################################################
############### Load and Pre-Process Data ##########################
####################################################################

library(tidyverse)

#Paths
blogs_path <- "Data/en_US.blogs.txt"
news_path <- "Data/en_US.news.txt"
twitter_path <- "Data/en_US.twitter.txt"

#Function

data_glimpse <- function(data_path) {
        
        size_mb <- round(file.size(data_path)/ 1000000, 1)
        
        lines <- strsplit(system(paste("wc -l ", data_path), intern = T), " ")[[1]][1]
        
        words <- strsplit(system(paste("wc -w ", data_path), intern = T), " ")[[1]][1]
        
        return(data.frame(size_mb, lines, words))
}

data_preview <- bind_rows(data_glimpse(blogs_path),
          data_glimpse(news_path),
          data_glimpse(twitter_path)) 


data_preview <- cbind(source = c("Blogs", "News", "Tweets"), data_preview)

total <- c("Total", sum(data_preview$size_mb), sum(as.numeric(data_preview$lines)),
           sum(as.numeric(data_preview$words)))

data_preview <- rbind(data_preview, total)

data_preview

###########################################################################################
############### Loading Sample ############################################################
###########################################################################################

# See How Many Lines for 10% Sample
data_preview <- data_preview %>% 
        mutate(sample10_lines = round(as.numeric(lines) /100 * 10),)

##### Load Sample as df

blogs <- tibble(Text = read_lines(blogs_path,n_max = data_preview$sample10_lines[1]),
                Source = "Blogs")

news <- tibble(Text = read_lines(news_path, n_max = data_preview$sample10_lines[2]),
               Source = "News")

twitter <- tibble(Text = read_lines(twitter_path, n_max = data_preview$sample10_lines[3]),
                  Source = "Twitter")

#Combine 
data0 <- bind_rows(blogs, news, twitter)

format(object.size(data0), units = "MB")

data0 %>% count(Source) %>% rename("Lines" = n)


##########################################################################################
####################### Pre-Process ######################################################
##########################################################################################

library(quanteda)

#Create clean Corpus function
clean_text <- function(data){
        
        #Create Corpus
        clean_corpus <- corpus(data, text_field = "Text")
        
        #Split in Senetences
        clean_corpus <- tokens(clean_corpus, what = "sentence")
        print(paste("Number of Sentences", length(clean_corpus)))
        
        #Split in tokens and clean
        clean_corpus <- 
                clean_corpus %>% str_to_lower() %>% 
                tokens(split_hyphens = T,
                       split_tags = T,
                       remove_punct = T,
                       remove_numbers = T,
                       remove_symbols = T,
                       remove_url = T,
                       remove_separators = T
                )  %>% 
                tokens_remove("^\\#")
        
        print("Tokenisation Complete!")
        
        return(clean_corpus)
}

#apply function
tokens_0 <- clean_text(data0)   

#############################
### Profanity Filtering #####
#############################

#create function
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
        print(paste("Number of profane words removed", before_prof - after_prof))
        return(tokens)
}

#apply function
tokens <- filter_profanity(tokens_0)



##########################################################################################
################## Feature Selection and Vocabulary ######################################
##########################################################################################

library(quanteda.textstats)

#Create Function
select_feat_vocab <- function(tokens_import, coverage_threshold = 90) {
        
        
        #Create DFM
        dfm <- dfm(tokens_import)
        
        #Count Word Frequency
        word_counts <- textstat_frequency(dfm) 
        
        #Determine Vocabulary
        vocab_df <- word_counts %>% select(feature, frequency) %>%
                mutate(cum_sum = cumsum(frequency)) %>%
                mutate(cum_percentage = round(cum_sum / sum(frequency) * 100)) %>%
                filter(cum_percentage <= coverage_threshold)
        
        total_words <- paste("Total Words", nrow(word_counts))
        print(total_words)
        words_cover <- paste("No. of Words Covering", coverage_threshold,"% of Language", nrow(vocab_df))
        print(words_cover)
        #Vocabulary
        closed_vocab <- vocab_df$feature
        
        return(closed_vocab)
}

#apply function

vocabulary <- select_feat_vocab(tokens)


##########################################################################################
####################### UNK replacement ##################################################
##########################################################################################

unk_oov_words <- function(tokens, vocabulary){
        
        oov_words <- setdiff(featnames(dfm(tokens)), vocabulary)
        print(paste("Number of OOV words", length(oov_words)))
        
        replacement_vector <- rep("<unk>", length(oov_words))
        
        tokens_oov <- tokens_replace(tokens,
                                     pattern = oov_words,
                                     replacement = replacement_vector)
        
        return(tokens_oov)
}

#apply function

tokens_unk <- unk_oov_words(tokens = tokens, vocabulary = vocabulary)


##########################################################################################
############### Create a wholistic pre-processing function ###############################
##########################################################################################

pre_process <- function(data, coverage_threshold) {
        
        #Do basic cleaning and tokenise
        tokens0 <- clean_text(data)
        
        #Filter Profane words
        tokens <- filter_profanity(tokens_0 = tokens0)
        
        #Select Features and Get Vocabulary
        vocabulary <- select_feat_vocab(tokens_import = tokens,
                                        coverage_threshold = coverage_threshold)
        
        #Replace OOV word with unk
        tokens_unk <- unk_oov_words(tokens = tokens, vocabulary = vocabulary)
        
        #print message
        print("Pre-processing complete, two output returned: vocabulary and tokens_unk")
        
        return(list(vocabulary = vocabulary, tokens_unk = tokens_unk))
}

#apply function

test <- pre_process(data0$Text[1:50], coverage_threshold = 90)


