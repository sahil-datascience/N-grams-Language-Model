

#####################
### Sampling
#####################

library(tidyverse)


#Paths
blogs_path <- "Data/en_US.blogs.txt"
news_path <- "Data/en_US.news.txt"
twitter_path <- "Data/en_US.twitter.txt"

##### Load Sample as df

blogs <- tibble(Text = read_lines(blogs_path), Source = "Blogs")

news <- tibble(Text = read_lines(news_path), Source = "News")

twitter <- tibble(Text = read_lines(twitter_path), Source = "Twitter")

#Combine 
data0 <- bind_rows(blogs, news, twitter)

format(object.size(data0), units = "MB")

data0 %>% count(Source) %>% rename("Lines" = n)

#### 
## Take Sample
####

library(rsample)

set.seed(1234)

split_initial <- initial_validation_split(data0, prop = c(0.6, 0.2))
split_initial

training_data_main <- training(split_initial)
validation_data_main <- validation(split_initial)
testing_data_main <- testing(split_initial)


###
# Save to Output
###

saveRDS(training_data_main, file = "output/data_split/training_data_main")
saveRDS(validation_data_main, file = "output/data_split/validation_data_main")
saveRDS(testing_data_main, file = "output/data_split/testing_data_main")
