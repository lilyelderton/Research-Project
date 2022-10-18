####################
## practice tf-id ##
####################

# load packages
library(dplyr)
library(stringr)
library(tidytext)

# read in data 
ip_data_raw <- read_csv("data_raw/ip_data_raw_abstracts_added.csv")

# find how many times a word is mentioned in each article 
article_words <- tidy_ip %>% 
  count(Title, word, sort = TRUE)

# what is the total amount of words in each article?
total_words <- article_words %>% 
  group_by(Title) %>% 
  summarize(total = sum(n))

# join the 2 datasets
article_words <- left_join(article_words, total_words)

################
## Zipf's law ##
################

# law says the frequency that a word appeasrs is inversely proportional to its rank
freq_by_rank <- article_words %>% 
  group_by(Title) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

# visualise 
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = Title)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

################
## bind tf-id ##
################

article_tf_idf <- article_words %>%
  bind_tf_idf(word, Title, n)




