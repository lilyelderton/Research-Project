####################
## Topic modeling ##
####################

# load packages 
library(topicmodels)

# read in data 
ip_data_raw <- read_csv("data_raw/ip_data_raw_abstracts_added.csv")

# find how many times a word is mentioned in each article 
article_words <- tidy_ip %>% 
  count(Title, word, sort = TRUE)

# topic models requires data to be in a DTM
articles_dtm <- article_words %>% 
  cast_dtm(Title, word, n)

# create an LDA model with 5 topics - example k
articles_lda <- LDA(articles_dtm, k = 5, control = list(seed = 1234))
articles_lda

# examine per-topic-per-word probabilites
article_topics <- tidy(articles_lda, matrix = "beta")
article_topics

# use slice_max to find top 5 terms per topic
top_terms <- article_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>% 
  ungroup() %>%
  arrange(topic, -beta)
top_terms

# visualise
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# create an LDA model with 2 topics - example k
articles_lda_2 <- LDA(articles_dtm, k = 2, control = list(seed = 1234))
articles_lda_2

# examine per-topic-per-word probabilites
article_topics_2 <- tidy(articles_lda_2, matrix = "beta")
article_topics_2

# use slice_max to find top 5 terms per topic
top_terms_2 <- article_topics_2 %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>% 
  ungroup() %>%
  arrange(topic, -beta)
top_terms_2

# visualise
top_terms_2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

## per document classification 
# can we fit the articles into topics?
articles_gamma <- tidy(articles_lda, matrix = "gamma")
articles_gamma

# find consensus topic (most common) for each article
# first use slice_max to find topic most associated 
article_classifications <- articles_gamma %>%
  group_by(document) %>%
  slice_max(gamma) %>%
  ungroup()
article_classifications

#then compare each to the consensus topic of each article
article_topics <- article_classifications %>%
  count(document, topic) %>%
  group_by(document) %>%
  slice_max(n, n = 1) %>% 
  ungroup() %>%
  transmute(consensus = document, topic)

misclassified <- article_classifications %>%
  inner_join(article_topics, by = "topic") %>%
  filter(document != consensus)
# lots of topics misclassified!

# find which words in each document were assigned to which topic
assignments <- augment(articles_lda, data = articles_dtm)
assignments

# can combine with consensus data to find which words were misclassified
assignments <- assignments %>%
  inner_join(article_topics, by = c(".topic" = "topic"))

assignments

# what were the most commonly misclassified words?
wrong_words <- assignments %>%
  filter(document != consensus)

wrong_words %>%
  count(document, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))