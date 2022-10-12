### unnest_tokens function -----------------
# define text
text <- c("Because I could not stop for Death -", 
          "He kindly stopped for me -", 
          "The Carriage held but just Ourselves -", 
          "and Immortality")
text

## we need this in a dataframe to be able to analyse it
library(dplyr) # load dplyr
# make dataframe 
text_df <- tibble(line = 1:4, text = text) # tibbles are a type of df
text_df                                    # they are great for tidy tools

## need to convert this to one-token-per-doc-per-row 
## for this we use unnest_tokens()
library(tidytext) # load tidytext package (where function is)
# tokenise 
text_df %>% 
  unnest_tokens(word, text) # use to_lower = FALSE if you don't want auto-lowercase


### Tidying more than one document ---------------
library(janeaustenr) # works of jane austen in a one-row-per-line format
library(stringr)
library(tidyverse)

# source books 
original_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(),          # adds linenumber and chapter cols 
         chapter = cumsum(str_detect(text,   # to keep track of where words come from 
                                     regex("^chapter [\\divxlc]", 
                                           ignore_case = TRUE)))) %>% 
  ungroup()
original_books

# tokenise
tidy_books <- original_books %>% 
  unnest_tokens(word, text)
tidy_books
# remove stop words
data(stop_words) # dataset that includes stop words
tidy_books <- tidy_books %>% 
  anti_join(stop_words)

# find the most common words 
tidy_books %>% 
  count(word, sort = TRUE) # word counts stored in a tidy data frame 

# visualise the most common words
library(ggplot2)

tidy_books %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 600) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)


