###################################
## Practice reading-in full text ##
###################################

## Load packages 
library(tidyverse) # for general data manipulation
library(fulltext)  # to extract fulltext from papers

## Read in just DOIs from csv file
ip_doi <- read_csv(file = "immunopsychiatry.csv", 
               col_types = cols_only(DOI = col_guess())) %>% 
  na.omit() %>% 
  {as.vector(t(.))}

## Extract papers from server using fulltext
# extract from pubmed 
ip_fulltexts <- ft_get(ip_doi, from = "entrez")
ip_fulltexts

## put text into a table 
text_table <- ft_table() 
# text only
text_only <- select(text_table, c(dois, text)) %>% 
  distinct() %>%
  rename(doc_id = dois)
# write to file 
write.table(text_table, "text_table.csv")
write.table(text_only, "text_only.csv")

## use XML format to extract body text only
library(pubchunks)

# get the row indices of all .xml files
xml_texts_index <- str_which(as.vector(text_table$paths), 
                             ".xml")
# put the .xml file texts in their own dataframe (using indices)
xml_texts <- slice(text_only, xml_texts_index)
# get a vector of the file paths to the .xml files
xml_texts_paths <- slice(text_table, xml_texts_index)$paths

# use pub_chunks to extract just the "body" section of each .xml file
xml_texts_body <- pub_chunks(as.list(xml_texts_paths[1:41]), 
                             sections = c("doi", "body"))

# convert the output of pub_chunks from a nested list to a dataframe which is easier to read
tab <- pub_tabularize(xml_texts_body) %>%
  bind_rows()
# remove any row with an NA
tab <- na.omit(tab)

# each paragraph is in a separate row 
# put all paragraphs of each paper together in one row
tab2 <- tab %>%
  group_by(doi, .publisher) %>%
  mutate(text = paste0(body, collapse = " ")) %>%
  slice(1) %>%
  select(-body) %>%
  rename(DOI = doi, Publisher = .publisher, Text = text)

#write data to file
write.table(tab, "data.csv")

##################################
## cannot read in with fulltext ##
##################################

## read in abstracts using easyPubMed 
library(rentrez)

# get ids for all papers on pubmed mentioning immunopsychiatry
ip_ids <- entrez_search(db = "pubmed", 
                        "immunopsychiatry", 
                        retmax = 2000, 
                        use_history = T)

# get abstracts from the papers with these ids
ip_abstracts <- entrez_fetch(db = "pubmed", 
                             web_history = ip_ids$web_history, 
                             rettype = "abstract")

library(dplyr)
ip_df <- tibble(line = 1:126, text = ip_abstracts)


## read in abstracts using RISmed
library(RISmed)

# set topic for search
topic <- "immunopsychiatry OR immuno-psychiatry" # search literally what you'd put inot pubmed

# search for topic on pubmed
search_query <- EUtilsSummary(topic, retmax=200)
summary(search_query)

# look at ids of the returned articles
QueryId(search_query)

# use EUtilGet to fetch the actual data
records <- EUtilsGet(search_query)
class(records)

# collect article title, abstract and ID
ip_data <- data.frame("ID" = ArticleId(records),
                      "Year" = YearPubmed(records), 
                      "Title" = ArticleTitle(records),
                      "Abstract" = AbstractText(records)) # check records to get field names
head(ip_data,1)

ip_data$Abstract <- as.character(ip_data$Abstract)
ip_data$Abstract <- gsub(",", " ", ip_data$Abstract, fixed = TRUE)

# write to file 
write.csv(ip_data, "/Users/lilyelderton/Documents/Year 4/Research Project/Practice/Project Practice/data_raw/ip_data_raw.csv")

# add in missing abstracts manually on excel
# where abstracts aren't given, the first paragraph(s) of the introduction are used in place where appropriate
# read in file
library(readr)
ip_data_raw <- read_csv("data_raw/ip_data_raw_abstracts_added.csv")
View(ip_data_raw)

## tidy the text
library(tidytext)
library(dplyr)
library(stringr)

# remove NA abstracts 
ip_data_raw <- na.omit(ip_data_raw)

# tokenise words in abstract 
tidy_ip <- ip_data_raw %>% 
  mutate(Abstract = str_replace_all(Abstract, "\\s-", "_")) %>% 
  mutate(Abstract = str_replace_all(Abstract, "-", "_")) %>%      # these mutations help keep hyphenated words together 
  unnest_tokens(word, Abstract) %>%  # abstract to specify it is that being tokenised, not the title
  anti_join(stop_words)

# find the most common words 
tidy_ip %>% 
  count(word, sort = TRUE) # word counts stored in a tidy data frame 

# visualise the most common words
library(ggplot2)

tidy_ip %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 50) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

