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





