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