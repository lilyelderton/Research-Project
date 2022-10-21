################################
## testing litsearchR package ##
################################

## load packages 
# includes downloading litsearchR (not yet on CRAN)
library(dplyr)
library(ggplot2)
library(ggraph)
library(igraph)
library(readr)
library(litsearchr)

#check package version 
packageVersion("litsearchr")

#######################################################
## example search for literature on immunopsychiatry ##
#######################################################

### download and explore dataset

## search for term on pubmed then download results to citation manager

# load results 
naive_results <- import_results(file = "pubmed-immunopsyc-set-2.nbib")
# this gives us a df with each result in a row 
# how many rows did we get?
nrow(naive_results)

# look at first few results 
naive_results

# what are the names of the columns in df?
colnames(naive_results)

# as an example, what is the title of the first paper?
naive_results[1, "title"]

### get potential search terms 

## keywords

# what key words have the authors provided for the articles?
naive_results[1, "keywords"]
naive_results[2, "keywords"]
naive_results[3, "keywords"] # some authors won't have provided keywords e.g 3rd article

# how many authors didn't provided keywords?
sum(is.na(naive_results[, "keywords"])) # 47

# we can gather the keywords together 
extract_terms(keywords = naive_results[, "keywords"],
              method = "tagged") # this argument means we get keywords that the authors themselves tagged to article

# we only get multi-word phrases? check the terms for extract_term to see why
# we need to change its default arguments to change this 
keywords <- extract_terms(keywords = naive_results[, "keywords"],
                          method = "tagged",
                          min_n = 1) # keywords of n = 1 or more to get single words too
                                     # keep min_freq = 2 so only words that appear in 2 or more docs
                                     # and max_n = 5 should be fine
keywords

# could further narrow these down to only those in 3 docs or more 
extract_terms(keywords = naive_results[, "keywords"],
              method = "tagged",
              min_n = 1, 
              min_freq = 3) # then only get 32 terms compared to 65

## titles and abstracts
# since authors can't always be relied upon to provide keywords 

# titles are fairly short and shouls contain mostly relevant terms 
# but will still have to filter them to get only interesting words 
# method for this is RAKE - fakerake function in litsearchr

extract_terms(text = naive_results[, "title"], 
              method = "fakerake", 
              min_freq = 3, 
              min_n = 1) # similar arguments as above

# gives a lot of terms - should remove stop words from these 
# an example list of clinical psychology words (clin_psy_stopwords.txt) can be added to the normal stopwords
clinpsy_stopwords <- read_lines("clin_psy_stopwords.txt")

all_stopwords <- c(get_stopwords("English"), clinpsy_stopwords)

# now try and extract only relevant title words
title_terms <- extract_terms(text = naive_results[, "title"],
                             method = "fakerake", 
                             min_freq = 4,   # 4 seems like a good amount from trial
                             min_n = 1, 
                             stopwords = all_stopwords)
title_terms

## can then add together words from titles and keywords, removing duplicates
terms <- unique(c(keywords, title_terms))
terms


### network analysis 

## can analyse our search terms as a network to see how linked they are to each other 
## this can help us filter out the unrelated terms 

# we will use the title and abstract as the content of each article
# join these two elements together 
docs <- paste(naive_results[, "title"], naive_results[, "abstract"])
# check first doc
docs[1]

## create a DFM 
# rows represent articles (title and abstract) and colums represent the search terms
dfm <- create_dfm(elements = docs,
                  features = terms) # the list of terms whose relationships we want to analyse

# examine first 3 articles as example 
dfm[1:3, 1:4]

# turn matrix into a network of linked search terms 
g <- create_network(dfm, 
                    min_studies = 3) # excludes terms in fewer than 3 articles

## visualise the network
# Use Kamada and Kawai layout which draws terms closely linked together, and less linked further away
ggraph(g, layout = "stress") + 
  coord_fixed() +
  expand_limits(x = c(-3, 3)) +
  geom_edge_link(aes(alpha = weight)) + # adds lines to link terms, with more solid line = weight of link 
  geom_node_point(shape = "circle filled", fill = "white") + 
  geom_node_text(aes(label = name), 
                 hjust = "outward", check_overlap = TRUE) + # check_overlap means arbitrary terms will be chosen to stop cluttering  
  guides(edge_alpha = FALSE)
  

### pruning 

## use network to rank search terms by importance so we can prune the least important ones 

# use strength function from igraph package
strengths <- strength(g)

# arrange terms in asc order of strength
term_strengths <- data.frame(term = names(strengths), 
           strength = strengths, 
           row.names = NULL) %>%
  mutate(rank = rank(strength, ties.method = "min")) %>% 
  arrange(strength) 

term_strengths

## we want to discard some of the terms that rarely occur together 
## what rule can we use to decide which terms to discard?

# visualising the strengths of the terms may help 
cutoff_fig <- ggplot(term_strengths, 
                     aes(x = rank, y = strength, label = term)) +
  geom_line() +
  geom_point() +
  geom_text(data = filter(term_strengths, rank > 5), 
            hjust = "right", nudge_y = 20, check_overlap = TRUE)

cutoff_fig

## pruning cumulatively 
## simply retain a certain proportion of total strength of network terms

# e.g. 80%
cutoff_cum <- find_cutoff(g, 
                          method = "cumulative", 
                          percent = 0.8)

cutoff_cum  # down to 109 terms 

# look at this on figure
cutoff_fig +
  geom_hline(yintercept = cutoff_cum, linetype = "dashed")

# prune away terms with low strength 
get_keywords(reduce_graph(g, cutoff_cum))

## changepoints pruning 
## certain points where strength of strongest term jumps a lot compared to term before 
## can use these places as cutoffs

cutoff_change <- find_cutoff(g,
                             method = "changepoint",
                             knot_num = 3) # how many knots we want to cut keywords into
cutoff_change

# put these on figure 
cutoff_fig +
  geom_hline(yintercept = cutoff_change, linetype = "dashed")

# prune away terms 
# have to pick one of the changepoints
g_redux <- reduce_graph(g, cutoff_change[1])
selected_terms <- get_keywords(g_redux)

selected_terms

### grouping 

## turn the revised list of search terms into a new search query 
## helps us get more articles relevant to the same topic 

# group related terms into subtopics 
grouped_terms <- list(
  markers = selected_terms[c(4, 5, 7, 10, 11, 15, 45, 47)], 
  disorder = selected_terms[c(3, 12, 17, 22, 23, 25, 26, 33, 34, 36, 37, 38, 50, 51)], 
  immune = selected_terms[c(2, 16, 17, 18, 43, 44)]
)

grouped_terms


### writing a new search 

## takes list of grouped search terms and writes the text of a new search

# write new search
write_search(
  grouped_terms, 
  languages = "English",  # languages you want search translated into
  exactphrase = TRUE,     # whether terms with n>1 should be matched exactly or as 2 separate words
  stemming = FALSE,       # whether words are stripped down to their stem to catch all variants of the word
  closure = "left",       # whether partial matches are matched from left end or word, right end, or only exact matchs (full)
  writesearch = TRUE)     # whether we write text to file

# read contents of the created txt file
cat(read_file("search-inEnglish.txt"))
# can copy this into pubmed 

## download results from new search
# can only get first 10,000
new_results <- import_results(file="pubmed-biomarkerO-set.nbib")

# how many results did we get? 
nrow(new_results) # 10000

## check these results are relevant to chosen topic

# check against naive search
naive_results <- naive_results %>%
  mutate(in_new_results = title %in% new_results[, "title"]) 

against_naive <- naive_results %>%
  filter(!in_new_results) %>%
  select(title, keywords)
# will be missing some here since not articles could be downloaded 
# but should be 0 rows 

# check against gold standard results
# this checks to see if important titles you know should be included are there 
important_titles <- c(
  "The future of immunopsychiatry: Three milestones to clinical innovation", 
  "Psychoneuroimmunology or immunopsychiatry?"
)

data.frame(check_recall(important_titles, new_results[, "title"]))

# these would work better if all results could be downloaded 



