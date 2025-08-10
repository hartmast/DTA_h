library(tidyverse)
library(vroom)

# read data
d <- read_lines("data/lexeme_mit_h_nach_vokal_lemma_word_DTA.txt.zip")

# only unique combinations of
# lemma and word form
d1 <- gsub(".*<", "", d)
d1 <- gsub(">$", "", d1)
d1 <- tibble(w = d1)
d1 <- d1 %>% separate_wider_delim(w, 
                                  delim="/", 
                                  names = c("word", "lemma"),
                                  too_many = "merge")


# omit all with any kind of punctuation
d1 <- d1[-grep("[[:punct:]]", d1$lemma),]


# unique lemmas with all spelling variants
d_lemmas <- d1$lemma %>% unique
d_lemmas <- tibble(lemma = d_lemmas)
d_lemmas$wordforms <- sapply(1:nrow(d_lemmas), function(i) paste(unique(filter(d1, lemma == d_lemmas[i,]$lemma)$word), collapse=","))

# export
# write_csv(d_lemmas, "data/lexeme_mit_h_wortformen.csv")


# read data - lemmas from Wikipedia
d <- read_lines("data/lexeme_mit_dehnungs_h_laut_wikipedia.txt.zip")

# only unique combinations of
# lemma and word form
d1 <- gsub(".*<", "", d)
d1 <- gsub(">$", "", d1)
d1 <- tibble(w = d1)
d1 <- d1 %>% separate_wider_delim(w, 
                                  delim="/", 
                                  names = c("word", "lemma"),
                                  too_many = "merge")


# omit all with any kind of punctuation
d1 <- d1[-grep("[[:punct:]]", d1$lemma),]


# unique lemmas with all spelling variants
d_lemmas <- d1$lemma %>% unique
d_lemmas <- tibble(lemma = d_lemmas)
d_lemmas$wordforms <- sapply(1:nrow(d_lemmas), function(i) paste(unique(filter(d1, lemma == d_lemmas[i,]$lemma)$word), collapse=","))

# export
# write_csv(d_lemmas, "data/lexeme_mit_h_nach_Wikipedia_wortformen.csv")

