library(tidyverse)
library(scales)
library(patchwork)
library(vroom)
library(ggrepel)

# read data ---------------------------------------------------------------

# This dataframe contains the word forms
# of all lemmas that are mentioned as
# containing a "Dehnungs-h" (lengthening <h>)
# in contemporary German orthography
# as well as their actual spelling in DTA:

d_lemmas <- read_csv("data/lexeme_mit_h_nach_Wikipedia_wortformen.csv")

# This dataframe contains all instances
# of words lemmatized as a lexeme mentioned
# in the Wikipedia entry for "Dehnungs-h"
# in their original spelling

d <- read_lines("data/lexeme_mit_dehnungs_h_laut_wikipedia.txt.zip")

# as dataframe
d <- tibble(tx = d)
d1 <- d %>% separate_wider_delim(tx, "><", 
                           names = c("year", "decade", "author", "title", "id"),
                           too_few = "align_start")
d1 <- d1 %>% separate_wider_delim(id, ">:", names = c("id", "text"))

# get lemma & word form
d1$text <- trimws(gsub("[<>]", "", d1$text))
d1 <- d1 %>% separate_wider_delim(text, "/", names = c("word_form", "lemma"),
                            too_many="merge")

# clean up metadata columns
d1$year <- as.numeric(gsub(".* ", "", d1$year))
d1$decade <- as.numeric(gsub(".* ", "", d1$decade))
d1$author <- gsub("(?<=^).*? ","", d1$author, perl = T)
d1$title <-gsub("^file_title ", "", d1$title)
d1$id <- gsub("^file_id ", "", d1$id)

# check if word form contains h
d1$with_h <- grepl("h", d1$word_form)

# for "nehmen", omit forms with i (nimmt, nimmst etc)
# and o (genommen)
d1 <- d1[-which(d1$lemma == "nehmen" & grepl("i", d1$word_form)),]
d1 <- d1[-which(d1$lemma == "nehmen" & grepl("o", d1$word_form)),]

# calculate per-lemma proportion of word
# forms with h
d_tbl <- d1 %>% group_by(decade, lemma) %>% summarise(
  n = n(), # todo: Frequenz pro 1 Mio. Wörter
  h = length(which(with_h)),
  rel = h / n
)

# add frequency per million words
# for this, we need the overall corpus size
# of the individual DTA periods (by decade)

decades <- read_tsv("data/dta_tokens_per_decade.tsv", col_names = c("Total_Freq", "decade"))

# combine with d_tbl
d_tbl <- left_join(d_tbl, decades, by="decade")

# add Frequency per million words
d_tbl$Freq_pmw <- (d_tbl$n / d_tbl$Total_Freq) * 1e6


# add frequency across all decades
d_tbl_across_decades <- d1 %>% group_by(lemma) %>% summarise(
  n_across_decades = n(), 
  freq_pmw_across_decades = (n_across_decades / sum(decades$Total_Freq))*1e6,
  h_across_decades = length(which(with_h)),
  rel_across_decades = h_across_decades / n_across_decades
)

d_tbl <- left_join(d_tbl, d_tbl_across_decades)


# get means and standard errors
se <- function(x) sqrt(var(x) / length(x))

(p0 <- d_tbl %>% group_by(decade) %>% summarise(
  mean = mean(rel),
  se   = se(rel),
  ymin = mean-se,
  ymax = mean+se
) %>% ggplot(aes(x = decade, y = mean)) +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin = ymin, ymax = ymax, width = 3)) +
  scale_y_continuous(labels = percent) +
  ylab("Proportion of spelling variants with <h>") +
  xlab("Decade") +
    ggtitle("Development of <h> spellings overall"))


# group by frequency bands (overall) - groups calculated via quartiles:
# 0-610, 611-2893, 2894-13636, 230415
(p1 <- d_tbl %>% mutate(Freq_Band = case_when(n_across_decades < 1000 ~ "<1000",
                                              n_across_decades > 1000 & n_across_decades <=10000 ~ "1001-10000",
                                              n_across_decades > 10000 & n_across_decades <= 100000 ~ "10001-100000",
                                              n_across_decades > 10000 ~ ">100000")) %>%
    mutate(Freq_Band = factor(Freq_Band, levels = rev(c("<1000", "1001-10000", "10001-100000", ">100000")))) %>%
    group_by(decade, Freq_Band) %>% summarise(
      mean = mean(rel),
      se   = se(rel),
      ymin = mean-se,
      ymax = mean+se
    ) %>% filter(mean > 0) %>% ggplot(aes(x = decade, y = mean, col = Freq_Band, group = Freq_Band)) +
    geom_point() + 
    geom_line(lwd=.8) +
    geom_errorbar(aes(ymin = 
                        ymin, ymax = ymax, 
                      group = Freq_Band), alpha = .2) +
    scale_y_continuous(labels = percent) +
    ylab("Proportion of lemmas with <h>") +
    xlab("Decade") +
    scale_color_viridis_d(end = .9) +
    guides(col = guide_legend(title = "Frequency band\n(total corpus\nfrequencies)")) +
    ggtitle("Development of <h> spellings by frequency band"))

# group by frequency bands (per decade, normalized pmw):
# 1-2500, 2501-5000,5001-10000, >10000
(p1a <- d_tbl %>% mutate(Freq_Band = case_when(Freq_pmw <= 50 ~ "<50",
                                              Freq_pmw > 50 & Freq_pmw <= 100 ~ "51-100",
                                              Freq_pmw > 100 & Freq_pmw <= 1000 ~"101-1000",
                                              Freq_pmw > 1000 ~ ">1000")) %>%
    mutate(Freq_Band = factor(Freq_Band, levels = rev(c("<50", "51-100", "101-1000",">1000")))) %>%
    group_by(decade, Freq_Band) %>% summarise(
      mean = mean(rel),
      se   = se(rel),
      ymin = mean-se,
      ymax = mean+se
    ) %>% ggplot(aes(x = decade, y = mean, col = Freq_Band, group = Freq_Band)) +
    geom_point() + 
    geom_line(lwd=.8) +
    geom_errorbar(aes(ymin = 
                        ymin, ymax = ymax, 
                      group = Freq_Band), alpha = .2) +
    scale_y_continuous(labels = percent) +
    ylab("Proportion of lemmas with <h>") +
    xlab("Decade") +
    scale_color_viridis_d(end = .9) +
    guides(fill = guide_legend(title = "Frequency band")) +
    ggtitle("Development of <h> spellings by frequency band") +
    guides(col = guide_legend(title = "Frequency band\n(per mio. words)")))

# group by frequency bands (per decade):
# 1-2500, 2501-5000,5001-10000, >10000
(p1b <- d_tbl %>% mutate(Freq_Band = case_when(n < 2500 ~ "<2500",
                                       n > 2501 & n < 5001 ~ "2501-5000",
                                       n > 5000 & n < 10001 ~ "5000-10000",
                                       n > 10000 ~ ">10000")) %>%
  group_by(decade, Freq_Band) %>% summarise(
    mean = mean(rel),
    se   = se(rel),
    ymin = mean-se,
    ymax = mean+se
  ) %>% ggplot(aes(x = decade, y = mean, col = Freq_Band, group = Freq_Band)) +
  geom_point() + 
  geom_line(lwd=.8) +
  geom_errorbar(aes(ymin = 
                      ymin, ymax = ymax, 
                    group = Freq_Band), alpha = .2) +
  scale_y_continuous(labels = percent) +
  ylab("Proportion of lemmas with <h>") +
  xlab("Decade") +
    scale_color_viridis_d(end = .9) +
    guides(col = guide_legend(title = "Frequency band\n(per decade)")) +
    ggtitle("Development of <h> spellings by frequency band"))


# group by frequency bands (per million words):
# get quartiles

quartiles <- quantile(d_tbl$freq_pmw_across_decades) %>% as.numeric()

(p1c <- d_tbl %>% mutate(Freq_Band = case_when(freq_pmw_across_decades <= quartiles[2] ~ "<3.31",
                                              freq_pmw_across_decades > quartiles[2] & freq_pmw_across_decades <=quartiles[3] ~ "3.32-15.72",
                                              freq_pmw_across_decades > quartiles[3] & freq_pmw_across_decades <=quartiles[4] ~ "15.73-74",
                                              freq_pmw_across_decades > quartiles[4] ~ ">74")) %>%
    mutate(Freq_Band = factor(Freq_Band, levels = rev(c("<3.31", "3.32-15.72", "15.73-74", ">74")))) %>%
    group_by(decade, Freq_Band) %>% summarise(
      mean = mean(rel),
      se   = se(rel),
      ymin = mean-se,
      ymax = mean+se
    ) %>% filter(mean > 0) %>% ggplot(aes(x = decade, y = mean, col = Freq_Band, group = Freq_Band)) +
    geom_point() + 
    geom_line(lwd=.8) +
    geom_errorbar(aes(ymin = 
                        ymin, ymax = ymax, 
                      group = Freq_Band), alpha = .2) +
    scale_y_continuous(labels = percent) +
    ylab("Proportion of lemmas with <h>") +
    xlab("Decade") +
    scale_color_viridis_d(end = .9) +
    guides(col = guide_legend(title = "Frequency band\n(total corpus\nfrequencies per\nmillion words)")) +
    ggtitle("Development of <h> spellings by frequency band"))


# track developments by lemma -
# take only the most frequent ones


set.seed(3000)
(p2 <- d_tbl %>% filter(n>3000) %>% 
    mutate(Lemma = ifelse(decade == 1890 & !lemma %in% c("sehr", "Uhr", "Sohn", "Wahl", "wahr"), lemma, "")) %>%
    mutate(Lemma = ifelse(lemma == "sehr" & decade == 1680, "sehr", Lemma)) %>%
    mutate(Lemma = ifelse(lemma == "Uhr" & decade == 1700, "Uhr", Lemma)) %>%
    mutate(Lemma = ifelse(lemma == "Sohn" & decade == 1650, "Sohn", Lemma)) %>%
    mutate(Lemma = ifelse(lemma == "Wahl" & decade == 1840, "Wahl", Lemma)) %>%
    mutate(Lemma = ifelse(lemma == "wahr" & decade == 1700, "wahr", Lemma)) %>%
  ggplot(aes(x = decade, y = rel,  
             group = lemma, col = lemma)) +
   geom_point( ) +
  geom_text_repel(aes(label = Lemma), size = 4, vjust = -2, max.overlaps = 30, show.legend = F) + geom_line() +
  scale_color_viridis_d(end = .8) +
  scale_y_continuous(labels = percent) +
  ylab("Proportion of spelling variants with <h>") +
  xlab("Decade") +
    ggtitle("Development of <h> spellings by lemma (Freq > 3000)") +
    guides(col = guide_legend(title = "Lemma")))

p0 | p1 | p2
# ggsave("h_spellings.png", width = 16, height = 5.5)


# which lemmas occur with a stable high frequency?
d_tbl_flipped <- d_tbl %>% select(lemma, Freq_pmw) %>% pivot_wider(names_from = decade, values_from = Freq_pmw)


# lemmas with frequency pmw of >15 across all decades in which they are attested (from 1500 onwards)
high_freq_lemmas <- d_tbl_flipped[which(sapply(1:nrow(d_tbl_flipped), function(i) all(na.omit(as.numeric(d_tbl_flipped[i,5:49]))>15))),] %>% select(lemma) %>% unlist() %>% unname()

set.seed(3000) # set a seed because ggrepel package assigns random positions
(p2d <- d_tbl %>% filter(lemma %in% high_freq_lemmas) %>% 
    mutate(Lemma = ifelse(decade == 1890 & !lemma %in% c("Ehre", "mehr", "kehren", "fahren", "Jahr", "nehmen", "lehren", "wohl"), lemma, "")) %>%
    mutate(Lemma = ifelse(lemma == "Ehre" & decade == 1680, "Ehre", Lemma)) %>%
    mutate(Lemma = ifelse(lemma == "mehr" & decade == 1600, "mehr", Lemma)) %>%
    mutate(Lemma = ifelse(lemma == "kehren" & decade == 1650, "kehren", Lemma)) %>%
    mutate(Lemma = ifelse(lemma == "fahren" & decade == 1840, "fahren", Lemma)) %>%
    mutate(Lemma = ifelse(lemma == "Jahr" & decade == 1700, "Jahr", Lemma)) %>%
    mutate(Lemma = ifelse(lemma == "nehmen" & decade == 1750, "nehmen", Lemma)) %>%
    mutate(Lemma = ifelse(lemma == "lehren" & decade == 1790, "lehren", Lemma)) %>%
    mutate(Lemma = ifelse(lemma == "wohl" & decade == 1700, "wohl", Lemma)) %>%
    ggplot(aes(x = decade, y = rel,  
               group = lemma, col = lemma)) +
    geom_point( ) +
    # geom_smooth(method = "loess") +
    geom_text_repel(aes(label = Lemma), size = 4, vjust = -2, max.overlaps = 35, show.legend = F) + geom_line() +
    scale_color_viridis_d(option = "magma", begin = .1, end = .8) +
    scale_y_continuous(labels = percent) +
    ylab("Proportion of spelling variants with <h>") +
    xlab("Decade") +
    ggtitle("Development of <h> spellings by lemma (Freq > 3000)") +
    guides(col = guide_legend(title = "Lemma")))

# same graphic but for different POS categories

# annotate pos categories
# d_tbl$lemma %>% unique %>% as_tibble() %>% writexl::write_xlsx("helpers/lemmas_for_anno.xlsx")
d_anno <- readxl::read_xlsx("helpers/lemmas_for_anno.xlsx")
d_tbl <- left_join(d_tbl, d_anno, by = c("lemma" = "Lemma"))
d_tbl$lemma <- gsub("[[:punct:]]", "", d_tbl$lemma)

d_tbl %>% mutate(Freq_Band = case_when(n < 2500 ~ "<2500",
                                               n > 2501 & n < 5001 ~ "2501-5000",
                                               n > 5000 & n < 10001 ~ "5000-10000",
                                               n > 10000 ~ ">10000")) %>%
    group_by(decade, Freq_Band, POS) %>% summarise(
      mean = mean(rel),
      se   = se(rel),
      ymin = mean-se,
      ymax = mean+se
    ) %>% ggplot(aes(x = decade, y = mean, col = Freq_Band, group = Freq_Band)) +
    geom_point() + 
    facet_wrap(~POS) +
    geom_line(lwd=.8) +
    geom_errorbar(aes(ymin = 
                        ymin, ymax = ymax, 
                      group = Freq_Band), alpha = .2) +
    scale_y_continuous(labels = percent) +
    ylab("Proportion of lemmas with <h>") +
    xlab("Decade") +
    scale_color_viridis_d(end = .9) +
    guides(col = guide_legend(title = "Frequency band\n(per decade)")) +
    ggtitle("Development of <h> spellings by frequency band")

