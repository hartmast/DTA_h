library(tidyverse)
library(scales)
library(patchwork)
library(vroom)
library(ggrepel)
library(plotly)
library(shiny)

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
  n_across_decades = n(), # todo: Frequenz pro 1 Mio. Wörter
  h_across_decades = length(which(with_h)),
  rel_across_decades = h_across_decades / n_across_decades
)

d_tbl <- left_join(d_tbl, d_tbl_across_decades)

# top 10
d_tbl %>% arrange(desc(n_across_decades)) %>% ungroup() %>% select(lemma) %>% unique() %>% head(10)

# get means and standard errors
se <- function(x) sqrt(var(x) / length(x))

# function for plot

myplot <- function(lemmas) {
  
  if(missing(lemmas)) {
    lemmas <- "nehmen"
  }
  
  p <- d_tbl %>% 
    filter(lemma %in% lemmas) %>%
    ggplot(aes(x = decade, y = rel,  
               group = lemma, col = lemma)) +
    geom_point( ) +
    geom_line() +
    scale_color_viridis_d(end = .8) +
    scale_y_continuous(labels = percent) +
    ylab("Proportion of spelling variants with <h>") +
    xlab("Decade") +
    ggtitle("Development of <h> spellings by lemma (Freq > 3000)") +
    guides(col = guide_legend(title = "Lemma"))
  return(p)
}




# shinyapp
ui <- fluidPage(
  sidebarPanel(
    checkboxGroupInput("lemma", "lemma", choices = levels(factor(d_tbl$lemma))),
  ),
  
  mainPanel(plotOutput("p2", height = 800))
)

server <- function(input, output, session) {

  output$p2 <- renderPlot({
    myplot(input$lemma)
  })
}

shinyApp(ui = ui, server = server)
