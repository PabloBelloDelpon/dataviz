
###--- Libraries
library(tidyverse)
library(readxl)
library(ggtext)
library(showtext)
source("06sociology_viz/plot_theme.R")



###--- Read data
# WoS query link:  https://www.webofscience.com/wos/woscc/summary/08e7543c-be9b-4990-b598-3436462b05c2-8bd7ba4a/times-cited-descending/1
tbl <- read_excel("06sociology_viz/data/soc_2010_2020.xls")



###--- Clean up data
tbl <- 
  tbl |>
  janitor::clean_names() |> 
  select(authors, 
         title = article_title, 
         journal = source_title,
         year = publication_year,
         cited = times_cited_wo_s_core) |> 
  mutate(title = str_to_title(title),
         title = str_split(title, ":"),
         title = map_chr(title, first))
  

###--- Journal abbreviations
labels <- 
  c("ANNUAL REVIEW OF SOCIOLOGY" = "ARS",
    "EUROPEAN SOCIOLOGICAL REVIEW" = "ESR",
    "GENDER & SOCIETY" = "G&S",
    "GLOBAL NETWORKS-A JOURNAL OF TRANSNATIONAL AFFAIRS" = "GN",
    "INFORMATION COMMUNICATION & SOCIETY" = "ICS",
    "JOURNAL OF CONSUMER CULTURE" = "JCC",
    "JOURNAL OF HEALTH AND SOCIAL BEHAVIOR" = "JHSB",
    "JOURNAL OF MARRIAGE AND FAMILY" = "JMF",
    "QUALITATIVE RESEARCH" = "QR",
    "SOCIAL INDICATORS RESEARCH" = "SIR",
    "SOCIAL NETWORKS" = "SN",
    "SOCIOLOGICAL METHODS & RESEARCH" = "SMR",
    "SOCIOLOGICAL QUARTERLY" = "SQ",
    "SOCIOLOGICAL THEORY" = "ST")

labels <- tibble(journal_abb = labels, journal = labels(labels))

###--- Shorten some titles
titles <- 
  c("The Politicization Of Climate Change And Polarization In The American Public's Views Of Global Warming, 2001-2010" = "The Politicization Of Climate Change",
    "Critical Questions For Big Data Provocations For A Cultural, Technological, And Scholarly Phenomenon" = "Critical Questions For Big Data Provocations",
    "Production, Consumption, Prosumption The Nature Of Capitalism In The Age Of The Digital 'Prosumer'" = "Production, Consumption, Prosumption",
    "The Logic Of Connective Action Digital Media And The Personalization Of Contentious Politics" = "The Logic Of Connective Action")

titles <- tibble(title_abb = titles, title = labels(titles))

###---  Jounral abb to journal name textbox
textbox <- 
  labels |> 
  mutate(journal = ifelse(str_detect(journal, "GLOBAL NETWORKS"), "GLOBAL NETWORKS", journal),
         journal = str_to_title(journal),
         journal = paste(journal_abb, "=", journal),
         journal = str_replace(journal, "Of", "of"),
         journal = str_replace(journal, "And", "and")) |> 
  pull(journal) |> 
  paste0(collapse = "<br>")



###--- Select top 20 articles
tbl_top20 <- 
  tbl |> 
  arrange(desc(cited)) |> 
  slice_head(n = 20) |> 
  mutate(journal = ifelse(
    str_detect(journal,"ANNUAL REVIEW OF SOCIOLOGY"), 
    "ANNUAL REVIEW OF SOCIOLOGY",
    journal)) |> 
  left_join(labels) |> 
  left_join(titles) |> 
  mutate(title = str_replace(title, "Of ", "of "),
         title = str_replace(title, "And ", "and "),
         title = str_replace(title, "For ", "for "),
         title = str_replace(title, "In ", "in "),
         title = str_replace(title, "To ", "to "),
         title = str_replace(title, "With ", "with ")) |> 
  mutate(title_abb = ifelse(is.na(title_abb) == TRUE, title, title_abb),
         title2 = paste(title_abb, "\n", authors)) 


###--- Create axis labels with two font sizes (one for title other for authors)
tbl_top20 <- 
  tbl_top20 |> 
  mutate(title2 = paste0("<span style='font-size: 16pt'>", title_abb,"</span><br><span style='font-size: 14pt'>",authors, "</span>"))



###--- Plot
tbl_top20 |> 
  mutate(title2 = fct_reorder(title2, cited)) |> 
  ggplot(aes(cited, title2, label = journal_abb)) +
  geom_text(color = c1) +
  geom_linerange(aes(xmin = 0, xmax = cited - 75)) +
  labs(title = "Sociology's Top Hits 2010-2020",
       caption = "Data from WoS",
       x = "Number of Citations",
       y = "") +
  geom_textbox(aes(x = 2050, y = 5.5, label = textbox),
               fill = bg,
               width = unit(0.4, "npc"), 
               inherit.aes = FALSE,
               size = 5,
               lineheight = 1.3,
               family = font)


ggsave("06sociology_viz/fig.png", width = 8.54*2, height = 6.15*2.5, dpi = 300)
ggsave("06sociology_viz/fig.pdf", width = 8.54*2, height = 6.15*2.5, dpi = 600)
