
###--- Libraries
library(tidyverse)
library(ngramr)
library(showtext)
library(grid)
library(ggtext)
library(Cairo)
source("helper_functions.R")


output_data_file <-  "03merton_coleman/ngram_coleman_merton.RDS"
output_plot1 <- "03merton_coleman/merton_colemanV1.png"
output_plot2 <- "03merton_coleman/merton_colemanV2.png"

###--- Get the data from Google Ngram
# tbl <- ngramr::ngram(phrases = c("James S. Coleman + James Coleman","Robert K. Merton"),
#                      corpus = "eng_2019",
#                      year_start = 1800,
#                      aggregate = TRUE,
#                      smoothing = 5,
#                      case_ins = TRUE)
# 
# saveRDS(tbl,"output_data_file")

###--- Read the data
tbl <- readRDS(output_data_file)

tbl2 <- 
  tbl %>% 
  filter(Year > 1900) %>% 
  mutate(Frequency = Frequency * 10^10) 

###--- Colors & Font
bg <- "#ede5d8"
c1 <- "#253c7f"
c2 <- "#222c5e"
c3 <- "#17163d"
c4 <- "#010007"

font <- "bauhaus"
font_add(family = font, regular = "/Users/pablobellodelpon/Library/Fonts/BauhausRegular.ttf")


###-- Margins around plot grid 
top <- 5.496774
right <- 5.109677
bottom <- 6.464516
left <- 5.109677
margins <- c(top,right,bottom,left)
margins <- 0.6 * margins


###---- Annotations

  ###--- Merton
  p <- "Robert K. Merton"
  labels_p <- c(as.character(expression(tau)),"S.Theory & \nS.Structure", "Matthew Effect",as.character(expression(theta)))
  dates_p <- c(1910,1949,1968,2003)
  
  
  ###--- Coleman
  b <- "James Coleman"
  labels_b <- c(as.character(expression(tau)),"EEO","FST",as.character(expression(theta)))
  dates_b <- c(1926,1968,1990,1995)
  
  
  
  ###--- Y axis for P books
  freq_p_books <-
    as_tibble(tbl2) %>%
    filter(Year %in% dates_p & str_detect(Phrase, p) == TRUE) %>%
    pull(Frequency)
  
  ###--- Y axis for B books
  freq_b_books <-
    as_tibble(tbl2) %>%
    filter(Year %in% dates_b & str_detect(Phrase, b) == TRUE) %>%
    pull(Frequency)
  

###--- Graphic device and set the plot's theme
showtext_auto()
showtext_opts(dpi = 320)
theme_set(theme_figs(back_color = bg,
                     grid_color = bg,
                     font_family = font,
                     margins_grid = margins))

(p1 <- tbl2 %>% 
  ggplot(aes(Year,Frequency, group = Phrase)) +
  geom_line(color = c2) +
  labs(title = "James Coleman and Robert K. Merton",
       subtitle = "Google Ngram frequency",
       caption = "Pablo Bello \n \nNorrköping") +
    geom_hline(aes(yintercept = max(tbl2$Frequency)*1.1)) +
  annotate("text", x = 1990, y = 100, label = b,family = font,size = 25/.pt,color = c1) +
  annotate("text", x = 1925, y = 900, label = p,family = font,size = 25/.pt, color = c1) +
  annotate("text", x = dates_p[c(2:3)] - c(9,0), y = freq_p_books[c(2:3)] + c(0,100), label = labels_p[c(2:3)],family = font,size = 20/.pt,color = c1) + 
  annotate("text", x = dates_b[c(2:3)] + c(3,0), y = freq_b_books[c(2:3)] - c(50,50), label = labels_b[c(2:3)],family = font,size = 20/.pt, color = c1))

###--- Save as png
ggsave(output_plot1,dpi=320,width = 30,height = 40,units = "cm")  



###--- Add greek letter annotations 
p1 +
  annotate("text", x = dates_p[c(1,4)], y = freq_p_books[c(1,4)] + c(25, -25), label = labels_p[c(1,4)],size = 20/.pt,color = c1,parse = TRUE) +
  annotate("text", x = dates_b[c(1,4)], y = freq_b_books[c(1,4)] + c(25, -25), label = labels_b[c(1,4)],size = 20/.pt,color = c1,parse = TRUE)


###--- Save as pdf
ggsave(output_plot2,dpi=320,width = 30,height = 40,units = "cm")  

