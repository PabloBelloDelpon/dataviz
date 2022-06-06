
###--- Libraries
library(tidyverse)
library(ngramr)
library(showtext)
library(grid)
library(ggtext)
source("helper_functions.R")


###--- Get the data from Google Ngram
tbl <- ngramr::ngram(phrases = c("Pierre Bourdieu","Talcott Parsons"),
              corpus = "eng_2019",
              year_start = 1800,
              aggregate = TRUE,
              smoothing = 3,
              case_ins = TRUE)

saveRDS(tbl,"02bourdieu_parsons/ngram_bourdieu_parsons.RDS")

###--- Read the data
tbl <- readRDS("02bourdieu_parsons/ngram_bourdieu_parsons.RDS")

tbl2 <- 
  tbl %>% 
  filter(Year > 1900) %>% 
  mutate(Frequency = Frequency * 10^7) 

###--- Colors & Font
bg <- "#f6f0df"
c1 <- "#9cb2a7"
c2 <- "#31343c"

font <- "futura"
font_add(family = font, regular = "/Users/pablobellodelpon/Library/Fonts/Futura Light font.ttf")


###-- Margins around plot grid 
top <- 5.496774
right <- 5.109677
bottom <- 6.464516
left <- 5.109677
margins <- c(top,right,bottom,left)
margins <- 0.6 * margins


###---- Annotations

p <- "Parsons"
books_p <- c("SSA", "TGTA \nSS")
books_p_year <- c(1937,1951)

b <- "Bourdieu"
books_b <- c("S. de l'Algérie","Distinction", "IRS")
books_b_year <- c(1959,1979,1992)


###--- Y axis for parson's books
freq_p_books <- 
  as_tibble(tbl2) %>% 
  filter(Year %in% books_p_year & str_detect(Phrase, p) == TRUE) %>% 
  pull(Frequency)

###--- Y axis for Bourdieu's books
freq_b_books <- 
  as_tibble(tbl2) %>% 
  filter(Year %in% books_b_year & str_detect(Phrase, b) == TRUE) %>% 
  pull(Frequency)

###--- Graphic device and set the plot's theme
showtext_auto()
showtext_opts(dpi = 320)
theme_set(theme_figs(back_color = bg,
                     grid_color = c1,
                     font_family = font,
                     margins_grid = margins))


###--- Plot
tbl2 %>% 
  ggplot(aes(Year,Frequency, group = Phrase)) +
  geom_line() +
  labs(title = "Bourdieu and Parsons",
       subtitle = "Google Ngram frequency",
       caption = "Pablo Bello \n \nNorrköping") +
  annotate("text", x = 1950, y = 10, label = b,family = font,size = 25/.pt) +
  annotate("text", x = 1975, y = 70, label = p,family = font,size = 25/.pt) +
  annotate("text", x = books_p_year, y = freq_p_books + c(3,5), label = books_p,family = font,size = 20/.pt) +
  annotate("text", x = books_b_year, y = freq_b_books + c(3,3,4), label = books_b,family = font,size = 20/.pt)


ggsave("02bourdieu_parsons/plot_bourdieu_parsons2.png",dpi=320,width = 30,height = 40,units = "cm")  

showtext_auto(FALSE)






