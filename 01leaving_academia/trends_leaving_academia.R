
###--- Packages
library(academictwitteR)
library(tidyverse)
library(here)
library(silgelib)
library(showtext)

###--- Get the data from the Twitter API
# token  <- ""
# trends <- 
#   count_all_tweets(
#   query = "#leavingacademia",
#   start_tweets = "2010-01-01T00:00:00Z",
#   end_tweets = paste0(Sys.Date(),"T00:00:00Z"),
#   bearer_token = token,
#   n = 10^6
# )
# 
# 
# trends <- as_tibble(trends)
#saveRDS(trends,"trends_leaving_academia.RDS")

###--- Load the data
trends <- readRDS("trends_leaving_academia.RDS")

###--- Annotations
date1 <- as.Date("2022-03-04")
date2 <- as.Date("2022-03-31")
name1 <- "Christopher Jackson"
name2 <- "Caspar Addyman"
extra <- tibble(date = c(date1,date2),name = c(name1,name2))
extra <- 
  extra %>% 
  mutate(y = c(110, 220),
         xcurve = c(50,70),
         xtext = c(120,130),
         yend = c(60,110))

###--- Set up for plot

###--- Colors
bg <- "#E5E3C9"


###--- Theme 
theme_figs <- function(){
  theme_roboto(base_size = 20,) +
    theme(
      plot.background = element_rect(fill = bg, color = bg),
      plot.title = element_text(size = rel(1.5), hjust = .5),
      plot.subtitle = element_text(size = rel(.8), hjust = .5,margin=margin(0,0,30,0)),
      axis.title.x = element_text(size = rel(1)),
      axis.title.y = element_text(size = rel(1)),
      axis.text.x = element_text(size = rel(1)),
      #axis.text.y.left = element_text(size = rel(1.2)),
      #axis.text.y = element_blank(),
      axis.text.y.right = element_text(size = rel(0.9)),
      legend.position = "none",
      panel.grid = element_blank()
    )
}

theme_set(theme_figs())


###--- Basic Plot
(plot <- 
  trends %>% 
  mutate(end = as.Date(end)) %>% 
  filter(end > as.Date("2021-06-01")) %>% 
  ggplot(aes(end,tweet_count, group = 1)) +
  geom_line(size = 1) +
  labs(y = "Tweet Count",
       x = "",
       title = "#leavingacademia",
       subtitle = "Some academics announced their resignation on Twitter.\nWhat was the volume of the debate?"))

###--- Add text labels
(plot3 <- 
  plot +
  annotate(geom = "text",
           size = 5, 
           x = extra$date - extra$xtext, 
           y = extra$y, 
           label = extra$name,
           hjust = "left",
           family = theme_roboto()$text[["family"]]))


###--- Add arrows
plot3 +
  annotate(
    geom = "curve",
    x = extra$date - extra$xcurve, 
    y = extra$y, 
    xend = extra$date, 
    yend = extra$yend, 
    #data = extra[,c(1:2)],
    curvature = -.2, 
    arrow = arrow(length = unit(2, "mm"))
  ) 


###--- Save the plot
showtext_opts(dpi = 320) 

ggsave("#leavingacademia.png",dpi=320)  

showtext_auto(FALSE)


