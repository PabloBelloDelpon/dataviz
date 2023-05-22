###--- Helper functions
library(silgelib)
library(showtext)

###--- Set a theme for plots


theme_figs <- function(back_color,grid_color,font_family, margins_grid){
  theme_minimal(base_size = 20) +
    theme(
      plot.background = element_rect(fill = back_color, color = back_color),
      panel.background = element_rect(fill = grid_color, color = grid_color),
      plot.title.position = 'plot', 
      plot.title = element_text(size = rel(1.5), hjust = .5, vjust = 10, color = c1),
      plot.subtitle = element_text(size = rel(.8), hjust = .5),
      axis.title.x = element_text(size = rel(1), hjust = 0.15),
      panel.grid = element_blank(),
      text = element_text(family = font_family),
      plot.margin = unit(margins_grid, "cm"),
      axis.text.y = ggtext::element_markdown(colour = c1)
    )
}


###---  Plot theme
font <- "bauhaus"
font_add(family = font, regular = "/Users/pablobellodelpon/Library/Fonts/BauhausRegular.ttf")

###--- Colors
bg <- "#D2B48C"
bg <- "#FFE5BD"
c1 <- "#880D2A"

###-- Margins around plot grid 
top <- 5.496774
right <- 5.109677
bottom <- 6.464516
left <- 5.109677
margins <- c(top,right,bottom,left)
margins <- 0.6 * margins


###--- Graphic device and set the plot's theme
showtext_auto()
showtext_opts(dpi = 320)
theme_set(theme_figs(back_color = bg,
                     grid_color = bg,
                     font_family = font,
                     margins_grid =  margins))


