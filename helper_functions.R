###--- Helper functions
library(silgelib)

###--- Set a theme for plots


theme_figs <- function(back_color,grid_color,font_family, margins_grid){
    #theme_roboto(base_size = 20)
    theme_minimal(base_size = 30) +
      theme(
        plot.background = element_rect(fill = back_color, color = back_color),
        panel.background = element_rect(fill = grid_color, color = grid_color),
        plot.title = element_text(size = rel(1.5), hjust = .5,vjust = 2),
        plot.subtitle = element_text(size = rel(.8), hjust = .5),
        plot.caption = element_markdown(lineheight = 1.2),
        axis.title.x = element_text(size = rel(1)),
        #axis.title.y = element_text(size = rel(1)),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = rel(1)),
        #axis.text.y.left = element_text(size = rel(1.2)),
        axis.text.y = element_blank(),
        #axis.text.y.right = element_text(size = rel(0.9)),
        legend.position = "none",
        panel.grid = element_blank(),
        text = element_text(family = font_family),
        plot.margin = unit(margins_grid, "cm")
      )
  }

