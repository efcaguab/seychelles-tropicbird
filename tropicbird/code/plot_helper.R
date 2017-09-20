# ggplot theme
library(ggplot2)
library(cowplot)

theme_fe <- theme_cowplot(font_size = 8, 
													font_family = "Helvetica", 
													line_size = 0.35) +
	theme(axis.ticks.length = grid::unit(-0.25, "lines"), 
				axis.text.x = element_text(margin = margin(t = 1, unit = "lines")),
				axis.text.y = element_text(margin = margin(r = 1, unit = "lines")), 
				axis.title.x = element_text(size = 8),
				axis.title.y = element_text(size = 8),
				strip.background = element_blank(),
				strip.text = element_text(size = 8),
				panel.margin = unit(c(1,0.5,0.5,0.5), 'lines'),
				plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))