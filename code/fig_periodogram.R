library(magrittr)
library(ggplot2)
library(foreach)
library(dplyr)
source("./functions.R")
source("./plot_helper.R")

nest_wavelet <- readRDS('../data/processed/new_nest_wav.rds')

pw <- plyr::llply(nest_wavelet, function(x){
	d <- x$series$date
	
	conf <- dplyr::data_frame(u = c(0.1, unique(wav.as.df(x$Power, x)$x), max(wav.as.df(x$Power, x)$x) + 1), 
														uu = rev(u)) %>%
		dplyr::rowwise() %>%
		dplyr::mutate(uu = min(u, uu) + 1, 
									uu = log10(uu))
	
	pvalues <- (x$Power.pval >= 0.05) %>% wav.as.df(x) %>% 
		dplyr::mutate(Freq = as.numeric(Freq))
	
	species <- paste(x$species, x$location,  sep = " -- ")
	p1 <- wav.as.df(x$Power, x) %>%
		mu(spe = species) %>%
		ggplot(aes(x = x)) +
		geom_tile(aes(fill = Freq, y = y)) +
		# stat_contour(aes(z = Freq, y = y), colour = "black", bins = 8) +
		scale_y_continuous(expand = c(0,0), 
											 breaks = log10(c(3, 6, 12, 24, 36)), 
											 labels = c("1/4", "1/2", "1", "2", "3"), 
											 limits = c(min(wav.as.df(x$Power, x)$y), max(wav.as.df(x$Power, x)$y))) +
		scale_x_continuous(expand = c(0,0), 
											 breaks = which(d - lubridate::round_date(d, "year") == 0),
											 labels = lubridate::year(d[d - lubridate::round_date(d, "year") == 0]), 
											 limits = c(min(wav.as.df(x$Power, x)$x+0.5), max(wav.as.df(x$Power, x)$x)-0.5)) +
		stat_contour(data = pvalues, aes(z = Freq, y = y, x = x), bins = 1, colour = "gray30") +
		# geom_hline(yintercept = log10(c(2, 3, 6, 12, 24)), colour = "grey20", 
		# size = 0.2, linetype = 2) +
		geom_line(data = conf, aes(x = u, y = uu), linetype = 2, alpha = 0.5) +
		scale_fill_gradientn(colours = c("#377eb8", "white", "#e41a1c"),
												 values = scales::rescale(c(min(x$Power), mean(x$Power), max(x$Power)))) +
		facet_wrap(~spe) +
		theme_fe +
		theme(legend.position = "none", axis.title = element_text(size = 10),
					# strip.text = element_text(face = "italic"),
					plot.margin = unit(c(0,0,-0.6,0), "lines")) +
		ylab("period (years)") + xlab("")
	return(p1)
})

p <- cowplot::plot_grid(plotlist = pw, ncol = 1, labels = c("A", "B", "C", "D"), label_size = 10)

pdf("../figs/periodogram.pdf", width = 3.5, height = 7)
p
dev.off()

