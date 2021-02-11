library(magrittr)
library(ggplot2)
library(foreach)
library(dplyr)
source("./functions.R")
source("./plot_helper.R")

nest_mod <- readRDS('../data/processed/new_nest_models.rds')
library(mgcv)

# generate plot data for the islets
n <- 362/2
islet_fits <- nest_mod$per_islet %>%
	lapply(function(x){
		x %>% lapply(plot, pages = 1, pers = T, n = n, n2 = n)
	})
# extract plot data for the islets
islet_fits <- foreach(i=1:length(islet_fits)) %do% {
	extract_islet_fits(islet_fits[[i]], nest_mod$per_islet[[i]], add_intercept = T) %>%
		lapply(function(y) mu(y, spp_name = names(islet_fits)[i]))
} %>%
	revert_list() %>%
	lapply(function(x){
		dplyr::bind_rows(x[[1]], x[[2]])
	})

# generate plot data for the mean new nest
mean_fits <- nest_mod$mean %>%
	lapply(plot, pages = 1, pers = T, n = n, n2 = n) 
# extract mean plot data
mean_fits <- extract_islet_fits(mean_fits, nest_mod$mean, se = T) %>%
	lapply(function(x){
		x %>% re(spp_name = islet)
	})

var <- 'month'

islet_fits <- islet_fits[var][[1]]
mean_fits <- mean_fits[var][[1]]

mean_fits %<>%
	tidyr::separate(spp_name, into = c('spp_name', 'location'), sep = '\\.')

mean_fits_wt <- mean_fits %>%
	filter(spp_name == "White-tail tropicbird") 

pm <- mean_fits_wt %>%
	ggplot(aes(var, exp(fit))) +
	# geom_line(aes(group = islet), size = 0.25, alpha = 1, show.legend = F) +
	geom_line(data = mean_fits_wt, aes(linetype = location), 
						size = 0.5, alpha = 1, show.legend = T) +
	geom_ribbon(aes(ymin = exp(fit-se), ymax = exp(fit+se), group = location),
							colour = 'transparent', alpha = 0.2) +
	# facet_wrap(~spp_name, scales = 'free', ncol = 2) +
	scale_x_continuous(breaks = seq(1, 12, by = 2), labels = month.abb[seq(1, 12, by = 2)], 
										 name = '', expand = c(0,0)) +
	scale_y_continuous(name = 'nest density\n(new nests / month)') +
	# scale_colour_brewer(palette = 'Set1', name = "", guide = guide_legend(title = NULL, direction = 'horizontal')) +
	# scale_fill_brewer(palette = 'Set1', name = "", guide = guide_legend(title = NULL, direction = 'horizontal')) +
	scale_linetype_manual(name = "", guide = guide_legend(title = NULL, direction = 'horizontal'), values = c(1,2,3)) +
	# guides(fill = guide_legend(direction = 'horizontal', ncol = 1)) +
	theme_fe +
	theme(legend.position = 'top', legend.direction = 'vertical', legend.box = "vertical") 
				# legend.margin = margin(), 
				# legend.box.margin = margin())

pp <- cowplot::plot_grid(get_legend(pm), 
									 pm + theme(legend.position = "none"), ncol = 1, rel_heights = c(0.05, 0.95))

cowplot::save_plot("../figs/seasonality-new-nest.pdf", pp, base_height = NULL, base_width = 3.5, base_aspect_ratio = 2/1.2)
# pdf("../figs/seasonality-new-nest.pdf", width = 3.5, height = 2.5)
# pm
# dev.off()


nest_mod <- readRDS('../data/processed/hatching_success_models.rds')
library(mgcv)

# generate plot data for the islets
n <- 362/2
islet_fits <- nest_mod$per_islet %>%
	lapply(function(x){
		x %>% lapply(plot, pages = 1, pers = T, n = n, n2 = n)
	})
# extract plot data for the islets
islet_fits <- foreach(i=1:length(islet_fits)) %do% {
	extract_islet_fits(islet_fits[[i]], nest_mod$per_islet[[i]], add_intercept = F) %>%
		lapply(function(y) mu(y, spp_name = names(islet_fits)[i]))
} %>%
	revert_list() %>%
	lapply(function(x){
		dplyr::bind_rows(x[[1]], x[[2]])
	})

# generate plot data for the mean new nest
mean_fits <- nest_mod$mean %>%
	lapply(plot, pages = 1, pers = T, n = n, n2 = n) 
# extract mean plot data
mean_fits <- extract_islet_fits(mean_fits, nest_mod$mean, se = T, add_intercept = T) %>%
	lapply(function(x){
		x %>% re(spp_name = islet)
	})

var <- 'month'

islet_fits <- islet_fits[var][[1]]
mean_fits <- mean_fits[var][[1]]

pms <- ggplot(mean_fits, aes(var, colour = spp_name, fill = spp_name)) +
	# geom_line(aes(group = islet), size = 0.25, alpha = 1, show.legend = F) +
	geom_line(data = mean_fits, aes(y = plogis(fit)), 
						size = 0.5, alpha = 1, show.legend = F) +
	geom_ribbon(data = mean_fits, aes(ymin = plogis(fit-se), ymax = plogis(fit+se)),
							colour = 'transparent', alpha = 0.2) +
	# facet_wrap(~spp_name, scales = 'free', ncol = 2) +
	scale_x_continuous(breaks = seq(1, 365, by = 30.5), labels = month.abb, 
										 name = '', expand = c(0,0)) +
	scale_y_continuous(name = 'hatching success', labels = scales::percent) +
	scale_colour_brewer(palette = 'Set1') +
	scale_fill_brewer(palette = 'Set1') +
	theme_fe +
	theme(legend.position = 'none')


nest_mod <- readRDS('../data/processed/fledging_success_models.rds')
library(mgcv)

# generate plot data for the islets
n <- 362/2
islet_fits <- nest_mod$per_islet %>%
	lapply(function(x){
		x %>% lapply(plot, pages = 1, pers = T, n = n, n2 = n)
	})
# extract plot data for the islets
islet_fits <- foreach(i=1:length(islet_fits)) %do% {
	extract_islet_fits(islet_fits[[i]], nest_mod$per_islet[[i]], add_intercept = F) %>%
		lapply(function(y) mu(y, spp_name = names(islet_fits)[i]))
} %>%
	revert_list() %>%
	lapply(function(x){
		dplyr::bind_rows(x[[1]], x[[2]])
	})

# generate plot data for the mean new nest
mean_fits <- nest_mod$mean %>%
	lapply(plot, pages = 1, pers = T, n = n, n2 = n) 
# extract mean plot data
mean_fits <- extract_islet_fits(mean_fits, nest_mod$mean, se = T, add_intercept = T) %>%
	lapply(function(x){
		x %>% re(spp_name = islet)
	})

var <- 'month'

islet_fits <- islet_fits[var][[1]]
mean_fits <- mean_fits[var][[1]]

pmsf <- ggplot(mean_fits, aes(var, colour = spp_name, fill = spp_name)) +
	# geom_line(aes(group = islet), size = 0.25, alpha = 1, show.legend = F) +
	geom_line(data = mean_fits, aes(y = plogis(fit)), 
						size = 0.5, alpha = 1, show.legend = F) +
	geom_ribbon(data = mean_fits, aes(ymin = plogis(fit-se), ymax = plogis(fit+se)),
							colour = 'transparent', alpha = 0.2) +
	# facet_wrap(~spp_name, scales = 'free', ncol = 2) +
	scale_x_continuous(breaks = seq(1, 365, by = 30.5), labels = month.abb, 
										 name = '', expand = c(0,0)) +
	scale_y_continuous(name = 'fledging success', labels = scales::percent) +
	scale_colour_brewer(palette = 'Set1') +
	scale_fill_brewer(palette = 'Set1') +
	theme_fe +
	theme(legend.position = 'none')

# 
# p <- ggdraw() +
# 	draw_plot(pw[[1]], x = 0,   y = 0.666, width = 0.5, height = 0.333) +
# 	draw_plot(pw[[2]], x = 0.5, y = 0.666, width = 0.5, height = 0.333) +
# 	draw_plot(pm,      x = 0,   y = 0.333, width = 1,   height = 0.333) +
# 	draw_plot(pms,     x = 0,   y = 0,     width = 1,   height = 0.333)

# pdf("../figs/seasonality.pdf", width = 7, height = 5)
# p
# dev.off()

grobs <- ggplotGrob(pm)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

s <- cowplot::plot_grid(pm + theme(legend.position = "none"), 
												pms, pmsf, ncol = 1, 
												labels = c("A", "B", "C"), label_size = 8, 
												rel_heights = c(1, 1, 1), 
												align = "hv")
s <- cowplot::plot_grid(legend, s, rel_heights = c(0.05, 1), ncol = 1)
pdf("../figs/seasonality.pdf", width = 3.5, height = 5)
s
dev.off()
