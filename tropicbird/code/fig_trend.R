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

var <- 'date'

islet_fits <- islet_fits[var][[1]]
mean_fits <- mean_fits[var][[1]]

mean_fits %<>%
	mu(var = as.POSIXct(var, origin = "1970-01-01", tz = "Indian/Mahe")) %<>%
	tidyr::separate(spp_name, into = c('spp_name', 'location'), sep = '\\.')

islet_fits %<>%
	mu(var = as.POSIXct(var, origin = "1970-01-01", tz = "Indian/Mahe") )

denis <- readRDS(file = '../data/processed/denis.rds') %>%
	dplyr::mutate(location2 = location)

pm <- ggplot(mean_fits, aes(var, exp(fit), colour = spp_name, fill = spp_name, linetype = location)) +
	# geom_line(aes(group = islet), size = 0.25, alpha = 1, show.legend = F) +
	geom_line(data = mean_fits, aes(y = exp(fit)), 
						size = 0.5, alpha = 1, show.legend = T) +
	geom_ribbon(data = mean_fits, aes(ymin = exp(fit-se), ymax = exp(fit+se)),
							colour = 'transparent', alpha = 0.2) +
	geom_point(data = denis, aes(x = date, y = n_new_nest), shape = 2) +
	# facet_wrap(~spp_name, scales = 'free', ncol = 2) +
	scale_x_datetime(name = '', expand = c(0,0)) +
	scale_y_continuous(name = 'new nests (month)') +
	scale_colour_brewer(palette = 'Set1', name = "", guide = guide_legend(title = NULL, direction = 'horizontal')) +
	scale_fill_brewer(palette = 'Set1', name = "", guide = guide_legend(title = NULL, direction = 'horizontal')) +
	scale_linetype_manual(name = "", guide = guide_legend(title = NULL, direction = 'horizontal'), values = c(1,2,3,4)) +
	theme_fe +
	theme(legend.position = 'none')

pm



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

var <- 'date'

islet_fits <- islet_fits[var][[1]]
mean_fits <- mean_fits[var][[1]]

mean_fits %<>%
	mu(var = as.POSIXct(var, origin = "1970-01-01", tz = "Indian/Mahe"))
islet_fits %<>%
	mu(var = as.POSIXct(var, origin = "1970-01-01", tz = "Indian/Mahe") )

pms <- ggplot(mean_fits, aes(var, colour = spp_name, fill = spp_name)) +
	# geom_line(aes(group = islet), size = 0.25, alpha = 1, show.legend = F) +
	geom_line(data = mean_fits, aes(y = plogis(fit)), 
						size = 0.5, alpha = 1, show.legend = F) +
	geom_ribbon(data = mean_fits, aes(ymin = plogis(fit-se), ymax = plogis(fit+se)),
							colour = 'transparent', alpha = 0.2) +
	# facet_wrap(~spp_name, scales = 'free', ncol = 2) +
	scale_x_datetime(name = '', expand = c(0,0)) +
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

var <- 'date'

islet_fits <- islet_fits[var][[1]]
mean_fits <- mean_fits[var][[1]]

mean_fits %<>%
	mu(var = as.POSIXct(var, origin = "1970-01-01", tz = "Indian/Mahe")) %<>%
	tidyr::separate(spp_name, into = c('spp_name', 'location'), sep = '\\.')
islet_fits %<>%
	mu(var = as.POSIXct(var, origin = "1970-01-01", tz = "Indian/Mahe") )

cousin_succ <- readRDS(file = '../data/processed/cousin-success.rds') %>%
	dplyr::filter(date > as.POSIXct("2000-01-01")) %>%
	dplyr::mutate(location2 = location) 

pmsf <- ggplot(mean_fits, aes(var, colour = spp_name, fill = spp_name)) +
	# geom_line(aes(group = islet), size = 0.25, alpha = 1, show.legend = F) +
	geom_line(data = mean_fits, aes(y = plogis(fit), linetype = location), 
						size = 0.5, alpha = 1, show.legend = T) +
	geom_ribbon(data = mean_fits, aes(ymin = plogis(fit-se), ymax = plogis(fit+se), linetype = location),
							colour = 'transparent', alpha = 0.2) +
	# facet_wrap(~spp_name, scales = 'free', ncol = 2) +
	# geom_line(data = cousin_succ, aes(x = date, y = p_fledged/100, linetype = location)) +
	geom_point(data = cousin_succ, aes(x = date, y = p_fledged/100, shape = location2)) +
	geom_point(data = denis, aes(x = date, y = p_fledged, shape = location2)) +
	scale_colour_brewer(palette = 'Set1', name = "", guide = guide_legend(title = NULL, direction = 'horizontal')) +
	scale_fill_brewer(palette = 'Set1', name = "", guide = guide_legend(title = NULL, direction = 'horizontal')) +
	scale_shape_manual(values = c(1,2), name = "", guide = guide_legend(title = NULL, direction = 'horizontal')) +
	scale_linetype_manual(name = "", guide = guide_legend(title = NULL, direction = 'horizontal'), values = c(1,2,3,4)) +
	scale_x_datetime(name = '', expand = c(0,0)) + 
	scale_y_continuous(name = 'fledging success', labels = scales::percent) + 
	theme_fe +
	theme(legend.position = 'top', legend.direction = 'vertical', legend.box = "vertical", 
				legend.margin = margin(), 
				legend.box.margin = margin())

pmsf

grobs <- ggplotGrob(pmsf)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

s <- cowplot::plot_grid(pm, 
												pmsf + theme(legend.position = "none"), ncol = 1, 
												labels = c("A", "B"), label_size = 8, 
												rel_heights = c(1, 1), 
												align = "hv")
s <- cowplot::plot_grid(legend, s, rel_heights = c(0.4, 1), ncol = 1)
pdf("../figs/trend-all-locations.pdf", width = 3.5, height = 5)
s
dev.off()
