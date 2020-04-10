library(magrittr)
library(ggplot2)
library(foreach)
library(dplyr)
source("./functions.R")
source("./plot_helper.R")

nest_mod <- readRDS('data/processed/new_nest_models.rds')
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

denis <- readRDS(file = 'data/processed/denis.rds') %>%
	dplyr::group_by(location) %>%
	dplyr::summarise(n_new_nest_sd = sd(n_new_nest),
									 n_new_nest = mean(n_new_nest),  
									 date = mean(date),
									 p_fledged_mean = mean(p_fledged), 
									 p_fledged_sd = sd(p_fledged)) %>%
	dplyr::mutate(location2 = location) 

mean_fits_wt <- mean_fits %>%
	filter(spp_name == "White-tail tropicbird",
				 location != "Denis") 

pm <- ggplot(mean_fits_wt, aes(x = var)) +
	# geom_line(aes(group = islet), size = 0.25, alpha = 1, show.legend = F) +
	geom_line(aes(y = exp(fit),linetype = location), 
						size = 0.5, alpha = 1, show.legend = T) +
	geom_ribbon(aes(ymin = exp(fit-se), ymax = exp(fit+se), group = location),
							colour = 'transparent', alpha = 0.2) +
	geom_point(data = denis, aes(x = date, y = n_new_nest), shape = 2) +
	geom_errorbar(data = denis, aes(x = date, ymin = n_new_nest - n_new_nest_sd, ymax = n_new_nest + n_new_nest_sd),
								linetype = 1) +
	# facet_wrap(~spp_name, scales = 'free', ncol = 2) +
	scale_x_datetime(name = '', expand = c(0,0)) +
	scale_y_continuous(name = 'nest activity\n(new nests / month)') +
	# coord_cartesian(ylim = c(0, 25)) +
	# scale_colour_brewer(palette = 'Set1', name = "", guide = guide_legend(title = NULL, direction = 'horizontal')) +
	# scale_fill_brewer(palette = 'Set1', name = "", guide = guide_legend(title = NULL, direction = 'horizontal')) +
	scale_linetype_manual(name = "", guide = guide_legend(title = NULL, direction = 'horizontal'), values = c(1,2,3,4)) +
	theme_fe 

pm



nest_mod <- readRDS('data/processed/hatching_success_models.rds')
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



nest_mod <- readRDS('data/processed/fledging_success_models.rds')
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

cousin_succ <- readRDS(file = 'data/processed/cousin-success.rds') %>%
	dplyr::filter(date > as.POSIXct("2000-01-01")) %>%
	dplyr::mutate(location2 = location) 

mean_fits_wt <- mean_fits %>%
	filter(spp_name == "White-tail tropicbird")

pmsf <- ggplot(mean_fits_wt, aes(var)) +
	# geom_line(aes(group = islet), size = 0.25, alpha = 1, show.legend = F) +
	geom_line(data = mean_fits_wt, aes(y = plogis(fit), linetype = location), 
						size = 0.5, alpha = 1, show.legend = T) +
	geom_ribbon(data = mean_fits_wt, aes(ymin = plogis(fit-se), ymax = plogis(fit+se), linetype = location),
							colour = 'transparent', alpha = 0.2) +
	# facet_wrap(~spp_name, scales = 'free', ncol = 2) +
	# geom_line(data = cousin_succ, aes(x = date, y = p_fledged/100, linetype = location)) +
	geom_point(data = cousin_succ, aes(x = date, y = p_fledged/100, shape = location2)) +
	geom_point(data = denis, aes(x = date, y = p_fledged_mean, shape = location2)) +
	geom_errorbar(data = denis, aes(x = date, ymin = p_fledged_mean - p_fledged_sd, ymax = p_fledged_mean + p_fledged_sd)) +
	scale_colour_brewer(palette = 'Set1', name = "", guide = guide_legend(title = NULL, direction = 'horizontal')) +
	scale_fill_brewer(palette = 'Set1', name = "", guide = guide_legend(title = NULL, direction = 'horizontal')) +
	scale_shape_manual(values = c(3,2), name = "", guide = guide_legend(title = NULL, direction = 'horizontal')) +
	scale_linetype_manual(name = "", guide = FALSE, values = c(1,2,3,4)) +
	# scale_linetype_manual(name = "", guide = FALSE) +
		scale_x_datetime(name = '', expand = c(0,0)) + 
	scale_y_continuous(name = 'breeding success', labels = scales::percent) + 
	theme_fe +
	# guides(linetype = guide_legend(direction = "horizontal", order = 1)) +
	theme(legend.position = 'top', legend.direction = 'vertical', legend.box = "vertical", 
				legend.margin = margin(), 
				legend.box.margin = margin())

pmsf

legend_points <- cowplot::get_legend(pmsf)
legend_lines <- cowplot::get_legend(pm)
plots <- cowplot::plot_grid(pmsf + theme(legend.position = "none"), 
												pm + theme(legend.position = "none"), ncol = 1, 
												labels = c("A", "B"), label_size = 8, 
												rel_heights = c(1, 1), 
												align = "hv")
leg <- cowplot::plot_grid(legend_lines, legend_points, ncol = 2, rel_widths = c(1,0.5))
leg

s <- cowplot::plot_grid(leg, plots, rel_heights = c(0.05, 0.95), ncol = 1)
s

cowplot::save_plot("figs/trend-all-locations.pdf", s, base_height = NULL, base_width = 3.5, base_aspect_ratio = 1/1.2)
# pdf(, width = 3.5, height = 5)
# s
# dev.off()
