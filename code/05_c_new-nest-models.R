library(magrittr)
library(mgcv)
library(dplyr)
source("./functions.R")
n <- 362/2

new_nest <- readRDS(file = "../data/processed/new_nest.rds")
other_nest <- readRDS('../data/processed/new_nest_other.rds')
new_nest <- dplyr::bind_rows(new_nest, other_nest)

models_islet <- new_nest %>% plyr::dlply(c('spp_name', 'location'),function(y){
	y %>% plyr::dlply('islet', function(x){
		x %<>%
			mu(date = cut(date, 'month'), 
				 date = as.POSIXct(date, tz = 'Indian/Mahe')) %>%
			gr(date) %>%
			su(n_new_nest = sum(n_new_nest)) %>%
			ungroup() %>%
			mu(month = lubridate::month(date),
				 date = date, 
				 date_n = as.numeric(date))
		
		gam(n_new_nest ~ s(month, k = 6, bs = "cc") + s(date_n, k = 6, bs = "tp"),
				gamma = 1.4, family = 'poisson', data = x)
	})
})


models_general <- new_nest %>% plyr::dlply(c('spp_name', 'location'),function(x){
		x %<>%
			mu(date = cut(date, 'month'), 
				 date = as.POSIXct(date, tz = 'Indian/Mahe')) %>%
			gr(date) %>%
			su(n_new_nest = sum(n_new_nest)) %>%
			ungroup() %>%
			mu(month = lubridate::month(date),
				 date = date , 
				 date_n = as.numeric(date))
		
		gam(n_new_nest ~ s(month, k = 6, bs = "cc") + s(date_n, k = 6, bs = "tp"), 
				gamma = 1.4, family = 'poisson', data = x)
})

saveRDS(list(per_islet = models_islet, mean = models_general),
				'../data/processed/new_nest_models.rds')

new_nest %>%
	mu(date = cut(date, 'month'), 
		 date = as.POSIXct(date, tz = 'Indian/Mahe')) %>%
	gr(date, spp_name, location) %>%
	su(n_new_nest = sum(n_new_nest)) %>%
	ungroup() %>%
	mu(month = lubridate::month(date),
		 date = date , 
		 date_n = as.numeric(date)) %>%
	dplyr::rename(date_numeric = date_n) %>%
	dplyr::filter(spp_name == "White-tail tropicbird") %>% 
	readr::write_csv("new_nest_model_data.csv")
