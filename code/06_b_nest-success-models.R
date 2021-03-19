library(magrittr) 
library(mgcv)

nest_success <- readRDS("../data/processed/nest_success.rds")
source("./functions.R")


models_islet <- nest_success %>% plyr::dlply(c('spp_name'),function(y){
	y %>% plyr::dlply('islet', function(x){
		
		x %<>%
			# se(fe_e, fate_e, fate_h) %>%
			tidyr::gather(egg_or_hatchling, fate, fate_e:fate_h) %>%
			mu(date = fe_e,
				 date_n = as.numeric(date),
				 month = lubridate::yday(date)) %>%
			mu(outcome = plyr::mapvalues(fate, 
																	 c("success", "failure", "undetermined"),
																	 c(1, 0, NA)), 
				 outcome = as.numeric(outcome)) %>%
			fi(egg_or_hatchling == "fate_e", 
				 !is.na(fate))
		
		message(paste(x$spp_name[1], x$islet[1]))
		
		gam(outcome ~ s(month, k = 6, bs = "cc") + s(date_n, k = 6, bs = "tp"),
				gamma = 1.4, family = 'binomial', data = x)
	})
})

models_general <- nest_success %>% plyr::dlply(c('spp_name'),function(x){
	x %<>%
		# se(fe_e, fate_e, fate_h) %>%
		tidyr::gather(egg_or_hatchling, fate, fate_e:fate_h) %>%
		mu(date = fe_e,
			 date_n = as.numeric(date),
			 month = lubridate::yday(date)) %>%
		mu(outcome = plyr::mapvalues(fate, 
															c("success", "failure", "undetermined"),
															c(1, 0, NA)), 
			 outcome = as.numeric(outcome)) %>%
		fi(egg_or_hatchling == "fate_e", 
			 !is.na(fate))
	
	gam(outcome ~ s(month, k = 6, bs = "cc") + s(date_n, k = 6, bs = "tp"),
			gamma = 1.4, family = 'binomial', data = x)
})

saveRDS(list(per_islet = models_islet, mean = models_general),
				'../data/processed/hatching_success_models.rds')



models_islet <- nest_success %>% plyr::dlply(c('spp_name'),function(y){
	y %>% plyr::dlply('islet', function(x){
		
		x %<>%
			# se(fe_e, fate_e, fate_h) %>%
			fi(fate_e != "undertermined", 
				 fate_h != "undetermined") %>%
			tidyr::gather(egg_or_hatchling, fate, fate_e:fate_h) %>%
			mu(date = fe_e,
				 date_n = as.numeric(date),
				 month = lubridate::yday(date)) %>%
			mu(outcome = plyr::mapvalues(fate, 
																	 c("success", "failure", "undetermined"),
																	 c(1, 0, NA)), 
				 outcome = as.numeric(outcome)) %>%
			fi(egg_or_hatchling == "fate_h", 
				 !is.na(fate))
		
		message(paste(x$spp_name[1], x$islet[1]))
		
		gam(outcome ~ s(month, k = 6, bs = "cc") + s(date_n, k = 6, bs = "tp"),
				gamma = 1.4, family = 'binomial', data = x)
	})
})

models_general <- nest_success %>% plyr::dlply(c('spp_name'),function(x){
	x %<>%
		# se(fe_e, fate_e, fate_h) %>%
		fi(fate_e != "undertermined", 
			 fate_h != "undetermined") %>%
		tidyr::gather(egg_or_hatchling, fate, fate_e:fate_h) %>%
		mu(date = fe_e,
			 date_n = as.numeric(date),
			 month = lubridate::yday(date)) %>%
		mu(outcome = plyr::mapvalues(fate, 
																 c("success", "failure", "undetermined"),
																 c(1, 0, NA)), 
			 outcome = as.numeric(outcome)) %>%
		fi(egg_or_hatchling == "fate_h", 
			 !is.na(fate))
	
	gam(outcome ~ s(month, k = 6, bs = "cc") + s(date_n, k = 6, bs = "tp"),
			gamma = 1.4, family = 'binomial', data = x)
})

saveRDS(list(per_islet = models_islet, mean = models_general),
				'../data/processed/fledging_success_models.rds')


# With all other islets for hatching success

alda <- nest_success %>%
	# se(fe_e, fate_e, fate_h) %>%
	fi(fate_e != "undertermined", 
		 fate_h != "undetermined") %>%
	tidyr::gather(egg_or_hatchling, fate, fate_e:fate_h) %>%
	mu(date = fe_e,
		 date_n = as.numeric(date),
		 month = lubridate::yday(date)) %>%
	mu(outcome = plyr::mapvalues(fate, 
															 c("success", "failure", "undetermined"),
															 c(1, 0, NA)), 
		 outcome = as.numeric(outcome)) %>%
	fi(egg_or_hatchling == "fate_h", 
		 !is.na(fate)) %>%
	dplyr::select(outcome, month, date_n, spp_name) %>%
	dplyr::mutate(location = "Aldabra")

cousine <- readRDS("../data/processed/cousine.rds")
aride <- readRDS("../data/processed/aride.rds")

others <- dplyr::bind_rows(cousine, aride) %>%
	dplyr::mutate(outcome = n_fledged/n_total, 
								date_n = as.numeric(date),
								month = lubridate::yday(date)) %>%
	dplyr::select(outcome, month, date_n, spp_name, location) 

hatching_success <- dplyr::bind_rows(alda, others)

models_general <- hatching_success %>% plyr::dlply(c('spp_name', 'location'),function(x){
	gam(outcome ~ s(month, k = 6, bs = "cc") + s(date_n, k = 6, bs = "tp"),
			gamma = 1.4, family = 'binomial', data = x)
})

saveRDS(list(per_islet = models_islet, mean = models_general),
				'../data/processed/fledging_success_models.rds')

hatching_success %>% 
	dplyr::rename(day_of_year = month, date_numeric = date_n) %>% 
	dplyr::filter(spp_name == "White-tail tropicbird") %>% 
	readr::write_csv("success_model_data.csv")

