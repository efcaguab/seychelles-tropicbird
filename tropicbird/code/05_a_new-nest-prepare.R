library(magrittr) 

tb <- readRDS('../data/processed/master_df.rds')
effort <- readRDS('../data/processed/effort.rds')
source("./functions.R")

## Aldabra

# determine the start date of each nest
new_nest <- tb %>%
	plyr::ddply(c('spp_name', 'islet', 'nest'), function(x){
		data.frame(date = min(x$date))
	})

new_nest <- effort %>%
	plyr::ddply(c('date', 'spp_name', 'islet'), function(x){
		# count the number of new nests
		n <- new_nest %>%
			fi(date == x$date[1], spp_name == x$spp_name[1], islet == x$islet[1])
		data.frame(n_new_nest = nrow(n))
	}) 

empty_islets <- new_nest %>%
	gr(spp_name, islet) %>%
	su(n_new_nest = sum(n_new_nest)) %>%
	# fi(n_new_nest < as.numeric(difftime(max(tb$date), min(tb$date), units = 'days'))/365 * 3) %>%
	fi(n_new_nest < 20) %>%
	se(-n_new_nest)

new_nest %<>%
	dplyr::anti_join(empty_islets) %>%
	dplyr::mutate(location = "Aldabra")

saveRDS(new_nest, file = "../data/processed/new_nest.rds",
				compress = F, ascii = T)

## Other islands

cousine <- readRDS("../data/processed/cousine.rds")
aride <- readRDS("../data/processed/aride.rds")

new_nest_other <- dplyr::bind_rows(cousine, aride) %>%
	dplyr::mutate(islet = NA) %>%
	dplyr::select(date, spp_name, islet, n_new_nest, location)

saveRDS(new_nest_other, file = "../data/processed/new_nest_other.rds",
				compress = F, ascii = T)
