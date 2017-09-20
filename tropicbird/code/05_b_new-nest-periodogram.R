library(WaveletComp)
library(magrittr)

source("./functions.R")

new_nest <- readRDS(file = "../data/processed/new_nest.rds")
other_nest <- readRDS('../data/processed/new_nest_other.rds')

new_nest <- dplyr::bind_rows(new_nest, other_nest)

nest_wavelet <- new_nest %>% plyr::ddply(c('spp_name', 'location'), function(x){
	x %>%
		mu(date = cut(date, 'month'), 
			 date = as.POSIXct(date, tz = 'Indian/Mahe')) %>%
		gr(date) %>%
		su(n_new_nest = sum(n_new_nest)) %>%
		mu(n_new_nest_log = log(n_new_nest + 1))
}) %>%
	plyr::dlply(c('spp_name', 'location'), function(x){
		wav <- analyze.wavelet(x, 
													 my.series = 'n_new_nest_log', 
													 loess.span = 0, n.sim = 100, verbose = F)
		wav$species <- x$spp_name[1]
		wav$location <- x$location[1]
		wav$sp_loc <- paste(x$spp_name[1], x$location[1], sep = ' - ')
		return(wav)
	})

saveRDS(nest_wavelet, '../data/processed/new_nest_wav.rds', compress = F, ascii = T)

# 
# other_wavelet <- other_nest %>% plyr::ddply('location', function(x){
# 	x %>%
# 		mu(date = cut(date, 'month'), 
# 			 date = as.POSIXct(date, tz = 'Indian/Mahe')) %>%
# 		gr(date) %>%
# 		su(n_new_nest = sum(n_new_nest)) %>%
# 		mu(n_new_nest_log = log(n_new_nest + 1))
# }) %>%
# 	plyr::dlply('location', function(x){
# 		wav <- analyze.wavelet(x, 
# 													 my.series = 'n_new_nest_log', 
# 													 loess.span = 0, n.sim = 100, verbose = F)
# 		wav$species <- x$spp_name[1]
# 		return(wav)
# 	})
