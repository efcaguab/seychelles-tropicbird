aride <- read.csv('../data/raw/aride.csv')
cousine <- read.csv('../data/raw/cousine.csv')

cousine %<>%
	dplyr::rename(date = mydate) %>%
	dplyr::mutate(date = as.POSIXct(date, format = "%d-%m-%y", tz = "Indian/Mahe")) %>%
	dplyr::rename(n_new_nest = New, 
								n_fledged = Fledged, 
								n_total = Total) %>%
	dplyr::mutate(n_hatched = NA) %>%
	dplyr::select(date, n_new_nest, n_hatched, n_fledged, n_total) %>%
	dplyr::mutate(location = "Cousine", 
								spp_name = "White-tail tropicbird")

aride %<>%
	dplyr::mutate(date = paste("15", Month, Year), 
								date = as.POSIXct(date, format = "%d %b %Y", tz = "Indian/Mahe")) %>%
	dplyr::rename(n_new_nest = New,
								n_fledged = Fledged,
								n_total = Total) %>%
	dplyr::mutate(n_hatched = n_total - Failed.at.Egg) %>% 
	dplyr::select(date, n_new_nest, n_hatched, n_fledged, n_total) %>%
	dplyr::mutate(location = "Aride", 
								spp_name = "White-tail tropicbird")

saveRDS(cousine, file = '../data/processed/cousine.rds',
				compress = F, ascii = T)
saveRDS(aride, file = '../data/processed/aride.rds',
				compress = F, ascii = T)
