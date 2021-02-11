aride <- read.csv('data/raw/aride.csv')
cousine <- read.csv('data/raw/cousine.csv')
cousin <- read.csv('data/raw/cousin.csv')
denis <- read.csv('data/raw/denis.csv')

library(magrittr)

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

saveRDS(cousine, file = 'data/processed/cousine.rds',
				compress = F, ascii = T)
saveRDS(aride, file = 'data/processed/aride.rds',
				compress = F, ascii = T)

cousin %<>% 
	dplyr::mutate(date = paste(year, '06', '15', sep = "-"), 
								date = as.POSIXct(date, tz = "Indian/Mahe")) %>%
	dplyr::filter(area == 'NW') %>%
	dplyr::select(date, p_fledged) %>%
	dplyr::mutate(location = "Cousin",
								spp_name = "White-tail tropicbird")

saveRDS(cousin, file = 'data/processed/cousin-success.rds', 
				compress = F, ascii = T)

denis %<>%
	dplyr::mutate(date = paste(Year, Month, '15'), 
								date = as.POSIXct(date, format = "%Y %b %d", tz = "Indian/Mahe"),
								n_new_nest = Fledged + Failed) %>%
	dplyr::rename(n_fledged = Fledged) %>%
	dplyr::select(date, n_new_nest, n_fledged) %>%
	dplyr::mutate(p_fledged = n_fledged/n_new_nest) %>%
	# dplyr::summarise_all(mean) %>%
	dplyr::mutate(location = "Denis",
								spp_name = "White-tail tropicbird")

saveRDS(denis, file = 'data/processed/denis.rds')
	