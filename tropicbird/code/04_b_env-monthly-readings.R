library(foreach)
library(lubridate)
library(magrittr)

source("./functions.R")
 	
# 2009-2015 ---------------------------------------------------------------

# get file list from 2009-2015
files <- "../data/raw/readings/legacy" %>%
	list.files(full.names = T) %>%
	as.list()

years <- stringr::str_extract(files, "([0-9])+") %>% as.numeric() %>%
	as.list()

# for each year check the headings, in 2010 the column time was added
col_names <- c(
	list(c('date', 'temp_db','temp_wb', 'temp_max', 'temp_min', 'wind_dir',
							 'wind_mean', 'wind_max', 'rain', 'obs', 'rain_cor')),
	list(c('date', 'time', 'temp_db','temp_wb', 'temp_max', 'temp_min', 'wind_dir',
				 'wind_mean', 'wind_max', 'rain', 'obs', 'rain_cor')) %>%
		rep(length(files)-1)
	) 

# read everything as text for now to avoid inconsistencies
col_types <- col_names %>% 
	lapply(function(x) do.call(paste0, as.list(rep('c', times = length(x)))))

# specify date format for each file, some of them had to be changed manually
# because they had different formats across months
date_format <- c(rep('%d-%b', 2), rep("%Y-%m-%d", 5))

# specify the number of header lines to skip in each file
skip_rows <- c(rep(2, 3), rep(1, 4))

# for each file
d_2009_2015 <- foreach(i = 1:length(files), .combine = dplyr::bind_rows) %do% {
	# for each month (sheet in the excel file)
	y_measures <- foreach(j = 1:12, .combine = dplyr::bind_rows) %do% {
		gdata::xls2csv(files[[i]][1], sheet = j) %>%
			readr::read_csv(skip = skip_rows[i], 
											col_names = FALSE,	col_types = col_types[[i]],
											n_max = 31, na  = c("", "X", "x", "NA", "nd"))
	} %>% 
		`names<-`(col_names[[i]]) %>%  # change names accordingly
		dplyr::mutate(date = as.Date(date, format = date_format[[i]]))
	year(y_measures$date) <- years[[i]]  # fix date
	return(y_measures)
}

# 2016 --------------------------------------------------------------------

names_2016 <- c('date', 'time', 'temp_db','temp_wb', 'temp_max', 'temp_min', 'wind_dir',
								'rain', 'obs', 'notes')
d_2016 <- gdata::read.xls("../data/raw/readings/current/Met readings 2016.xls",
													skip = 1, header = F) %>%
	`names<-`(names_2016) %>%
	extract( , 1:length(names_2016)) %>%
	lapply(as.character) %>%
	as.data.frame() %>%
	dplyr::mutate(date = as.Date(date)) 


# merge and fix inconsistencies -------------------------------------------

mr <- dplyr::bind_rows(d_2009_2015, d_2016) 
find_negative <- function(x){
	replace(x, x < 0, NA)
}

mr %<>%
	dplyr::filter(!is.na(date)) %>%
	dplyr::mutate(rain = replace(rain, 
															 stringi::stri_detect(rain, regex = "(?i)trace"), 
															 "0.1"),
								rain = stringi::stri_extract(rain, regex = "[[:digit:]]+\\.*[[:digit:]]*"),
								rain_cor = replace(rain_cor, 
															 stringi::stri_detect(rain_cor, regex = "(?i)trace"), 
															 "0.1"),
								rain_cor = stringi::stri_extract(rain_cor, regex = "[[:digit:]]+\\.*[[:digit:]]*")) %>%
	dplyr::mutate_at(dplyr::vars(dplyr::matches("temp")), as.numeric) %>%
	dplyr::mutate_at(dplyr::vars(dplyr::matches("temp")), find_negative) %>%
	dplyr::mutate_at(dplyr::vars(dplyr::matches("rain")), as.numeric) %>%
	# there were problems with the rain back in 2009 between April 15 and Sept 05
	# so we will use corrected rainfall for that period
	mu_c(date >= as.Date("2009-04-15") & date <= as.Date("2009-09-05"), 
			 rain = rain_cor)

saveRDS(mr, file = "../data/processed/env_monthly_readings.rds", 
				compress = F, 
				ascii = T)

