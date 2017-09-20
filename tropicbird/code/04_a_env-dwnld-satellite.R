library(rerddap)
library(magrittr)
library(foreach)
library(lubridate)
library(plyr)

all_tb <- readRDS('../data/processed/master_df.rds')
source('./functions.R')

min_date <- min(all_tb$date)
lubridate::day(min_date) <- 1
min_date <- as.Date(min_date)
max_date <- max(all_tb$date)
m <- lubridate::month(max_date) + 1
lubridate::month(max_date) <-  m
lubridate::day(max_date) <- 1
max_date <- max_date - 24*3600
max_date <- as.Date(max_date)

# 200 km in all directions from the nests
bbox <- list(w = 44.3, e = 48.1, s = -10.8, n = -8)

datasets <- tibble::data_frame(
	var = c('sst', 'sst_a', 'npp', 'wind'),
	var_id = c('analysed_sst', 'sstAnom', 'productivity', 'w'),
	id = c('jplMURSST41', 'jplMURSST41anom1day', 'erdMH1pp1day','ncdcOwDly'),
	stride = c(25, 25, 3, 1)) 

# for each dataset
d_ply(datasets, "var", function(x){
	message('Reading ERDDAP info for ', x$var[1])
	ds_info <- info(x$id[1])
	ds_times <- ds_info$alldata$time[3, "value"] %>% 
		strsplit(", ") %>% unlist() %>% as.numeric() %>%
		divide_by(3600*24) + as.Date('1970-01-01')
	# determine start and end time
	ds_times[1] <- max(ds_times[1], min_date)
	ds_times[2] <- min(ds_times[2], max_date)
	periods <- tibble::data_frame(
		start = seq(ds_times[1], ds_times[2], 'year')) %>%
		mu(end = dplyr::lead(start, default = ds_times[2]))
	# determine download periods based on the stride
	periods <- foreach(i=1:x$stride[1], .combine = rbind) %do% {
		periods %>%
			mu(start = start + i - 1, end = end + i - 1)
	} %>%
		mu(end = replace(end, end > ds_times[2], ds_times[2]))
	# for each period
	message('Downloading and saving data...')
	d_ply(periods, 'start', function(y){
		file <- paste0("../data/raw/satellite/",
									 x$var[1], "/", 
									 x$id[1], "_",
									 "stride", x$stride, "_",
									 y$start[1],"_",
									 y$end[1], ".rds")
		if(!file.exists(file)){
			data <- griddap(ds_info, 
											fields = x$var_id[1],
											time = c(y$start[1], y$end[1]),
											latitude = c(bbox$s, bbox$n),
											longitude = c(bbox$e, bbox$w),
											stride = x$stride[1]) 
			saveRDS(data, file = file, 
							compress = FALSE, ascii = TRUE)
		}
	}, .progress = "text")
})

# read downloaded data and compile into one file
message("compiling data...")
env_sat <- dlply(datasets, 'var', function(x){
	list.files(paste0("../data/raw/satellite/", x$var[1]), full.names = T) %>%
		ldply(function(y){
			readRDS(file = y) %$%
				data
		})
}, .progress = "text")

saveRDS(env_sat, file = "../data/raw/satellite/compiled_env_sat.rds", 
				compress = T, ascii = T)
