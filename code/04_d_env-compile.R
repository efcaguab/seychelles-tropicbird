library(magrittr)
library(doMC)
registerDoMC(cores = 4)

source("./functions.R")

islet_loc <- c(lon = 46.2, lat = -9.4)

mr <- readRDS("../data/processed/env_monthly_readings.rds")
sr <- readRDS("../data/raw/satellite/compiled_env_sat.rds")

sr <- plyr::ldply(sr, function(x) {
	names(x)[4] <- "value"
	x %>%
		mu(date = as.POSIXct(time),
			 dist = geosphere::distCosine(islet_loc, cbind(lon, lat))) %>%
		gr(date) %>%
		su(value = weighted.mean(value, 1/log(dist), na.rm = T))
}, .parallel = T) %>%
	dplyr::as_data_frame() 

env <- mr %>%
	se(date, temp_db:temp_min, rain) %>%
	tidyr::gather(var, value, temp_db:temp_min, rain) %>%
	dplyr::bind_rows(mu(sr, date = as.Date(date)))

pdf(file = "../data/processed/all_variables.pdf", width = 11, height = 8.5)
env %>%
	ggplot(aes(date, value)) +
	geom_point(size = 0.1) +
	geom_line() +
	facet_wrap(~var, scales = "free_y", ncol = 1) +
	scale_x_date(expand = c(0,0)) +
	theme_bw(base_size = 10)
dev.off()

saveRDS(env, file = "../data/processed/all_env.rds", compress = F, ascii = T)