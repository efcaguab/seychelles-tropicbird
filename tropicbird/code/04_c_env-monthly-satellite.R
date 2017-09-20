library(magrittr)
library(lubridate)
library(ggplot2)
library(maptools)
library(rgdal)
library(rgeos)

env_sat <- readRDS(file = "../data/raw/satellite/compiled_env_sat.rds")
alda <- rgdal::readOGR("../data/gis/", "aldabra_area")
alda@data$id <- rownames(alda@data)
alda.points <- fortify(alda, region="id")
alda.df <- plyr::join(alda.points, alda@data, by="id")
alda.df <- alda.df[seq(1, nrow(alda.df), 100), ]

# monthly means
env_mm <- plyr::ldply(env_sat, function(x){
	n <- names(x)
	names(x)[4] <- "value"
	x %>%
		dplyr::mutate(date = as.Date(time),
									month = month(date)) %>%
		dplyr::group_by(month, lat, lon) %>%
		dplyr::summarise(valid = sum(!is.na(value)),
										 value = mean(value, na.rm = T)) %>%
		dplyr::mutate(var = n[4],
									value = replace(value, valid <= 5, NA))
})

metainfo <- data.frame(var = c("productivity", 
															 "analysed_sst",
															 "sstAnom",
															 "w"),
											 name = c("Net Primary Carbon Productivity",
											 				 "Sea Surface Temperature",
											 				 "Sea Surface Tempearture Anomaly",
											 				 "Wind Speed"),
											 units = c("mg C m-2 day-1",
											 					"ºC",
											 					"ºC",
											 					" m s-1"))

pdf(file = "../data/processed/satellite_variables.pdf", width = 11, height = 8.5)
plyr::d_ply(env_mm, "var", function(x){
	p <- ggplot(x, aes(x = lon, y = lat)) +
		geom_tile(aes(fill = value)) +
		geom_polygon(data = alda.df, aes(long, lat), colour = "black", fill = "grey60") +
		facet_wrap(~month) +
		scale_fill_gradient2(high = '#e41a1c', low = '#377eb8', 
												 midpoint = median(x$value, na.rm = T), 
												 na.value = "grey60",
												 name = metainfo$units[metainfo$var == x$var[1]]) +
		scale_x_continuous(expand = c(0,0)) +
		scale_y_continuous(expand = c(0,0)) +
		coord_quickmap() +
		ggtitle(metainfo$name[metainfo$var == x$var[1]])
	print(p)
})
dev.off()


