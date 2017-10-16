library(maptools)
library(foreach)
library(doMC)
library(magrittr)
registerDoMC(cores = 4)

# read data
back_path <- read.csv("./data/gis/back_path.csv") %>%
  dplyr::select(X, Y, name)
coast_path <- read.csv("./data/gis/coastal_path.csv") %>%
  dplyr::select(X, Y, name) %>%
  dplyr::mutate(name = as.character(name), 
                name = stringr::str_extract(name, "([0-9])+"),
                name = as.numeric(name))
picard <- readShapePoly("./data/gis/pi.shp") 

# calculate mid-section locations
get_mid_section <- . %>%
  dplyr::arrange(name) %>%
  dplyr::mutate(X = (X + dplyr::lag(X))/2,
                Y = (Y + dplyr::lag(Y))/2) %>%
  dplyr::filter(!is.na(X)) %>%
  dplyr::rename(locality = name)

tr_length <- function(x) {
  l <- vector(mode = "double", length = nrow(x))
  for(i in 2:nrow(x)){
    l[i] <- geosphere::distVincentyEllipsoid(c(x$X[i], x$Y[i]), 
                                             c(x$X[i-1], x$Y[i-1]))
  }
  l
}

back_path %<>%
  dplyr::mutate(length = tr_length(.)) %>%
  get_mid_section()
coast_path %<>%
  dplyr::mutate(length = tr_length(.)) %>%
  get_mid_section()

# claculate distance to shore
get_distance <- . %>%
  dplyr::select(1,2) %>%
  as.matrix() %>%
  geosphere::dist2Line(picard) %>%
  as.data.frame() %$%
  distance

back_path %<>% dplyr::mutate(dist_shore = get_distance(.))
coast_path %<>% dplyr::mutate(dist_shore = get_distance(.))

# put data together and save
back_path %<>%
  dplyr::mutate(area = "BP")
coast_path %<>%
  dplyr::mutate(area = "CP")

dplyr::bind_rows(back_path, coast_path) %>%
  dplyr::mutate(locality = as.character(locality)) %>%
  saveRDS("./data/processed/locality_distances.rds")


