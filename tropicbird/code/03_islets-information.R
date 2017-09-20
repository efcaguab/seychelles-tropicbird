library(maptools)
library(foreach)
library(doMC)
library(magrittr)
registerDoMC(cores = 4)

islets <- readShapePoly("../data/gis/islets.shp") 
picard <- readShapePoly("../data/gis/picard.SHP") 

areas <- geosphere::areaPolygon(islets)

min_dist <- foreach(i=1:length(islets@polygons), .combine = c) %dopar% {
  distance <- geosphere::dist2Line(islets@polygons[[i]]@Polygons[[1]]@coords, 
                       picard@polygons[[1]]@Polygons[[1]]@coords) 
    distance[, 1] %>% min()
}
 
islets@data %<>%
  dplyr::mutate(area = areas,
                dist_picard = min_dist) %>%
  dplyr::rename(Name. = islets)

rat_evidence <- read.csv("../data/raw/islet_rat_evidence.csv")

islets_data <- islets@data %>%
  dplyr::right_join(rat_evidence)

islets_data[islets_data$Name. == "Picard", ]$area <- mean(islets_data$area)

saveRDS(islets, "../data/processed/islets.rds")
