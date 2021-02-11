library(magrittr)
library(ggplot2)
library(mgcv)
cc <- readRDS(file = "./data/processed/crabs.rds")
col_event <- readRDS(file = "./data/processed/col_events.rds")
loc_dist <- readRDS(file = "./data/processed/locality_distances.rds")

cc_m <- cc %>%
  dplyr::full_join(col_event) %>%
  dplyr::filter(!is.na(locality)) %>%
  dplyr::full_join(loc_dist) %>%
  dplyr::mutate(dist_shore_l = dist_shore > 50) %>%
  dplyr::filter(!is.na(dist_shore_l)) %>%
  dplyr::filter(!(area == "CP" & as.numeric(locality) <= 12))

sexes <- c("female", "male")

m_c <- sexes %>%
  plyr::llply(function(x){
    d <- cc_m %>%
      dplyr::filter(!is.na(sex)) %>%
      dplyr::group_by(col_id, area, locality) %>% 
      dplyr::summarise(count = sum(sex %in% x)) %>% 
      dplyr::full_join(col_event) %>%
      dplyr::full_join(loc_dist) %>%
      dplyr::mutate(month = lubridate::yday(date)) %>%
      dplyr::group_by()
    
    mgcv::gam(count ~ 
                # s(as.numeric(date), k = 5) +
                s(month, k = 12, bs = "cc") +
                s(dist_shore, k = 3, bs = "tp") +
                ti(month, dist_shore, k = c(12, 3), bs = c("cc", "tp")) +
                s(moon_ph, k = 3, bs = "cc"),
                # s(moon_ph, k = 5, bs = "cc", by = dist_shore_l),
                # rain + 
                #n_people,
              family = nb(),
              data = d, 
              gamma = 1.4)
  })

n <- 362/2
m_c_p <- lapply(m_c, plot, pages = 1, pers = T, n = n, n2 = n)

list(mod = m_c, 
     pred = m_c_p) %>%
  saveRDS(file = "./data/processed/count_model.rds")
