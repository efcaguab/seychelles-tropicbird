library(magrittr)
library(mgcv)

cc <- readRDS(file = "./data/processed/crabs.rds")
col_event <- readRDS(file = "./data/processed/col_events.rds")
loc_dist <- readRDS(file = "./data/processed/locality_distances.rds")

cc_m <- cc %>%
  dplyr::full_join(col_event) %>%
  dplyr::filter(!is.na(locality)) %>%
  dplyr::full_join(loc_dist) %>%
  dplyr::mutate(dist_shore_l = dist_shore > 50,
                dist_shore_l = as.numeric(dist_shore_l)) %>%
  dplyr::filter(!(area == "CP" & as.numeric(locality) <= 12))

cc_m %<>%
  dplyr::mutate(month = lubridate::yday(date)) 

sexes <- c("female", "male")

m_s <- sexes <- c("female", "male") %>%
  plyr::llply(function(x){
    data <- dplyr::filter(cc_m, sex == x)
    
    mgcv::gam(t_length ~ 
                # s(as.numeric(date), k = 5) +
                s(month, bs = c("cc"), k = 12) +
                s(dist_shore, k = 3, bs = "tp") + 
                ti(month, dist_shore, k = c(12, 3), bs = c("cc", "tp")) +
                s(moon_ph, bs = "cc", k = 5),
                # s(moon_ph, bs = "cc", k = 3, by = dist_shore_l) +
                # rain, 
              data = data, 
              gamma = 1.4)
})

n <- 362/2
m_s_p <- lapply(m_s, plot, pers = T, n = n, n2 = n, pages = 1)

list(mod = m_s, 
     pred = m_s_p) %>%
  saveRDS(file = "./data/processed/size_model.rds")
