library(magrittr)
library(mgcv)

cc <- readRDS(file = "./data/processed/crabs.rds")
col_event <- readRDS(file = "./data/processed/col_events.rds")
loc_dist <- readRDS(file = "./data/processed/locality_distances.rds")

cc_m <- cc %>%
  dplyr::full_join(col_event) %>%
  dplyr::filter(!is.na(locality)) %>%
  dplyr::full_join(loc_dist)

cc_m %<>%
  dplyr::mutate(sex_l = sex == "male",
                month = lubridate::yday(date),
                dist_shore_l = as.numeric(dist_shore > 50)) 

m_r <- mgcv::gam(sex_l ~ 
                   # s(as.numeric(date), k = 5) +
                   s(month, k = 12, bs = "cc") +
                   s(month, k = 12, bs = "cc", by = dist_shore_l) +
                   s(moon_ph, k = 5, bs = "cc"),
                 data = cc_m, 
                 select = cc_m$t_length > 28,
                 family = "binomial", 
                 gamma = 1.4)

n <- 100
m_r_p <- plot(m_r, pers = T, n = n, n2 = n, pages = 1)


list(mod = m_r, 
     pred = m_r_p) %>%
  saveRDS(file = "./data/processed/sex_ratio_model.rds")
saveRDS(cc_m, file = "./data/processed/crab_master_df.rds")
