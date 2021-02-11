library(magrittr)
library(mgcv)

cc <- readRDS(file = "./data/processed/crabs.rds")
col_event <- readRDS(file = "./data/processed/col_events.rds")
loc_dist <- readRDS(file = "./data/processed/locality_distances.rds")

cc_m <- cc %>%
  dplyr::filter(sex == "female") %>%
  dplyr::mutate(egg = grepl("gg", comments) & !grepl("Dragging", comments)) %>%
  dplyr::group_by(col_id) %>%
  dplyr::summarise(egg = any(egg)) %>%
  dplyr::full_join(col_event) %>%
  dplyr::mutate(month = lubridate::yday(date)) %>%
  dplyr::filter(area %in% c("BP", "CP"))

m_e <- mgcv::gam(egg ~ 
                   # s(as.numeric(date), k = 5) +
                   # ti(month, dist_shore, bs = c("cc", "cr"), k = c(12, 3)) +
                   s(month, k = 12, bs = "cc") +
                   # s(dist_shore, k = 2, bs = "cr") +
                   s(moon_ph, bs = "cc", k = 3), 
                 data = cc_m, 
                 family = "binomial", 
                 gamma = 1.4)


n <- 362
m_e_p <- plot(m_e, pers = T, n = n, n2 = n, pages = 1)

list(mod = m_e, 
     pred = m_e_p) %>%
  saveRDS(file = "./data/processed/egg_model.rds")
