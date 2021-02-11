library(magrittr)
library(mgcv)

cc <- readRDS(file = "./data/processed/crabs.rds")
col_event <- readRDS(file = "./data/processed/col_events.rds")
loc_dist <- readRDS(file = "./data/processed/locality_distances.rds")

sexes <- c("female", "male")

m_m <- sexes %>%
  plyr::llply(function(x){
    data <- cc %>%
      dplyr::filter(t_length > 30, 
                    sex == x) %>%
      dplyr::group_by(col_id) %>% 
      dplyr::full_join(col_event) %>%
      dplyr::mutate(month = lubridate::yday(date)) %>%
      dplyr::filter(area %in% c("BP", "CP")) %>%
      dplyr::filter(!is.na(locality)) %>%
      dplyr::full_join(loc_dist) %>%
      dplyr::filter(!(area == "CP" & as.numeric(locality) <= 12),
                    date > as.Date("2010-01-01"))
    
    mgcv::gam(moult ~ 
                # s(as.numeric(date), k = 5) +
                # ti(month, dist_shore, bs = c("cc", "cr"), k = c(5, 3)) +
                # s(dist_shore, k = 2, bs = "cr") +
                # s(moon_ph, bs = "cc", k = 3) +
                s(month, k = 12, bs = "cc"),
              data = data , 
              gamma = 1.4)
  })

n <- 362
m_m_p <- lapply(m_m, plot, pers = T, n = n, n2 = n, pages = 1)


list(mod = m_m, 
     pred = m_m_p) %>%
  saveRDS(file = "./data/processed/moult_model.rds")


