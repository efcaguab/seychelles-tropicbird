library(magrittr)
library(mgcv)

cc <- readRDS(file = "./data/processed/crabs.rds")
col_event <- readRDS(file = "./data/processed/col_events.rds")
loc_dist <- readRDS(file = "./data/processed/locality_distances.rds")

breaks <- c(0, 20, 30, 80)

m_r_c <- breaks[-1] %>%
  plyr::llply(function(x){
    data <- cc %>%
      dplyr::mutate(t_bin = cut(t_length, breaks = breaks, labels = breaks[-1]),
                    t_bin = as.numeric(as.character(t_bin))) %>% 
      dplyr::group_by(col_id) %>% 
      dplyr::summarise(count = sum(t_bin == x)) %>% 
      dplyr::full_join(col_event) %>%
      dplyr::mutate(month = lubridate::yday(date)) %>%
      dplyr::filter(area %in% c("BP", "CP"))
    
    mgcv::gam(count ~ 
                s(as.numeric(date), k = 5) +
                s(month, k = 10, bs = "cc") +
                s(moon_ph, k = 10, bs = "cc"), 
              family = nb(),
              data = data , 
              gamma = 1.4)
  })

m_r_p <- breaks[-1] %>%
  plyr::llply(function(x){
    data <- cc %>%
      dplyr::mutate(t_bin = cut(t_length, breaks = breaks, labels = breaks[-1]),
                    t_bin = as.numeric(as.character(t_bin))) %>% 
      dplyr::group_by(col_id) %>% 
      dplyr::summarise(count = sum(t_bin == x), 
                       no_count = n() - count) %>% 
      dplyr::full_join(col_event) %>%
      dplyr::mutate(month = lubridate::yday(date)) %>%
      dplyr::filter(area %in% c("BP", "CP"))
    
    mgcv::gam(cbind(count, no_count) ~ 
                s(as.numeric(date), k = 5) +
                s(month, k = 10, bs = "cc") +
                s(moon_ph, k = 10, bs = "cc"), 
              family = "binomial",
              data = data , 
              gamma = 1.4)
  })

plot(m_r_c[[3]])
plot(m_r_p[[3]])
