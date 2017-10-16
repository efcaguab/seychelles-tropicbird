library(magrittr)
library(mgcv)

mu <- dplyr::mutate
se <- dplyr::select
gr <- dplyr::group_by
fi <- dplyr::filter
su <- dplyr::summarise
re <- dplyr::rename
ar <- dplyr::arrange

cc <- readRDS(file = "./data/processed/crabs.rds")
col_event <- readRDS(file = "./data/processed/col_events.rds")
loc_dist <- readRDS(file = "./data/processed/locality_distances.rds")

cc %>% dplyr::inner_join(col_event) %>%
  fi(mark != "") %$% table(area, resight)

wanderers <- cc %>% dplyr::inner_join(col_event) %>%
  fi(mark != "") %>%
  gr(mark) %>% 
  su(n_areas = dplyr::n_distinct(area),
     n_resights = sum(resight)) %>%
  ar(-n_areas) %T>% View %>%
  fi(n_areas > 1) %$% mark

cc %>% 
  dplyr::inner_join(col_event) %>%
  fi(mark %in% wanderers) %>%
  ar(mark, date) %>% 
  se(date, area_name, mark, resight, t_length) %>% View