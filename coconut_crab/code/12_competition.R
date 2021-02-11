library(unmarked)
library(magrittr)
library(foreach)
library(ggplot2)

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

cc %>%
  dplyr::inner_join(col_event) %>%
  fi(!is.na(locality)) %>%
  gr(date, locality) %>%
  su(lm = sum(t_length > 50 & sex == "male"),
     sm = sum(t_length < 30 & sex == "male"),
     fe = sum(sex == "female")) %>%
  fi(!(lm == 0 & sm == 0), 
     !(lm == 0 & fe == 0),
     !(sm == 0 & fe == 0)) %>%
  mu(month = lubridate::month(date)) %>%
  ggplot(aes(x = lm, y = sm, colour = month)) + 
  geom_count() +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) + 
  facet_wrap(~month) 