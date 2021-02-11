library(magrittr)

mu <- dplyr::mutate
se <- dplyr::select
gr <- dplyr::group_by
fi <- dplyr::filter
su <- dplyr::summarise
re <- dplyr::rename
ar <- dplyr::arrange

dens <- readRDS(file = "./data/processed/pop_density.rds")
col_event <- readRDS(file = "./data/processed/col_events.rds")

col_event_date <- col_event %>%
  # fi(!is.na(rain)) %>%
  gr(date) %>%
  su(moon_ph = mean(moon_ph),
     rain = rain[1])

m_d_d <- dens %>%
  mu(date = as.Date(date),
     yday = lubridate::yday(date),
     year = lubridate::year(date)) %>%
  dplyr::inner_join(col_event_date)

m_d <- mgcv::gam(density ~ s(yday, bs = "cc")  + s(as.numeric(date)), data = m_d_d)

summary(m_d)
n <- 362
m_d_p <- plot(m_d, n = n, pages = 1)


list(mod = m_d, 
     pred = m_d_p) %>%
  saveRDS(file = "./data/processed/density_model.rds")



