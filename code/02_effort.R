library(magrittr)
library(dplyr)

tb <- readRDS('../data/processed/master_df.rds')
tba <- readRDS('../data/processed/tb_data_clean.rds')
source('./functions.R')

effort <- tb %>%
	gr(date, inner_outer) %>%
	su()

effort <- tba$tblInnerOuter %>%
	fj(tba$tblIslets) %>%
	se(inner_outer, islet) %>%
	fj(effort) %>%
	se(-inner_outer)

effort <- dplyr::bind_rows(effort, effort) %>% 
	mu(spp_name = rep(unique(tb$spp_name), each = nrow(effort)))

saveRDS(effort, "../data/processed/effort.rds")
