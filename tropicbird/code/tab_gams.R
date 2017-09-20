library(mgcv)
library(magrittr)
source("./functions.R")

new_nest <- readRDS('../data/processed/new_nest_models.rds')
hat_succ <- readRDS('../data/processed/hatching_success_models.rds')
fle_succ <- readRDS('../data/processed/fledging_success_models.rds')

models <- list(new_nest = new_nest$mean, 
							 hat_succ = hat_succ$mean,
							 fle_succ = fle_succ$mean)

table <- models %>% plyr::ldply(function(x){
	plyr::ldply(x, function(y){
		s <- summary(y)
		s$s.table %>% as.data.frame() %>% tibble::rownames_to_column() %>%
			se(-Ref.df) %>% 
			mu(dev = 1 - y$deviance / y$null.deviance,
				 r = s$r.sq,
				 dev = round(dev*100), 
				 edf = round(edf, 1),
				 Chi.sq = round(Chi.sq, 1))
	})
}) %>%
	`names<-`(c("model", "species", "var", "edf", "X.square", "p", "dev", "R")) %>%
	mu(model = plyr::mapvalues(model, 
														 c("new_nest", "hat_succ", "fle_succ"),
														 c("new nests", "hatching success", "fledging success")),
		 var = plyr::mapvalues(var, 
		 											c("s(month)", "s(date_n)"),
		 											c("p.season", "p.date")), 
		 p = round(p, 3), 
		 R = round(R, 2)) %>%
	se(species, model, R, var, p) %>%
	tidyr::spread(var, p) %>%
	ar(species, dplyr::desc(model))

write.csv(table, "../tabs/tab_summary_gams.csv", row.names = F)