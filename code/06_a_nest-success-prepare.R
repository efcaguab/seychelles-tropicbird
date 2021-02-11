library(magrittr) 
library(dplyr)
tb <- readRDS('../data/processed/master_df.rds')
effort <- readRDS('../data/processed/effort.rds')
nest_periods <- read.csv("../data/raw/nest_periods.csv")
source("./functions.R")

nests <- tb %>%
	fi(spp_name != "No data") %>%
	plyr::ddply(c("islet", "nest", "spp_name"), function(x){
	
		# get all the dates in which the islet was visited
		survey <- effort %>%
			fi(islet == x$islet[1], 
				 spp_name == x$spp_nam[1]) %>%
			ar(date) %$% date
		
		# simplify if it was an egg or a hatcling
		x %<>% ar(date) %>%
			fi(!is.na(nest_status)) %>%
			mu(state = plyr::mapvalues(nest_status, 
																 c("e", "c1", "c2", "c3", "ep", "cd", "en", "not found"),
																 c("e", rep("ht", 3), rep(NA, 4)), 
																 warn_missing = F)) %>%
			fi(!is.na(state))
		
		## find all nest attributes for both egg and hatchling times
		l <- list()
		# fist egg encounter
		l$fe_e <- if ("e" %in% x$state) first(x$date[x$state == "e"]) else NA
		# visit previous to first egg encounter
		l$pv_e <- if(!is.na(l$fe_e)) survey[which(survey == l$fe_e) - 1] else NA
		# last egg encounter
		l$le_e <- if("e" %in% x$state) last(x$date[x$state == "e"]) else NA
		# visit following to last egg encounter
		l$fv_e <- if(!is.na(l$le_e)) survey[which(survey == l$le_e) + 1] else NA
		# egg fate
		l$fate_e <- if("ht" %in% x$state) "success" else "failure"
		if(!is.na(l$le_e)) {
			l$fate_e <- if(l$le_e == last(survey)) "undetermined" else l$fate_e	
		}
		# first hatchling encounter
		l$fe_h <- if("ht" %in% x$state) first(x$date[x$state == "ht"]) else NA
		# visit previous to first ht encounter
		l$pv_h <- if(!is.na(l$fe_h)) survey[which(survey == l$fe_h) - 1] else NA
		# last ht encounter
		l$le_h <- if("ht" %in% x$state) last(x$date[x$state == "ht"]) else NA
		# visit following to last ht encounter
		l$fv_h <- if(!is.na(l$le_h)) survey[which(survey == l$le_h) + 1] else NA
		# ht fate
		l$fate_h <- if("c3" %in% x$nest_status) "success" else "failure"
		if(!is.na(l$le_h)){
			l$fate_h <- if(l$le_h == last(survey)) "undetermined" else l$fate_h
		}
		# last stage 
		l$stage <- last(x$nest_status)
		
		l <- lapply(l, function(x)	if(length(x) == 0) NA else x)
		l <- data.frame(l$pv_e, l$fe_e, l$le_e, l$fv_e, l$pv_h, l$fe_h, l$le_h, l$fv_h, l$fate_e, l$fate_h, l$stage) 
		# make nicer namaes
		names(l) <- stringr::str_sub(names(l), 3)
		l
	}, .progress = "text")

# calculate times
nests %<>%
	mu(incubation_time = as.numeric(difftime(le_e, fe_e, units = "day")), 
		 brooding_time = as.numeric(difftime(le_h, fe_h, units = "day")), 
		 nest_time = incubation_time + brooding_time)

# filter out nests with longer incubation or brooding periods, they are likely mistakes
nests <- nest_periods %>%
	mu(nest_total = incubation + brooding) %>%
	inner_join(nests) %>%
	fi(incubation_time <= incubation | is.na(incubation_time),
		 brooding_time <= brooding | is.na(brooding_time)) 

# filter out islets with not enough nests to make it meaningful
empty_islets <- nests %>%
	group_by(spp_name, islet) %>%
	su(n_new_nest = n()) %>% 
	# fi(n_new_nest < as.numeric(difftime(max(tb$date), min(tb$date), units = 'days'))/365 * 3) %>%
	fi(n_new_nest < 20) %>%
	se(-n_new_nest)

nests %<>%
	dplyr::anti_join(empty_islets)

saveRDS(nests, file =  "../data/processed/nest_success.rds", ascii = TRUE, compress = FALSE)
# 
# nests %<>%
# 	fi(incubation_time < 50 | is.na(incubation_time),
# 		 	brooding_time < 90 | is.na(brooding_time), 
# 		 	nest_time < 140 | is.na(nest_time)) 
# 
# nests %>%
# 	fi(fate_e == "success") %>%
# 	ggplot(aes(x = fe_e, y = incubation_time, colour = spp_name)) +
# 	geom_smooth(method = "lm")
# 
# nests %>%
# 	fi(fate_h == "success", 
# 		 brooding_time != 0) %>%
# 	plyr::dlply("spp_name", function(x){
# 		lm(brooding_time ~ fe_h, data = x)
# 	}) %>%
# 	lapply(summary)
# 
# nests %>%
# 	fi(fate_h == "success", 
# 		 brooding_time != 0) %>%
# 	ggplot(aes(x = fe_h, y = brooding_time, colour = spp_name)) +
# 	geom_point() +
# 	geom_smooth(method = "lm")
# 
# # lets explore it a bit
# # incubation time
# nests %>%
# 	mu(incubation_time = as.numeric(difftime(le_e, fe_e, units = "day")), 
# 		 brooding_time = as.numeric(difftime(le_h, fe_h, units = "day"))) %>% 
# 	fi(incubation_time < 100, 
# 		 brooding_time < 150) %>%
# 	ggplot(aes(x = brooding_time, fill = spp_name)) +
# 	geom_histogram(binwidth = 2) +
# 	facet_grid(spp_name ~fate_h, scales = "free")
#
# tb %>% 
# 	fi(date > as.POSIXct("2012-01-01")) %>%
# 	group_by(islet, nest, spp_name) %>% 
# 	summarise(empty = as.numeric(any(nest_status == "en", na.rm = T)), date = first(date)) %>%
# 	ggplot(aes(date, empty, colour = spp_name)) + 
# 	geom_point() + 
# 	geom_smooth()
