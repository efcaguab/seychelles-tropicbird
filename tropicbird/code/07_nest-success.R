library(magrittr)

all_tb <- readRDS("../data/processed/basic-data.rds")
tb <- readRDS("../data/processed/tropicbird_monitoring.rds")
islets <- readRDS("../data/processed/islets.rds")

season_cuts <- seq(as.Date("2009-07-01"), as.Date("2016-07-01"), by = "year")

ext_e <- function(a){
	eggs <- which(a == "e")
	if (length(eggs)){
		if(eggs[1] == 1){
			return(1 + ext_e(a[-1]))
		} else return(eggs[1])
	} else return(NA)
}

# split stuff by new eggs or empty eggs
nest_succ <-
	all_tb %>%
	plyr::ddply(c("fkSppId" ,"fkIsletID", "NestID"), function(x){
		x %<>% dplyr::arrange(date)
		
		sequen <- x$NestCode
		nest_split <- vector("integer", length = length(sequen))
		first_egg <- ext_e(sequen)
		if (!is.na(first_egg)){
			if (first_egg != 1){
				nest_split[first_egg:length(nest_split)] <- nest_split[first_egg:length(nest_split)] + 1
			}
		}
		last_empty <- rev(which(as.character(sequen) == "en"))[1]
		if (!is.na(last_empty)){
			if (last_empty != length(sequen)){
				nest_split[(last_empty+1):length(nest_split)] <-
					nest_split[(last_empty+1):length(nest_split)] + 1
			}
		}
		x %>%
			dplyr::mutate(nest_split = nest_split)
	}, .progress = "text") 

# obtain nest succession
nest_succ <- nest_succ %>%
	plyr::ddply(c("fkSppId" ,"fkIsletID", "NestID", "nest_split"), function(x){
		nest_succession <- x %>% 
			dplyr::arrange(date) %$%
			NestCode %>% as.character()
		
		nest_succession <- nest_succession[nest_succession != dplyr::lead(nest_succession, default = "oo")] 
		
		nest_succession <- nest_succession[!nest_succession %in% c("not found", "en", "cd", "ep")]
		nest_succession_c <- nest_succession %>% paste(collapse = "-")
		
		x %>%
			dplyr::mutate(n_s = nest_succession_c, 
										final_stage = dplyr::last(nest_succession))
	}, .progress = "text")


## models
islet_info <- tb$tblIslets %>% dplyr::left_join(islets@data)

nest_succ_agg <- nest_succ %>%
	dplyr::mutate(season = cut(date, season_cuts)) %>%
	# dplyr::filter(!is.na(season)) %>%
	dplyr::group_by(fkSppId, fkIsletID, NestID, nest_split) %>%
	dplyr::summarise(date = min(date), 
									 final_stage = unique(final_stage), 
									 success = final_stage == "c3",
									 failure_chi = final_stage %in% c("c1", "c2"),
									 failure_egg = final_stage == "e") %>%
	dplyr::group_by(fkIsletID, fkSppId) %>%
	dplyr::mutate(nest_islets = n()) %>%
	dplyr::group_by() %>%
	# filter out islets with not enough data
	dplyr::filter(nest_islets > 20)

nest_succ_agg %<>%
	tidyr::gather(destiny, response, success:failure_egg) %>%
	dplyr::left_join(islet_info, by = c("fkIsletID" = "pkIsletID"))

saveRDS(nest_succ_agg, file = "../data/processed/nest_success.rds",
				ascii = T, compress = F)

