library(magrittr)
# Read data
tb <- Hmisc::mdb.get("../data/raw/TropicBirdNesting_be.mdb", tables = NULL) %>%
  lapply(dplyr::tbl_df)

source("./functions.R")
# 
# tb$tblIsletCounts %>%
#   dplyr::group_by(fkIsletID, NestID) %>%
#   dplyr::summarise(n = n()) %>% View


# check data & clean ----------------------------------------------------------

# nests_without_islet <- tb$tblIsletCounts %>%
#   dplyr::filter(is.na(fkIsletID), !is.na(NestID))
# islets_without_nest <- tb$tblIsletCounts %>%
#   dplyr::filter(is.na(NestID), !is.na(fkIsletID))
# duplicated_nests_ids <- tb$tblIsletCounts %>%
#   dplyr::group_by(NestID) %>%
#   dplyr::summarise(n_islets = dplyr::n_distinct(fkIsletID)) %>% 
#   dplyr::filter(n_islets >= 2, !is.na(NestID)) %$% NestID %>% unique()
# duplicated_nests <- tb$tblIsletCounts %>%
#   dplyr::filter(NestID %in% duplicated_nests_ids) 

# ignore for now...

# colection ---------------------------------------------------------------

# islets
tb$tblInnerOuter %<>%
	mu(inner_outer_id = as.integer(pkInnerOuter),
		 inner_outer = as.character(InnerOuter), 
		 inner_outer = tolower(inner_outer)) %>%
	se(inner_outer_id, inner_outer)

# collection events
get_time <- 
	function(x) 
		as.numeric(difftime(x, as.POSIXct(trunc.POSIXt(x, "day")), units = 'hours'))

tb$tblColEvent %<>%
	mu(col_id = as.integer(pkDataColEvent),
		 date = as.POSIXct(Date., format = "%m/%d/%y", tz = "Indian/Mahe"),
		 all_checked = as.character(All.Nests.checked), 
		 all_checked = plyr::mapvalues(all_checked, 
		 															c('N', 'Y', 'yes', ''), 
		 															c(F, T, T, NA)),
		 inner_outer_id = as.integer(InnerOuter),
		 col_notes = as.character(Notes.CollectionEvent)) %>%
	mu_at(dplyr::vars(TimeStart, TimeEnd), dplyr::funs(as.POSIXct), 
				format = '%m/%d/%y %H:%M:%S', tz = "Indian/Mahe") %>% 
	mu_at(dplyr::vars(TimeStart, TimeEnd), dplyr::funs(get_time)) %>% 
	se(col_id, date, start = TimeStart, end = TimeEnd, inner_outer_id, all_checked,
		 col_notes)

# manual fixes
tb$tblColEvent %<>%
	mu_c(col_id == 77, inner_outer_id = 2) %>%
	fi(col_id != 291, col_id != 224, !is.na(date)) %>%
	ar(date, inner_outer_id)

# joins
collections <- tb$tblColEvent %>%
	se(-inner_outer_id)

# observations ------------------------------------------------------------

# islets
tb$tblIslets %<>%
	mu(islet_id = as.integer(pkIsletID),
		 islet_name = as.character(Name.),
		 islet_name = readr::parse_number(islet_name), 
		 inner_outer_id = as.integer(fkInnerOuterIslet)) %>%  
	fi(islet_id != 19) %>%
	mu_c(is.na(islet_name), islet_name = 0) %>% 
	mu(islet_name = stringr::str_pad(islet_name, 2, pad = '0'), 
		 islet = paste0('LG', islet_name)) %>%
	se(islet_id, islet, inner_outer_id)

# adults
tb$tblAdultCode %<>%
	mu(adult_id = as.integer(pkAdultCodeID),
		 adult_code = as.character(AdultCode),
		 adult = as.character(Description)) %>%
	mu_c(adult_id == 4, adult_code = NA, adult = NA) %>%
	se(adult_id, adult_code, adult)
	
# egg fragments
tb$tblEggFragments %<>%
	mu(egg_fragments_id = as.integer(ID),
		 egg_fragments = as.character(Egg.fragments)) %>%
	se(egg_fragments_id, egg_fragments)

# nests
tb$tblNestCode %<>%
	mu(nest_status_id = as.integer(pkNestCodeID),
		 nest_status = as.character(NestCode),
		 nest_status_desc = as.character(Description)) %>%
	se(nest_status_id, nest_status, nest_status_desc)

# rat signs
tb$tblRatSigns %<>%
	mu(rat_id = as.integer(ID),
		 rat = as.character(Rat.signs)) %>%
	se(rat_id, rat)

# species
tb$tblSpp %<>%
	mu(spp_id = as.integer(ID),
		 spp_name = as.character(Common.name)) %>%
	se(spp_id, spp_name) %>%
	mu(spp_sn1 = c('Phaeton rubricauda', 'Phaeton lepturus', NA, NA),
		 spp_sn2 = c('P. rubricauda', 'P. lepturus', NA, NA)) 

# rename that shit
tb$tblIsletCounts %<>%
	mu(id = as.integer(pkID), 
		 col_id = as.integer(fkColEventID), 
		 islet_id = as.integer(fkIsletID),
		 nest = as.integer(NestID), 
		 status = tolower(as.character(NewExisting)),
		 spp_id = as.integer(fkSppId),
		 adult_id = as.integer(fkAdultCode), 
		 nest_status_id = as.integer(fkNestCode),
		 egg_fragments_id = plyr::mapvalues(Egg.fragments, c('Y', 'N'), c(T, F)),
		 egg_fragments_id = as.integer(egg_fragments_id),
		 rat_id = as.integer(Rat.signs),
		 old_id = as.integer(IDLink.CLEANED.hist.data.03112011)) %>%
	se(id, col_id, islet_id, nest, status, spp_id, adult_id, nest_status_id, 
		 egg_fragments_id, rat_id)

# merge
observations <- tb$tblIsletCounts %>%
	fj(tb$tblIslets) %>% 
	fj(tb$tblAdultCode) %>% 
	fj(tb$tblEggFragments) %>%
	fj(tb$tblNestCode) %>%
	fj(tb$tblRatSigns) %>%
	fj(tb$tblSpp) %>%
	fj(tb$tblInnerOuter)

tb$tblCLEANED_FINAL_EXPORT_03112011 <- NULL

saveRDS(append(tb, collections) %>% append(observations), 
				file = "../data/processed/tb_data_clean.rds", 
				ascii= TRUE, compress = FALSE)

tb <- fj(collections, observations)

# fix dates
threshold <- 3 # group surveys within 3 days of each other
dates <- dplyr::data_frame(date = unique(fi(tb, !is.na(date))$date)) %>%
	mu(date_gr = dist(date) %>% hclust() %>% cutree(h = threshold*24*3600))

tb <- tb %>%
	mu(date_orig = date) %>%
	ij(dates) %>%
	gr(date_gr) %>%
	mu(date = min(date))

##### fix nests that are missing some info
no_data_nest <- fi(tb, spp_name == 'No data' |
									 	is.na(spp_name) |
									 	is.na(islet),
									 !is.na(nest))$nest 
# start by removing them from the main data frame 
no_data_nest_tb <- fi(tb, nest %in% no_data_nest) %>% ar(nest)
tb %<>% fi(!(nest %in% no_data_nest)) 
tb <- plyr::ddply(no_data_nest_tb, 'nest', function(x){
	if(nrow(x) == 1) {
		return(NULL)
	} else{
		species <- unique(x$spp_name)
		species <- species[species != 'No data']
		species <- species[!is.na(species)]
		nest <- unique(x$nest)
		nest <- nest[!is.na(nest)]
		islet <- unique(x$islet)
		islet <- islet[!is.na(islet)]
		if (length(species) == 0 | length(nest) == 0 | length(islet) == 0) {
			return(NULL)
		} else if(length(species) == 2 | length(islet) == 2){
			return(x)
		} else {
			
			species_sn1 <- unique(x$spp_sn1)
			species_sn1 <- species_sn1[species_sn1 != 'No data']
			species_sn1 <- species_sn1[!is.na(species_sn1)]
			
			species_id <- unique(x$spp_id)
			species_id <- species_id[!is.na(species_id)]
			
			species_sn2 <- unique(x$spp_sn2)
			species_sn2 <- species_sn2[!is.na(species_sn2)]
			
			islet_id <- unique(x$islet_id)
			islet_id <- islet_id[!is.na(islet_id)]
			
			io <- unique(x$inner_outer)
			io <- io[!is.na(io)]
			
			# message(nest, species, species_sn1, species_sn2)
			x$nest <- nest
			x$spp_name <- species
			x$spp_sn1 <- species_sn1
			x$spp_sn2 <- species_sn2
			x$islet <- islet
			x$inner_outer <- io
			return(x)
		}
	}
}) %>% dplyr::bind_rows(tb) %>%
	fi(spp_name != 'No data')

# remove observations prevous to Dec 2009
first <- as.POSIXct('2009-12-14', tz = "Indian/Mahe")
nests_first <- fi(tb, date == first) %$% unique(nest)
tb %<>%
	fi(date >= first | nest %in% nests_first) %>%
	fi(!is.na(date), !is.na(nest), !is.na(islet), !is.na(spp_name))

## make more sensible nests ids
tb %<>%
	mu(nest_islet = paste(nest, islet, spp_id))

# remove duplicates
tb %<>% plyr::ddply(c('date', 'inner_outer', 'nest_islet'), function(x){
	if(nrow(x) != 1){
		x %<>%
			fi(nest_status != 'not found')
		if(nrow(x) != 1){
			x %<>%
				ar(date_orig)
			return(x[nrow(x), ])
		} else {
			return(x)
		}
	} else {
		return(x)
	} 
})

saveRDS(tb, file = "../data/processed/master_df.rds")


