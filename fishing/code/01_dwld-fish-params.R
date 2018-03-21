library(rfishbase)
library(magrittr)

fish <- Hmisc::mdb.get("data/raw/Fish_catch_be.mdb", tables = NULL) %>%
  lapply(dplyr::tbl_df)

# get catch
catch <- fish$tblFISH %>%
  dplyr::group_by(Scientificname) %>%
  dplyr::summarise(n_catch = n(),
                   biomass = mean(Weight, na.rm = T) * n_catch,
                   biomass = round(biomass)) %>%
  dplyr::full_join(fish$tblFishing_Spp_List_2010, 
                    by = c('Scientificname' = 'ID.species')) %>%
  dplyr::select(sciname = Scientific.names, catch = n_catch, biomass) %>%
  dplyr::mutate(sciname = as.character(sciname), 
                sciname = trimws(sciname)) %>%
  dplyr::arrange(sciname, catch)


#  correct scientific names -----------------------------------------------
 
names <- read.csv("data/processed/fixed_species_names.csv") %$% x %>%
  as.character()

species_id <- species(names, fields = c('sciname', 
                                        'FBname',
                                        'SpecCode',
                                        'Vulnerability'))

resilience <- stocks(names, fields = c('Resilience', 
                                       'ResilienceRemark', 
                                       'IUCN_Code',
                                       'IUCN_ID'))

sustainability <- dplyr::inner_join(species_id, resilience) %>%
  dplyr::filter(!is.na(Resilience))

sustainability %<>%
  dplyr::mutate(iucn_status = plyr::mapvalues(IUCN_Code, 
                                              c('N.E.', 'DD', 'LC',
                                                'NT', 'VU', 'EN'),
                                              c('0 - Not evaluated',
                                                '0 - Data deficient',
                                                '1 - Least concern',
                                                '2 - Near Threatened',
                                                '3 - Vulnerable',
                                                '4 - Endangered')),
                vuln_categ = cut(Vulnerability, c(0, 20, 40, 60, 80, 100), 
                                 labels = c('Low', 'Low-Moderate',
                                            'Moderate-High', 
                                            'High-Very High',
                                            'Very High'), ordered_result = T))


# fix stupid names
catch %<>%
  dplyr::filter(sciname != '', 
                sciname != 'Epinephelus',
                sciname != 'Other', 
                sciname != 'Shypraena spp') %>%
  dplyr::mutate(sciname = plyr::mapvalues(sciname, 
                                          c('Anyperondon leucogrammicus',
                                            'caranx lugubris',
                                            'Cephalopolis hemistiktos',
                                            'Epinephelus  macrospilos',
                                            'Momotaxis gradoculis',
                                            'Shyraena barracuda'),
                                          c('Anyperodon leucogrammicus',
                                            'Caranx lugubris',
                                            'Cephalopholis hemistiktos',
                                            'Epinephelus macrospilos',
                                            'Monotaxis grandoculis',
                                            'Sphyraena barracuda')))

mixed <- data.frame(sciname = 'Cephalopholus miniata/urodeta or Epinephelus spilotoceps/merra',
           right = c('Cephalopholis miniata',
                     'Cephalopholis nigripinnis',
                     'Epinephelus spilotoceps',
                     'Epinephelus merra')) %>%
  dplyr::bind_rows(
    data.frame(sciname = 'Lethrinus rubrioperculatus/variegatus/lentjan',
               right = c('Lethrinus rubrioperculatus',
                         'Lethrinus lentjan',
                         'Lethrinus variegatus'))
  ) %>%
  dplyr::bind_rows(
    data.frame(sciname = 'Lutganus monostigma/fulviflamma',
               right = c('Lutjanus monostigma',
                         'Lutjanus fulviflamma'))
  ) %>%
  dplyr::bind_rows(
    data.frame(sciname = 'Balistidae spp',
               right = c('Balistoides viridescens',
                         'Pseudobalistes flavimarginatus'))
  ) %>%
  dplyr::group_by(sciname) %>%
  dplyr::mutate(n = n()) %>%
  dplyr::inner_join(catch) %>% 
  dplyr::mutate(catch = round(catch / n),
                biomass = round(biomass / n)) %>%
  dplyr::group_by()

catch <- mixed %>%
  dplyr::select(sciname = right, catch, biomass) %>% 
  dplyr::bind_rows(catch) %>%
  dplyr::filter(!(sciname %in% mixed$sciname))
  
dplyr::full_join(sustainability, catch) %>% 
  saveRDS('data/processed/fish-sustainability.rds')
