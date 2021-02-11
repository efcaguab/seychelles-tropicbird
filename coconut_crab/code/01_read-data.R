library(magrittr)
# Read data
tb <- Hmisc::mdb.get("./data/raw/CoconutCrab_be.mdb", tables = NULL) %>%
  lapply(dplyr::tbl_df)
# location
loc <- list(lon = 46.2063,
            lat = -9.3897)

# collection --------------------------------------------------------------

# wind
tb$tblWind %<>%
  dplyr::rename(wind_id = ID,
                wind = Wind.strength) %>%
  dplyr::mutate(wind_id = as.character(wind_id),
                wind = as.character(wind))
# rain
tb$tblRain %<>%
  dplyr::rename(rain_id = ID,
                rain = Conditions) %>%
  dplyr::mutate(rain_id = as.character(rain_id),
                rain = c("dry", "rain", "wet")) %>%
  dplyr::select(-Description)

# area
tb$tblArea %<>%
  dplyr::rename(area_id = ID,
                area = Code,
                area_name = Name.) %>%
  dplyr::mutate(area_id = as.character(area_id),
                area = as.character(area),
                area_name = as.character(area_name)) %>%
  dplyr::select(area_id, area, area_name)

# collections
col_event <- tb$tblColEvent %>%
  dplyr::rename(col_id = CollectionEventNumber,
                date = ColEventDate, 
                area_id = Area,
                t_start = TimeStart,
                t_end = TimeEnd,
                cloud_cover = CloudCover,
                wind_id = WindStrength,
                rain_id = Rain,
                n_people = NumberPeople) %>%
  dplyr::mutate(col_id = as.character(col_id),
                date = as.Date(date, "%m/%d/%y %H:%M:%S"),
                t_start = as.character(t_start),
                t_start = replace(t_start, t_start == "", "20:00:00"),
                t_start = stringr::str_sub(t_start, start = -8),
                t_start = as.POSIXct(paste(date, t_start), 
                                     format = "%Y-%m-%d %H:%M:%S",
                                     tz = "Indian/Mahe"),
                t_start = replace(t_start,
                                  difftime(t_start, trunc(t_start, "days"), units = "secs") < 3600 * 12,
                                  t_start[difftime(t_start, trunc(t_start, "days"), units = "secs") < 3600 * 12] + 3600*12),
                t_end = stringr::str_sub(as.character(t_end), 10),
                t_end = as.POSIXct(paste(date, t_end), 
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz = "Indian/Mahe"),
                t_end = replace(t_end, is.na(t_end),
                                t_start[is.na(t_end)] + 45 *60),
                area_id = as.character(area_id),
                cloud_cover = cloud_cover/100,
                moon_if = oce::moonAngle(as.POSIXlt(t_start, tz = "UTC"), 
                                         longitude = loc$lon,
                                         latitude = loc$lat)$illuminatedFraction,
                moon_ph = oce::moonAngle(as.POSIXlt(t_start, tz = "UTC"), 
                                         longitude = loc$lon,
                                         latitude = loc$lat)$phase,
                moon_ph = moon_ph - floor(moon_ph),
                wind_id = as.character(wind_id),
                rain_id = as.character(rain_id)) %>%
  dplyr::full_join(tb$tblWind) %>%
  dplyr::full_join(tb$tblRain) %>%
  dplyr::full_join(tb$tblArea) %>%
  dplyr::select(-Moon, -wind_id, -rain_id, -RecordersInitials, -DataEnterer,
                -DataChecker, -area_id)



# Crab table --------------------------------------------------------------

# morph
tb$tblMorphColour %<>%
  dplyr::rename(morph_id = ID,
                morph_name = MorphColour,
                morph = MorphCode) %>%
  dplyr::mutate(morph_id = as.character(morph_id),
                morph = as.character(morph),
                morph_name = as.character(morph_name))

# moult
tb$tblMoultStatus %<>%
  dplyr::rename(moult = MS.code,
                moult_name = MS.Name.) %>%
  dplyr::mutate(moult = as.integer(moult),
                moult_name = as.character(moult_name)) %>%
  dplyr::select(moult, moult_name)

# sex
tb$tblSex %<>%
  dplyr::rename(sex_id = ID,
                sex = Sex) %>%
  dplyr::mutate(sex_id = as.character(sex_id),
                sex = as.character(sex))

#crab
cc <- tb$tblCrab %>%
  dplyr::rename(number = Entry.number,
                col_id = CollectionEventNumber,
                locality = Locality,
                distance = Distance,
                resight = Resight, 
                mark = MR.ID,
                t_length = Mid.to.mid,
                weight = Weight,
                c_weight = Corrected.weight,
                sex_id = Sex,
                moult = Moult.status,
                morph_id = Morph,
                comments = Comments,
                a_comments = Analysis.comments,
                tt_length = Tip.to.tip,
                rostrum_length = Rostrum,
                width = Width) %>%
  dplyr::mutate(col_id = as.character(col_id),
                locality = as.character(locality),
                distance = as.integer(distance),
                resight = as.logical(resight),
                mark = as.character(mark),
                t_length = as.numeric(t_length),
                weight = as.numeric(weight),
                c_weight = as.numeric(c_weight),
                sex_id = as.character(sex_id),
                moult = as.integer(moult),
                moult = replace(moult, moult == 0, NA),
                morph_id = as.character(morph_id),
                comments = as.character(comments),
                a_comments = as.character(a_comments)) %>%
  dplyr::full_join(tb$tblMorphColour) %>%
  dplyr::full_join(tb$tblMoultStatus) %>%
  dplyr::full_join(tb$tblSex) %>%
  dplyr::select(-MarkType,
                -Recorders,
                -Location.original,
                -Mark,
                -sex_id,
                -morph_id) %>%
  # standarise  corrected weight
  dplyr::mutate(c_weight = replace(c_weight,
                                   is.na(c_weight),
                                   weight[is.na(c_weight)])) %>%
  # remove 1mm crab...
  dplyr::filter(t_length > 1)

saveRDS(cc, file = "data/processed/crabs.rds", ascii= TRUE, compress = FALSE)
saveRDS(col_event, file = "data/processed/col_events.rds", ascii= TRUE, compress = FALSE)

