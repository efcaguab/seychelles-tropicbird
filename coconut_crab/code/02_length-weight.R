library(magrittr)
library(mgcv)

cc <- readRDS(tb, file = "./data/processed/crabs.rds")

# normal vs corrected weight
we_we <- lm(c_weight ~ weight, data = cc)
# ∆- corrected weight is normal weight - 85 grams, but later I figured out that
# for the ones that is not filled the weight field might have already been
# corrected because there are weights smaller than 85

# Initial models ----------------------------------------------------------

cc_w <- cc %>%
  dplyr::filter(!is.na(t_length),
                !is.na(tt_length),
                !is.na(rostrum_length),
                !is.na(width))

mod_lw <- list(t_length = list(), 
               tt_length = list(), 
               rostrum_length = list(),
               width = list())

# final model -------------------------------------------------------------

length_weight <-  smatr::sma(c_weight ~ t_length,
                             data = cc, log = "xy", robust = T)

saveRDS(length_weight, file = "./data/processed/length_weight_model.rds",
        ascii = F, compress = F)

  
