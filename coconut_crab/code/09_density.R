library(unmarked)
library(magrittr)
library(foreach)

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

# habitat composition -----------------------------------------------------

CP <- readr::read_csv("./data/raw/habitat_CP.csv", col_types = "nnnnnnnnnnn") %>% 
  mu(area = "CP")

BP <- readr::read_csv("./data/raw/habitat_BP.csv", col_types = "nnnnnnnnnnn") %>%
  mu(area = "BP")

hab <- dplyr::bind_rows(CP, BP) %>% 
  mu(locality = as.character(locality)) %>% 
	dplyr::inner_join(loc_dist) 

pc <- hab %>%
  se(3, 5:11) %>%
  prcomp() 

library(ggfortify)
autoplot(pc, loadings = T, 
				 loadings.label = T, 
				 data = hab, 
				 colour = 'dist_shore',
				 # frame = T,
				 loadings.label.repel = T,
				 loadings.label.size = 3.5,
				 loadings.label.label = c("champignon", "exposed surface", "mangrove", "open mixed scrub", "standard mixed scrub", "pemphis", "sand", "grasses")) +
	scale_color_continuous(name = "distance\nfrom shore") +
	theme_bw()

ggsave("./paper/supp_figures/principal-component-analysis.pdf", width = 4.7,height = 3.3, scale = 1.5)

saveRDS(pc, file = "./data/processed/habitat_prcomp.rds", ascii = T, compress = F)

hab %<>% dplyr::bind_cols(as.data.frame(pc$x))


# Abundance per survey per locality ----------------------------------------------------

counts <- cc 

counts %<>% 
  dplyr::inner_join(col_event) %>%
  fi(!is.na(date), !is.na(area), !is.na(locality), !is.na(distance)) %>%
  gr(date, area, locality, distance) %>%
  su(n = n())

y <- expand.grid(date = unique(counts$date),
                 area = unique(counts$area),
                 locality = unique(counts$locality),
                 distance = 0:4) %>% 
  dplyr::left_join(counts) %>%
  mu(n = replace(n, is.na(n), 0), 
     distance = distance , 
     distance = paste("d", distance, sep = ""))

unmarkedFrames <- plyr::dlply(y, "date", function(x){
  d <- x %>%
    tidyr::spread(distance, n) %>% 
    dplyr::inner_join(hab, by = c("area", "locality")) %>% 
    dplyr::inner_join(col_event, by = c("date", "area")) %>%
    dplyr::inner_join(loc_dist, by = c("area", "locality")) %>%
    mu(locality = as.numeric(locality)) %>%
    fi(!(area == "CP" & locality <= 12))
  
  unmarkedFrameDS(
    y = as.matrix(se(d, 4:8)),
    siteCovs = se(d, area, dist_shore, PC1, PC2),
    dist.breaks = 0:5,
    tlength = se(d, length)[[1]],
    survey = "line",
    unitsIn = "m"
  )
  
}, .progress = "text")

abu0 <- plyr::llply(unmarkedFrames, 
                    function(x) distsamp(~ dist_shore + PC1 + PC2 ~ dist_shore + PC1 + PC2, x),
                    .progress = "text")
abu1 <- plyr::llply(unmarkedFrames, 
                    function(x) distsamp(~ dist_shore + PC1 + PC2 ~ 1, x),
                    .progress = "text")
abu2 <- plyr::llply(unmarkedFrames, 
                     function(x) distsamp(~ dist_shore + PC1 ~ 1, x),
                     .progress = "text")
abu3 <- plyr::llply(unmarkedFrames, 
                    function(x) distsamp(~ dist_shore + PC2 ~ 1, x),
                    .progress = "text")
abu4 <- plyr::llply(unmarkedFrames, 
                    function(x) distsamp(~ PC1 + PC2 ~ 1, x),
                    .progress = "text")
abu5 <- plyr::llply(unmarkedFrames, 
                    function(x) distsamp(~ PC1 ~ 1, x),
                    .progress = "text")
abu6 <- plyr::llply(unmarkedFrames, 
                    function(x) distsamp(~ PC2 ~ 1, x),
                    .progress = "text")
abu7 <- plyr::llply(unmarkedFrames, 
                    function(x) distsamp(~ dist_shore ~ 1, x),
                    .progress = "text")
abu8 <- plyr::llply(unmarkedFrames, 
                    function(x) distsamp(~ 1 ~ 1, x),
                    .progress = "text")
abu9 <- plyr::llply(unmarkedFrames, 
                    function(x) distsamp(~ 1 ~ 1, x, keyfun = "exp"),
                    .progress = "text")
abu10 <- plyr::llply(unmarkedFrames, 
                    function(x) distsamp(~ 1 ~ 1, x, keyfun = "hazard"),
                    .progress = "text")
abu11 <- plyr::llply(unmarkedFrames, 
                    function(x) distsamp(~ 1 ~ 1, x, keyfun = "uniform"),
                    .progress = "text")

foreach (i = 1:length(abu1), .combine = c) %do% {
  which.min(c(abu0[[i]]@AIC, abu1[[i]]@AIC, abu2[[i]]@AIC, abu3[[i]]@AIC, abu4[[i]]@AIC, 
              abu5[[i]]@AIC, abu6[[i]]@AIC, abu7[[i]]@AIC, abu9[[i]]@AIC-2))
} %>% table

foreach (i = 1:length(abu1), .combine = c) %do% {
  which.min(c(abu8[[i]]@AIC-2, abu9[[i]]@AIC, abu10[[i]]@AIC, abu11[[i]]@AIC))
} %>% table


ab <- abu9 %>%
  lapply(function(x) {
    xx <- x@estimates@estimates$state
    ests <- xx@estimates
    SEs <- SE(xx)
    Z <- ests/SEs
    p <- 2 * pnorm(abs(Z), lower.tail = FALSE)
    # print(c(ests, SEs, Z,p))
    if(!is.na(p)){
      if(p < 0.05){
        # print(backTransform(x, type = "state")@estimate)
        return(backTransform(x, type = "state")@estimate)
      } 
    } else return(NA)
  }) %>% unlist %>% as.data.frame.vector() %>%
  dplyr::add_rownames()
names(ab) <- c("date", "density")

saveRDS(ab, file = "./data/processed/pop_density.rds", ascii = TRUE, compress = FALSE)

library(ggplot2)
# no significant effect of environment on the detection estimates:
abu0 %>%
  plyr::ldply(function(x){
    e <- x@estimates@estimates$det@estimates %>% as.data.frame() %>%
      dplyr::add_rownames()
    names(e) <- c("coeficient", "estimate")
    e
  }) %>%
	dplyr::filter(coeficient != "sigma(Intercept)") %>%
  ggplot(aes(x = plogis(estimate))) +
  geom_density() + facet_wrap(~coeficient) +
	xlab("p-value") + theme_bw()

# detection estimates 
ggsave("./paper/supp_figures/detection-coovariates.pdf", width = 4.7,height = 1.65, scale = 1.5)


abu0 %>%
  plyr::ldply(function(x){
    e <- x@estimates@estimates$det@estimates %>% as.data.frame() %>%
      dplyr::add_rownames()
    names(e) <- c("coeficient", "estimate")
    e
  }) %>%
  mu(date = as.Date(date),
     yday = lubridate::yday(date)) %>%
  ggplot(aes(x = yday, y = plogis(estimate))) +
  geom_point() + facet_wrap(~coeficient) + geom_smooth()


abu0 %>%
  plyr::ldply(function(x){
    e <- x@estimates@estimates$det@estimates %>% as.data.frame() %>%
      dplyr::add_rownames()
    names(e) <- c("coeficient", "estimate")
    e
  }) %>%
  mu(date = as.Date(date),
     yday = lubridate::yday(date)) %>%
  ggplot(aes(x = date, y = estimate)) +
  geom_point() + facet_wrap(~coeficient, scales = "free_y") + geom_smooth()


abu0 %>%
  plyr::ldply(function(x){
    e <- x@estimates@estimates$det@estimates %>% as.data.frame() %>%
      dplyr::add_rownames()
    names(e) <- c("coeficient", "estimate")
    e
  }) %>%
  mu(date = as.Date(date),
     date_utc = as.POSIXct(as.POSIXlt(date, tz = "UTC")),
     moon_ph = oce::moonAngle(date_utc, lon = 46.2, lat = -9.4)$phase,
     moon_ph = moon_ph - floor(moon_ph)) %>%
  ggplot(aes(x = moon_ph, y = plogis(estimate))) +
  geom_point() + facet_wrap(~coeficient) + geom_smooth()

# no significant effect of environment on the abundance estimates:
abu0 %>%
  plyr::ldply(function(x){
    e <- x@estimates@estimates$state@estimates %>% as.data.frame() %>%
      dplyr::add_rownames()
    names(e) <- c("coeficient", "estimate")
    e
  }) %>%
	dplyr::filter(coeficient != "(Intercept)") %>%
  ggplot(aes(x = plogis(estimate))) +
  geom_density() + facet_wrap(~coeficient) +
	xlab("p-value") + theme_bw()

# detection estimates 
ggsave("./paper/supp_figures/density-coovariates.pdf", width = 4.7,height = 1.65, scale = 1.5)


abu0 %>%
  plyr::ldply(function(x){
    e <- x@estimates@estimates$state@estimates %>% as.data.frame() %>%
      dplyr::add_rownames()
    names(e) <- c("coeficient", "estimate")
    e
  }) %>%
  mu(date = as.Date(date),
     yday = lubridate::yday(date)) %>%
  ggplot(aes(x = yday, y = plogis(estimate))) +
  geom_point() + facet_wrap(~coeficient) + geom_smooth()


ab %>%
  mu(date = as.Date(date)) %>%
  ggplot(aes(x = date, y = density)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) + 
  geom_point() + ylim(c(0, 125))

ab %>%
  mu(date = as.Date(date), 
     yday = lubridate::yday(date)) %>%
  ggplot(aes(x = yday, y = density)) +
  geom_point() + geom_smooth()


# Abundance per survey per transect ----------------------------------------------------

counts <- cc 

for(i in 1:nrow(counts)) {
  nd <- runif(1, counts$distance[i] - 0.5, counts$distance[i] + 0.5)
  counts$distance[i] <- floor(abs(nd))
}

counts %<>%
  dplyr::inner_join(col_event) %>%
  fi(!is.na(date), !is.na(area), !is.na(locality), !is.na(distance)) %>%
  mu(locality = as.numeric(locality)) %>%
  fi(!(area == "CP" & locality <= 12)) %>%
  gr(date, area, distance) %>%
  su(n = n())

y <- expand.grid(date = unique(counts$date),
                 area = unique(counts$area),
                 distance = 0:4) %>% 
  dplyr::left_join(counts) %>%
  mu(n = replace(n, is.na(n), 0), 
     distance = distance , 
     distance = paste("d", distance, sep = ""))

hab_g <- hab %>%
  gr(area) %>%
  su(PC1 = mean(PC1), PC2 = mean(PC2))

loc_dist_g <- loc_dist %>%
  mu(locality = as.numeric(locality)) %>%
  fi(!(area == "CP" & locality <= 12)) %>%
  gr(area) %>%
  su(length = sum(length))

abu <- plyr::dlply(y, "date", function(x){
  d <- x %>%
    tidyr::spread(distance, n) %>% 
    dplyr::inner_join(hab_g, by = "area") %>% 
    dplyr::inner_join(col_event, by = c("date", "area")) %>%
    dplyr::inner_join(loc_dist_g, by = "area") 
  
  data <- unmarkedFrameDS(
    y = as.matrix(se(d, 3:7)),
    siteCovs = se(d, area, PC1, PC2),
    dist.breaks = 0:5,
    tlength = se(d, length)[[1]],
    survey = "line",
    unitsIn = "m"
  )
  
  distsamp(~ 1 ~ 1, data)
  
}, .progress = "text")

ab <- abu %>%
  lapply(function(x) {
    xx <- x@estimates@estimates$state
    ests <- xx@estimates
    SEs <- SE(xx)
    Z <- ests/SEs
    p <- 2 * pnorm(abs(Z), lower.tail = FALSE)
    # print(c(ests, SEs, Z,p))
    if(!is.na(p)){
      if(p < 0.05){
        # print(backTransform(x, type = "state")@estimate)
        return(backTransform(x, type = "state")@estimate)
      } 
    } else return(NA)
  }) %>% unlist %>% as.data.frame.vector() %>%
  dplyr::add_rownames()
names(ab) <- c("date", "abundance")

library(ggplot2)

ab %>%
  mu(date = as.Date(date)) %>%
  ggplot(aes(x = date, y = abundance)) +
  geom_smooth() + geom_point() 

ab %>%
  mu(date = as.Date(date), 
     yday = lubridate::yday(date)) %>%
  ggplot(aes(x = yday, y = abundance)) +
  geom_point() + geom_smooth() + ylim(c(0,106))


# Abundance per year per locality ----------------------------------------------------
# 
# counts <- cc %>% 
#   dplyr::inner_join(col_event) %>%
#   fi(!is.na(date), !is.na(area), !is.na(locality), !is.na(distance)) %>%
#   gr(date, area, locality, distance) %>%
#   su(n = n())
# 
# y <- expand.grid(date = unique(counts$date),
#                  area = unique(counts$area),
#                  locality = unique(counts$locality),
#                  distance = 0:4) %>% 
#   dplyr::left_join(counts) %>%
#   mu(n = replace(n, is.na(n), 0), 
#      distance = distance , 
#      distance = paste("d", distance, sep = ""),
#      month = cut(date, "month"),
#      month = as.Date(month),
#      year = lubridate::year(date))
# 
# abu <- plyr::dlply(y, "year", function(x){
#   d <- x %>%
#     tidyr::spread(distance, n) %>%
#     mu(locality = as.numeric(locality)) %>%
#     fi(!(area == "CP" & locality <= 12))
#   
#   dd <- foreach(dat = unique(d$date), .combine = dplyr::full_join) %do% {
#     dd <- d %>% ar(area, locality) %>%
#       fi(date == dat) %>%
#       se(area, locality, d0:d4)
#     names(dd)[3:7] <- paste(dat, names(dd)[3:7], sep = "_")
#     return(dd)
#   }
#   dd_loc <- dd %>%
#     se(1:2) %>% 
#     mu(locality = as.character(locality)) %>%
#     dplyr::inner_join(hab, by = c("area", "locality")) %>%
#     dplyr::inner_join(loc_dist, by = c("area", "locality")) 
#   
#   nc <- ncol(dd)
#   dd %<>% dplyr::inner_join(mu(dd_loc, locality = as.numeric(locality)), 
#                             by = c("area", "locality")) %>%
#     se(1:nc)
#   
#   
#   col_site <- col_event %>%
#     fi(date %in% unique(d$date)) %>%
#     gr(date) %>%
#     su(cloud_cover = mean(cloud_cover),
#        n_people = mean(n_people),
#        moon_ph = mean(moon_ph),
#        moon_if = mean(moon_if)) %>%
#     mu(yday = as.factor(lubridate::month(date)))
#   
#   col_site <- do.call("rbind", replicate(nrow(dd_loc), col_site, simplify = FALSE))
#   
#   data <- unmarkedFrameGDS(
#     y = as.matrix(se(dd, -1, -2)),
#     siteCovs = se(dd_loc, area, dist_shore, PC1, PC2),
#     yearlySiteCovs = as.data.frame(se(col_site, cloud_cover, n_people, moon_ph, yday, moon_if)),
#     dist.breaks = 0:5,
#     numPrimary = length(unique(d$date)),
#     tlength = se(dd_loc, length)[[1]],
#     survey = "line",
#     unitsIn = "m"
#   )
#   
#   gdistsamp(~ dist_shore + PC1 + PC2, ~ moon_if + yday, ~ PC1 + PC2 + n_people + moon_if, data = data)
#   
# }, .progress = "text", .inform = T)
# 
# ab <- abu %>%
#   lapply(function(x) {
#     xx <- x@estimates@estimates$state
#     ests <- xx@estimates
#     SEs <- SE(xx)
#     Z <- ests/SEs
#     p <- 2 * pnorm(abs(Z), lower.tail = FALSE)
#     # print(c(ests, SEs, Z,p))
#     if(!is.na(p)){
#       if(p < 0.05){
#         # print(backTransform(x, type = "state")@estimate)
#         return(backTransform(x, type = "state")@estimate)
#       } 
#     } else return(NA)
#   }) %>% unlist %>% as.data.frame.vector() %>%
#   dplyr::add_rownames()
# names(ab) <- c("date", "abundance")
# 
# library(ggplot2)
# # no significant effect of environment on the detection estimates:
# abu %>%
#   plyr::ldply(function(x){
#     e <- x@estimates@estimates$det@estimates %>% as.data.frame() %>%
#       dplyr::add_rownames()
#     names(e) <- c("coeficient", "estimate")
#     e
#   }) %>%
#   ggplot(aes(x = plogis(estimate))) +
#   geom_density() + facet_wrap(~coeficient)
# 
# abu %>%
#   plyr::ldply(function(x){
#     e <- x@estimates@estimates$det@estimates %>% as.data.frame() %>%
#       dplyr::add_rownames()
#     names(e) <- c("coeficient", "estimate")
#     e
#   }) %>%
#   mu(date = as.Date(date),
#      yday = lubridate::yday(date)) %>%
#   ggplot(aes(x = yday, y = plogis(estimate))) +
#   geom_point() + facet_wrap(~coeficient) + geom_smooth()
# 
# # no significant effect of environment on the density estimates:
# abu %>%
#   plyr::ldply(function(x){
#     e <- x@estimates@estimates$state@estimates %>% as.data.frame() %>%
#       dplyr::add_rownames()
#     names(e) <- c("coeficient", "estimate")
#     e
#   }) %>%
#   mu(date = as.Date(date),
#      yday = lubridate::yday(date)) %>%
#   ggplot(aes(x = date, y = plogis(estimate))) +
#   geom_point() + facet_wrap(~coeficient) + geom_smooth()
# 
# 
# ab %>%
#   mu(date = as.Date(date)) %>%
#   ggplot(aes(x = date, y = abundance)) +
#   geom_smooth() + geom_point() + ylim(c(0, 125))
# 
# ab %>%
#   mu(date = as.Date(date), 
#      yday = lubridate::yday(date)) %>%
#   ggplot(aes(x = yday, y = abundance)) +
#   geom_point() + geom_smooth()
# 
