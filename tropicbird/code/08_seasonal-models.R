new_nest <- readRDS("./data/processed/new_nests.rds")

library(magrittr)

source("functions.R")

all_tb <- readRDS("../data/processed/basic-data.rds")
effort <- readRDS("../data/processed/effort.rds")
tb <- readRDS("../data/processed/tropicbird_monitoring.rds")

# first try to find all islets that were visited in each visit
# (and had something on it)

tb$tblIslets %>% 
	re(pkIsletI)
tb$tblColEvent %>% names()


all_tb %>%
	gr(date, fkIsletID) %>%
	su(n = n()) %>% 
	gr(date) %>%
	su(n_islets = n()) %>% 
	ggplot(aes(date, n_islets)) +
	geom_point()

new_nest <- all_tb %>%
	gr(fkSppId, fkIsletID, date) %>%
	su(n_new_nest = sum(NewExisting == "New")) %>%
	mu(lag = as.numeric(date) - as.numeric(dplyr::lag(date))) %>%
	fi(date > as.Date("2009-11-30")) %>%
	ar(fkSppId, date) %T>% View
	
new_nest %>%
	ggplot(aes(x = date, y = n_new_nest)) +
	geom_line(aes(colour = as.factor(fkSppId))) +
	geom_point(aes(fill = as.factor(fkSppId)), shape = 21) +
	facet_wrap(~fkSppId, ncol = 1) +
	theme(legend.position = "none")

new_nest %>% 
	ggplot(aes(lag, n_new_nest)) +
	geom_point(aes(fill = as.factor(fkSppId)), shape = 21) +
	scale_x_log10()


gam(n_new_nest ~ s(month) + s(as.numeric(date)) + n_surveys, family = "poisson", data = x)