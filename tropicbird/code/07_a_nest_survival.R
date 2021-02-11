library(BEANSP)
library(magrittr)

ns <- readRDS("../data/processed/nest_success.rds")
source("./functions.R")

library(unmarked)
# eggs

surv_data <- ns %>%
	fi(fate_e != "undetermined") %>%
	mu(fe = fe_e, le = le_e, fv = fv_e, nt = incubation, fate = fate_e) %>%
	mu(y = plyr::mapvalues(fate, c("success", "failure"), c(1, 0)),
		 zl = as.numeric(difftime(le, fe, units = "days")) + 2, 
		 zr = as.numeric(difftime(fv, fe)) + 1,
		 ur = abs(zl - nt) + 1, 
		 ul = abs(zr - nt) + 1, 
		 ul = replace(ul, y == 0, 1), 
		 x1 = runif(nrow(.), 0, 10)) %>%
	fi(!is.na(zl)) %>%
	mu(id = 1:nrow(.))

a <- surv_data %>%
	fi(spp_name == "Red-tail tropicbird") %$%
	# dplyr::slice(1:50) %$%
	nestsr(nt[1], nx = 1, nn = length(id), ul = ul, ur = ur, zl = zl, zr = zr, 
				 x = matrix(x1, ncol = 1), y = y, a = as.double(rep(2.0,2)), 
				 b = as.double(rep(1.0,2)), sigma = as.double(rep(1.0,2)),
				 day = as.double(rep(0.0, nt[1])), enc = as.double(rep(0.0, nt[1]-1)),
				 covar = as.double(rep(0.0,1)), n0 = as.integer(10), ntotal = as.integer(110))


	mu(fe = min(fe_e, fe_h, na.rm = T), 
		 le = max(le_e, le_h, na.rm = T),
		 pv = max(pv_e, pv_h, na.rm = T),
		 fate_e = plyr::mapvalues(fate_e, c("success", "failure"), c("S", "F")),
		 fate_h = plyr::mapvalues(fate_h, c("success", "failure"), c("S", "F")), 
		 cov = 1) %>%
	mu(id = paste(islet, nest),
		 date1 = as.Date(fe), 
		 date2 = format(le, "%m/%d/%Y"),
		 date3 = format(pv, "%m/%d/%Y"),
		 fate = fate_h) %>%
	se(id, date1, date2, date3, fate, cov) %>%
	fi(!is.na(date1))

nestconv(1, nrow(surv_data), 140, surv_data)


data<-example2
jj<-as.integer(20)
nx<-as.integer(2)
nn<-as.integer(233)
x1<-example2[1:nn,6]
x2<-example2[1:nn,7]
x<-cbind(as.double(x1),as.double(x2))
temp<-nestconv(1,nn,jj,data)