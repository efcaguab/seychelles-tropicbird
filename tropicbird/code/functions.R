
# dplyr abbreviations
mu <- dplyr::mutate
re <- dplyr::rename
gr <- dplyr::group_by
su <- dplyr::summarise
se <- dplyr::select
fi <- dplyr::filter
ar <- dplyr::arrange
mu_at <- dplyr::mutate_at
ij <- dplyr::inner_join
fj <- dplyr::full_join

# conditional mutate
mu_c <- function(.data, condition, ..., envir = parent.frame()) {
	condition <- eval(substitute(condition), .data, envir)
	.data[condition, ] <- .data[condition, ] %>% dplyr::mutate(...)
	.data
}

# convert wavelet object to data frame
wav.as.df <- function(x, y){
	x %>%
		`row.names<-`(y$Period) %>%
		`colnames<-`(as.character(y$series$date)) %>%
		as.data.frame.table() %>%
		dplyr::mutate(y = log10(as.numeric(as.character(Var1))),
									x = as.numeric(Var2))
} 

# revert list
revert_list<-  function(ll) { # @Josh O'Brien
	nms <- unique(unlist(lapply(ll, function(X) names(X))))
	ll <- lapply(ll, function(X) setNames(X[nms], nms))
	ll <- apply(do.call(rbind, ll), 2, as.list)
	lapply(ll, function(X) X[!sapply(X, is.null)])
}

# extracts fits from islets
extract_islet_fits <- function(x, y, vars = c('month', 'date'), add_intercept = T, se = F) {
	# for each islet
	islets <- names(x)
	foreach(j=1:length(islets)) %do% {
		# for each variable in the model
		foreach(i=1:length(vars)) %do% {
			if(se){
				o <- data.frame(x[[j]][[i]]$x, x[[j]][[i]]$fit[, 1], x[[j]][[i]]$se) %>%
					`names<-`(c('var', 'fit', 'se')) %>%
					mu(islet = islets[j])
				if (add_intercept) {
					f <- y[[j]]$coefficients[1]
					o %>% dplyr::mutate(fit = fit + f) %>% return()
				} else {
					return(o)
				}
			} else {
				o <- data.frame(x[[j]][[i]]$x, x[[j]][[i]]$fit[, 1]) %>%
					`names<-`(c('var', 'fit')) %>%
					mu(islet = islets[j])
				if (add_intercept) {
					f <- y[[j]]$coefficients[1]
					o %>% dplyr::mutate(fit = fit + f) %>% return()
				} else {
					return(o)
				}
			}
		} %>%
			`names<-`(vars)
	} %>%
		`names<-`(islets) %>%
		revert_list() %>% 
		lapply(function(x) do.call(rbind, x)) %>%
		lapply(as.tbl)
}
