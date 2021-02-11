library(ggplot2)
library(magrittr)

b <- 4
breaks <- seq(as.Date('2006-01-01'), max(col_event$date), paste(b, 'month'))

cc %>%
	dplyr::inner_join(col_event) %>% 
	dplyr::group_by(col_id, area) %>%
	dplyr::summarise(date = mean(date), 
									 n_crabs = n()) %>%
	dplyr::filter(area %in% c('BP', 'CP'),
								date < as.Date('2016-01-01')) %>%
	dplyr::mutate(month = cut(date, breaks)) %>%
	# dplyr::group_by(month, area) %>%
	# dplyr::summarise(sd = sd(n_crabs),
	# 								 n_crabs = mean(n_crabs)) %>%
	# dplyr::ungroup() %>%
	dplyr::mutate(month = as.Date(month) + b*30/2) %>%
	ggplot(aes(x = month, y = n_crabs, colour = area, fill = area)) +
	stat_summary(size = 0.5, position = position_dodge(width = b*30/3),
							 geom = c('line')) +
	stat_summary(size = 0.5, position = position_dodge(width = b*30/3),
							 geom = c('linerange')) +
	stat_summary(size = 1.5, shape = 21, 
							 position = position_dodge(width = b*30/3),
							 geom = c('point')) +
	# scale_x_date(date_breaks = 'year') +
	theme_bw() +
	# scale_alpha_manual(values = c(0.6, 1)) +
	scale_fill_grey() +
	scale_color_grey() +
	xlab('date') +
	ylab('mean number of crabs per survey')
