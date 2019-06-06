# the names of the data sources @dat_d_name, @u_fl_name, @v_fl_name
# are set in the config file

rm(list = ls())
Sys.setenv(LANGUAGE = "en")

source("0_config_local.R")

library(raster)
library(lubridate)
library(maps)
library(colorspace)
library(dplyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

first_range_list <- list(
	1901:1910, 1911:1920, 1921:1930,
	1931:1940, 1941:1950, 1951:1960,
	1961:1970, 	1971:1980, 1981:1990,
	1991:2000)
last_range_list <- list(2005:2014, 1995:2004)

for (i in seq(along.with = first_range_list)) {
	years_range1 <- first_range_list[[i]]
	years_range2 <- last_range_list[[1]]
	
	# read all the values
	u_brick <- brick(file.path(dat_d_name, u_fl_name))
	v_brick <- brick(file.path(dat_d_name, v_fl_name))
	# print(str(u_brick))
	
	e <- extent(0, 180, 40, 80)
	u_ru_raster <- crop(u_brick, e)
	v_ru_raster <- crop(v_brick, e)
	
	# # check time by output of the Z dimension
	# print(head(year(getZ(u_brick)), 50))
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	u_res_ru <- u_ru_raster[[which(year(getZ(u_ru_raster)) %in% years_range1)]]
	u_res_ru2 <- u_ru_raster[[which(year(getZ(u_ru_raster)) %in% years_range2)]]
	v_res_ru <- v_ru_raster[[which(year(getZ(v_ru_raster)) %in% years_range1)]]
	v_res_ru2 <- v_ru_raster[[which(year(getZ(v_ru_raster)) %in% years_range2)]]
	
	res_ru <- sqrt(u_res_ru^2 + v_res_ru^2)
	res_ru2 <- sqrt(u_res_ru2^2 + v_res_ru2^2)
	
		# #************************************
		# #	quick plot check
		# #************************************
		# dev.new()
		# plot(res_ru, main = "Abs wind speed",
		# 	col = sequential_hcl(7, "TealGrn")
		# 	)
		
		# dev.new()
		# plot(u_res_ru, main = "u-wind speed",
		# 	col = sequential_hcl(7, "TealGrn")
		# 	)
		
		# dev.new()
		# plot(v_res_ru, main = "v-wind speed",
		# 	col = sequential_hcl(7, "TealGrn")
		# 	)
		# #************************************
	
	res_mean <- calc(res_ru, mean)
	res_mean2 <- calc(res_ru2, mean)
	
	res_delta <- res_mean2 - res_mean
	res_delta_rel <- (res_mean2 - res_mean)/res_mean
	
			# # a little different way of calculations
			# res_mean_test <- mean(res_ru)
			# # > all.equal(res_mean_test, res_mean)
			# # >[1] TRUE
	
	#*******************************************
	
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#	res plotting
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	n_delta_cols <- 10
	shown_range_of_rel_chng <- c(-0.25, 0.25)	

	rng1_name <- paste0(
		c(first(years_range1), last(years_range1)), 
		collapse = "-")
	rng2_name <- paste(
		c(first(years_range2), last(years_range2)), 
		collapse = "-")

	if (!dir.exists(res_d_name)) dir.create(file.path(res_d_name))

	pdf(paste0(rng2_name, "_to_", rng1_name, ".pdf"))
		# dev.new()
		plot(res_mean, 
			main = rng1_name, 
			# col = (diverging_hcl(7, h = 260, c = 80, l = c(35, 95), power = 1))
			col = rev(sequential_hcl(12, "Plasma"))
		)
		map("world", add = TRUE, fill = FALSE)
		
		# dev.new()
		plot(res_mean2, 
			main = rng2_name,
			col = rev(sequential_hcl(12, "Plasma"))
		)
		map("world", add = TRUE, fill = FALSE)
		
		# dev.new()
		plot(res_delta, 
			main = paste0(rng2_name, " to ", rng1_name), 
			# col = (diverging_hcl(7, h = 260, c = 80, l = c(35, 95), power = 1))
			col = diverging_hcl(n_delta_cols, "Red-Green"),
			# breaks = seq(
			# 	from = -max(abs(res_delta@data@values)),
			# 	to = max(abs(res_delta@data@values)),
			# 	length.out = (n_delta_cols + 1)
			# )
			breaks = seq(
				from = -1, to = 1,
				length.out = (n_delta_cols + 1)
			)
		)
		map("world", add = TRUE, fill = FALSE)


		plot(res_delta_rel, 
			main = paste0(rng2_name, " to ", rng1_name,": relative change"), 
			# col = (diverging_hcl(7, h = 260, c = 80, l = c(35, 95), power = 1))
			col = diverging_hcl(n_delta_cols, "Red-Green"),
			# breaks = seq(
			# 	from = -max(abs(res_delta@data@values)),
			# 	to = max(abs(res_delta@data@values)),
			# 	length.out = (n_delta_cols + 1)
			# )
			breaks = seq(
				from = min(shown_range_of_rel_chng), to = max(shown_range_of_rel_chng),
				length.out = (n_delta_cols + 1)
			),
     		axis.args=list(
     			at = seq(min(shown_range_of_rel_chng), 
     				max(shown_range_of_rel_chng), 
     				length.out = (n_delta_cols + 1)),
     			labels = seq(min(shown_range_of_rel_chng), 
     				max(shown_range_of_rel_chng), 
     				length.out = (n_delta_cols + 1)),      		
     			# labels = seq(min(shown_range_of_rel_chng), max(shown_range_of_rel_chng), 
     			# 	length.out = (n_delta_cols + 1)), 
     			cex.axis = 1),
		)
		map("world", add = TRUE, fill = FALSE)
	
	dev.off()
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}