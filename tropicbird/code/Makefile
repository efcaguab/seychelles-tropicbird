all: prepare analyse figures tables

# data folders
pd = ../data/processed/
rd = ../data/raw/
sd = $(rd)satellite/
f = ../figs/
t = ../tabs/

# data prepararion -----------------------------------------------------------
prepare: monitoring environmental 

## basic monitoring data
monitoring: $(pd)tb_data_clean.rds $(pd)master_df.rds $(pd)effort.rds $(pd)islets.rds

$(pd)tb_data_clean.rds $(pd)master_df.rds: 01_check-and-clean.R $(rd)TropicBirdNesting_be.mdb
	Rscript $<

$(pd)effort.rds : 02_effort.R $(pd)tb_data_clean.rds $(pd)master_df.rds
	Rscript $<

$(pd)islets.rds : 03_islets-information.R
	Rscript $<

## environmental data
environmental: satellite readings compile

satellite: download_satellite monthly_means 

download_satellite: $(sd)compiled_env_sat.rds 

$(sd)compiled_env_sat.rds: 04_a_env-dwnld-satellite.R $(pd)master_df.rds
	rm ~/.rerddap/* ; Rscript $<

monthly_means: $(pd)satellite_variables.pdf

$(pd)satellite_variables.pdf: 04_c_env-monthly-satellite.R $(sd)compiled_env_sat.rds
	Rscript $<

readings: $(pd)env_monthly_readings.rds

$(pd)env_monthly_readings.rds: 04_b_env-monthly-readings.R
	Rscript $<

compile: $(pd)all_env.rds 

$(pd)all_env.rds: 04_d_env-compile.R $(sd)compiled_env_sat.rds $(pd)env_monthly_readings.rds
	Rscript $<

# data analysis -----------------------------------------------------------
analyse: new_nests success

new_nests: $(pd)new_nest_wav.rds $(pd)new_nest_models.rds

$(pd)new_nest.rds: 05_a_new-nest-prepare.R $(pd)tropicbird_monitoring.rds $(pd)basic-data.rds $(pd)effort.rds
	Rscript $<

$(pd)new_nest_wav.rds: 05_b_new-nest-periodogram.R $(pd)new_nest.rds
	Rscript $<

$(pd)new_nest_models.rds: 05_c_new-nest-models.R $(pd)new_nest.rds
	Rscript $<

success: $(pd)nest_success.rds $(pd)hatching_success_models.rds $(pd)fledging_success_models.rds

$(pd)nest_success.rds: 06_a_nest-success-prepare.R $(pd)master_df.rds $(pd)effort.rds
	Rscript $<

$(pd)hatching_success_models.rds: 06_b_nest-success-models.R $(pd)nest_success.rds
	Rscript $<
	
$(pd)fledging_success_models.rds: 06_b_nest-success-models.R $(pd)nest_success.rds
	Rscript $<
	
# figures -----------------------------------------------------------

figures: $(f)seasonality.pdf $(f)periodogram.pdf $(f)trend.pdf

$(f)periodogram.pdf: fig_periodogram.R $(pd)new_nest_wav.rds
	Rscript $<

$(f)seasonality.pdf: fig_seasonality.R $(pd)new_nest_models.rds $(pd)hatching_success_models.rds $(pd)fledging_success_models.rds
	Rscript $<

$(f)trend.pdf: fig_trend.R $(pd)new_nest_models.rds $(pd)hatching_success_models.rds $(pd)fledging_success_models.rds
	Rscript $<

# tables -----------------------------------------------------------

tables: $(t)tab_summary_gams.csv

$(t)tab_summary_gams.csv: tab_gams.R $(pd)new_nest_models.rds $(pd)hatching_success_models.rds $(pd)fledging_success_models.rds
	Rscript $<


## number of new nests over time
# new_nests: $(pd)new_nests.rds



# nest success over time
# success: $(pd)nest_success.rds

# $(pd)nest_success.rds: 06_nest-success.R new_nests
# 	Rscript $<

# data analysis ---------------------------------------------------------------