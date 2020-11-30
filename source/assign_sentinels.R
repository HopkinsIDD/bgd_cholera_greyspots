library(tidyverse)

source("source/sentinel_cleaning_utils.R")
source("source/sentinel_assignment_utils.R")

#### settings ####
seednum <- 544400
set.seed(seednum)
nsims <- 20
radii_vec <- c(10, 20, 30)
buffstring <- paste(radii_vec, collapse="-")

#### paths ####
out_dir <- "generated_data/"
core_str <- glue::glue("buff{buffstring}_seed{seednum}")

#### load files ####
sentinels <- read_csv("data/hosp_GPS_wicddrb.csv") %>%
  clean_sentinels_data()
allhosp <- read_csv("data/healthcare_facilities/geocoded_potential_sentinels.csv") %>%
  clean_allhosp_data()
shp <- load_shapefile(admin = 0)
allhosp_buff <- get_facilities_buffers(allhosp, radii_vec)
sero <- load_sero_grid()

#### calculate weights ####
if(!file.exists(glue::glue("{out_dir}weights_pop_{core_str}.csv")) | !file.exists(glue::glue("{out_dir}weights_rr_{core_str}.csv")) | !file.exists(glue::glue("{out_dir}weights_abs_{core_str}.csv"))){

  pop_wts <- get_facilities_weights(sero, allhosp_buff, "pop")
  rr_wts <- get_facilities_weights(sero, allhosp_buff, "rr")
  abs_wts <- get_facilities_weights(sero, allhosp_buff, "implied_inf")
  write_csv(pop_wts, glue::glue("{out_dir}weights_pop_{core_str}.csv"))
  write_csv(rr_wts, glue::glue("{out_dir}weights_rr_{core_str}.csv"))
  write_csv(abs_wts, glue::glue("{out_dir}weights_abs_{core_str}.csv"))
} else{
  pop_wts <- read_csv(glue::glue("{out_dir}weights_pop_{core_str}.csv"))
  rr_wts <- read_csv(glue::glue("{out_dir}weights_rr_{core_str}.csv"))
  abs_wts <- read_csv(glue::glue("{out_dir}weights_abs_{core_str}.csv"))
}

########## Optimized division selection ##############

#### population-division selection ####
popdiv_list <- lapply(seq.int(nsims), function(i){
  generate_division_optimized(sentinels, pop_wts) 
})
popdiv <- data.table::rbindlist(popdiv_list, idcol = "sim")
write_csv(popdiv, glue::glue("{out_dir}sentinelSamples_popDiv_{core_str}_nsims{nsims}.csv"))

#### relative risk-division selection ####
rrdiv_list <- lapply(seq.int(nsims), function(i){
  generate_division_optimized(sentinels, rr_wts) 
})
rrdiv <- data.table::rbindlist(rrdiv_list, idcol = "sim")
write_csv(rrdiv, glue::glue("{out_dir}sentinelSamples_rrDiv_{core_str}_nsims{nsims}.csv"))

#### absolute risk-division selection ####
absdiv_list <- lapply(seq.int(nsims), function(i){
  generate_division_optimized(sentinels, abs_wts) 
})
absdiv <- data.table::rbindlist(absdiv_list, idcol = "sim")
write_csv(absdiv, glue::glue("{out_dir}sentinelSamples_absDiv_{core_str}_nsims{nsims}.csv"))

########## Optimized equity selection ##############

#### population-division selection ####
popeq_list <- lapply(seq.int(nsims), function(i){
  generate_equity_optimized(sentinels, pop_wts) 
})
popeq <- data.table::rbindlist(popeq_list, idcol = "sim")
write_csv(popeq, glue::glue("{out_dir}sentinelSamples_popEq_{core_str}_nsims{nsims}.csv"))

#### relative risk-division selection ####
rreq_list <- lapply(seq.int(nsims), function(i){
  generate_equity_optimized(sentinels, rr_wts) 
})
rreq <- data.table::rbindlist(rreq_list, idcol = "sim")
write_csv(rreq, glue::glue("{out_dir}sentinelSamples_rrEq_{core_str}_nsims{nsims}.csv"))

#### absolute risk-division selection ####
abseq_list <- lapply(seq.int(nsims), function(i){
  generate_equity_optimized(sentinels, abs_wts) 
})
abseq <- data.table::rbindlist(abseq_list, idcol = "sim")
write_csv(abseq, glue::glue("{out_dir}sentinelSamples_absEq_{core_str}_nsims{nsims}.csv"))

########## Negative controls ##############

#### random selection ####
rdm_list <- lapply(seq.int(nsims), function(i){
  generate_random(sentinels, allhosp) 
})
rdm <- data.table::rbindlist(rdm_list, idcol = "sim")
write_csv(rdm, glue::glue("{out_dir}sentinelSamples_random_{core_str}_nsims{nsims}.csv"))

#### division selection ####
div_list <- lapply(seq.int(nsims), function(i){
  generate_division(sentinels, allhosp) 
})
div <- data.table::rbindlist(div_list, idcol = "sim")
write_csv(rdm, glue::glue("{out_dir}sentinelSamples_division_buff{buffstring}_seed{seednum}_nsims{nsims}.csv"))

######### Positive controls (deterministic) ##############

#### population-optimized selection ####
popopt_list <- lapply(seq.int(nsims), function(i){
  generate_optimized(sentinels, pop_wts) 
})
popopt <- data.table::rbindlist(popopt_list, idcol = "sim")
write_csv(popopt, glue::glue("{out_dir}sentinelSamples_popOpt_{core_str}_nsims{nsims}.csv"))

#### relative risk-optimized selection ####
rropt_list <- lapply(seq.int(nsims), function(i){
  generate_optimized(sentinels, rr_wts) 
})
rropt <- data.table::rbindlist(rropt_list, idcol = "sim")
write_csv(rropt, glue::glue("{out_dir}sentinelSamples_rrOpt_{core_str}_nsims{nsims}.csv"))

#### absolute risk-optimized selection ####
absopt_list <- lapply(seq.int(nsims), function(i){
  generate_optimized(sentinels, abs_wts) 
})
absopt <- data.table::rbindlist(absopt_list, idcol = "sim")
write_csv(absopt, glue::glue("{out_dir}sentinelSamples_absOpt_{core_str}_nsims{nsims}.csv"))