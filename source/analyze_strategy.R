library(tidyverse)
library(sf)
source("source/sentinel_cleaning_utils.R")
source("source/strategy_analysis_utils.R")

#### settings ####
clear_orig <- TRUE
nsims <- 1:2 ## sort(unique(sentinel_df$sim))
nboots <- 1:2
mrc_vec = c("rr", "inf")
radii_vec <- c(10, 20, 30, 30)
buffstring <- paste(radii_vec, collapse="-")
quant_vec <- c(0.025, 0.25, 0.5, 0.75, 0.975)

#### paths ####
out_dir <- "generated_data/"
ss_filenames <- list.files(path = out_dir, pattern = "sentinelSamples")[1]

#### helper ####
quibble <- function(x, q = c(0.25, 0.5, 0.75)) {
  tibble("{{ x }}" := c(quantile(x, q, na.rm = TRUE), mean(x, na.rm = TRUE)), "{{ x }}_q" := c(q, "mean"))
}

#### main ####
if(clear_orig){
  file.remove(paste0(out_dir, list.files(path = out_dir, pattern = "survZoneBootData_")))
}
all_ids <- expand.grid(sim_id = nsims, rep_id = nboots, mrc = mrc_vec) %>% 
      dplyr::arrange(mrc, sim_id, rep_id)

## prepare cross-BGD total quantities ##
sero <- load_sero_grid()
tot_inf <- sum(sero$implied_inf_toAgg, na.rm = TRUE) ## total infections in bgd
tot_pop <- sum(sero$pop_toAgg, na.rm = TRUE) ## total population in bgd

## prepare cross-BGD quantities by category ##
## 12/9 I THINK THIS SECTION NEEDS TO BE REDONE WITH THE BOOTSTRAPPED SAMPLES ##
thresh_rr <- get_thresholds("rr")
thresh_inf <- get_thresholds("inf")
sero_cat_thresh <- sero %>%
  dplyr::mutate(cat_rr = ifelse(rr_median_toAgg < thresh_rr[1], "Mild", ifelse(rr_median_toAgg >= thresh_rr[1] & rr_median_toAgg < thresh_rr[2], "Moderate", "High")),
                cat_inf = ifelse(implied_inf_toAgg < thresh_inf[1], "Mild", ifelse(implied_inf_toAgg >= thresh_inf[1] & implied_inf_toAgg < thresh_inf[2], "Moderate", "High"))) %>%
  dplyr::mutate(cat_rr = factor(cat_rr, levels = c("High", "Moderate", "Mild")),
                cat_inf = factor(cat_inf, levels = c("High", "Moderate", "Mild"))) %>%
  sf::st_drop_geometry()
serocat_inf <- sero_cat_thresh %>%
  dplyr::rename(category = cat_inf) %>%
  dplyr::group_by(category) %>%
  dplyr::summarise(inf_cat_total = round(sum(implied_inf_toAgg, na.rm = TRUE), 0),
                  pop_cat_total = sum(pop_toAgg, na.rm = TRUE)) %>%
  dplyr::mutate(metric = "inf") 
serocat_rr <- sero_cat_thresh %>%
  dplyr::rename(category = cat_rr) %>%
  dplyr::group_by(category) %>%
  dplyr::summarise(inf_cat_total = round(sum(implied_inf_toAgg, na.rm = TRUE), 0),
                  pop_cat_total = sum(pop_toAgg, na.rm = TRUE)) %>%
  dplyr::mutate(metric = "rr") 
serocat <- dplyr::bind_rows(serocat_inf, serocat_rr) %>%
  dplyr::select(metric, category, inf_cat_total, pop_cat_total) %>%
  dplyr::filter(!is.na(category))

## write surveillance zone outputs to file ##

for (ix in 1:length(ss_filenames)){

  ss_fn <- ss_filenames[ix]
  strat_code <- stringr::str_remove(ss_fn, "sentinelSamples_")
  out_fn <- paste0(out_dir, "survZoneBootData_", strat_code)

  if(!file.exists(out_fn)){
    sentinel_df <- read_csv(paste0(out_dir, ss_fn))

    strategy_survzone_data_ls <- lapply(1:nrow(all_ids), function(i){
      rep_id <- all_ids[i,]$rep_id
      sim_id <- all_ids[i,]$sim_id
      mrc <- all_ids[i,]$mrc

      oneboot <- load_boots_rep(i = rep_id) %>%
        add_threshold_categories(metric = mrc)

      onesent <- dplyr::bind_rows(
                    dplyr::filter(sentinel_df, sim == sim_id),
                    get_icddrb_sentinel()
                  )

      buff_sp <- get_facilities_buffers(onesent, radii_vec)
      buff_mp <- sf::st_sf(sf::st_union(buff_sp))
      boot_buff_df <- sf::st_intersection(oneboot, buff_mp) %>%
        sf::st_drop_geometry() %>%
        tibble::as_tibble() %>%
        dplyr::mutate(sim = sim_id)

      return(boot_buff_df)
    })

    strategy_survzone_data <- data.table::rbindlist(strategy_survzone_data_ls)
    write_csv(strategy_survzone_data, path = out_fn)

    rm(strategy_survzone_data_ls)
    gc()

  } else{
    strategy_survzone_data <- read_csv(out_fn) %>%
      dplyr::mutate(category = factor(category, levels = c("High", "Moderate", "Mild")))
  }
  
  print(paste("********", strat_code, "*********\n"))
  
  ## Calculate joint confidence intervals across sim (sentinel assignment) and rep (INLA sample) ##

  #### Part 1: Infections and population (number & percent) captured in cholera surveillance zones ####
  tab_survzone <- strategy_survzone_data %>%
    dplyr::group_by(metric, sim, rep) %>%
    dplyr::summarise(pop_survzone = sum(pop_toAgg, na.rm = TRUE),
                    inf_survzone = sum(sample_implied_inf)) %>%
    dplyr::mutate(prop_pop_survzone = pop_survzone/tot_pop) %>%
    dplyr::group_by(metric) %>%
    dplyr::summarise(quibble(pop_survzone, quant_vec),
                    quibble(prop_pop_survzone, quant_vec),
                    quibble(inf_survzone, quant_vec)) %>%
    dplyr::rename(q = pop_survzone_q) %>%
    dplyr::select(-prop_pop_survzone_q, -inf_survzone_q) %>%
    dplyr::mutate(perc_inf_survzone = round(inf_survzone/pop_survzone*100, 1), ## percent of surveillance zone with recent infections
                  perc_inf_tot = round(inf_survzone/tot_inf*100, 1)) %>% ## percent of total infections
    dplyr::arrange(metric, q) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(metric, q)

  #### Part 2: Distribution of infections and population captured in cholera surveillance zones by risk category ####
  tab_survzone_cat <- strategy_survzone_data %>%
    dplyr::group_by(metric, sim, rep, category) %>%
    dplyr::summarise(pop_cat_survzone = sum(pop_toAgg, na.rm = TRUE),
                    inf_cat_survzone = sum(sample_implied_inf)) %>%
    dplyr::group_by(metric, category) %>%
    dplyr::summarise(quibble(pop_cat_survzone, quant_vec),
                    quibble(inf_cat_survzone, quant_vec)) %>%
    dplyr::rename(q = pop_cat_survzone_q) %>%
    dplyr::select(-inf_cat_survzone_q) %>%
    dplyr::left_join(dplyr::select(tab_survzone, metric, q, pop_survzone, inf_survzone), by = c("metric", "q")) %>%
    dplyr::mutate(distr_inf_cat_survzone = round(inf_cat_survzone/inf_survzone*100, 1),
                  distr_pop_cat_survzone = round(pop_cat_survzone/pop_survzone*100, 1)) %>%
    dplyr::arrange(metric, category, q) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(metric, category, q)

  #### Part 3: Distribution of infections and population by risk category in cholera surveillance zones relative to all of Bangladesh ####
  # serocat are the denominators
  tab_survzone_in_bang <- dplyr::left_join(tab_survzone_cat, serocat, by = c("metric", "category")) %>%
    dplyr::mutate(perc_inf_cat_survzone = round(inf_cat_survzone/inf_cat_total*100, 1),
                  perc_pop_cat_survzone = round(pop_cat_survzone/pop_cat_total*100, 1))
    ## IN SOME CASES THE PERC>100 AND I THINK IT IS BECAUSE WE WERE NOT USING THE FULL RANGE OF THE DISTRIBUTION TO ESTIMATE TOTAL INFECTIONS AND POPULATIONS ##


  # rm(strategy_survzone_data_ls)
  # gc()
}




