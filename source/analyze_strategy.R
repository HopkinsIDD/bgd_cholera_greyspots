library(tidyverse)
library(sf)
source("source/sentinel_cleaning_utils.R")
source("source/strategy_analysis_utils.R")

#### run options ####
option_list <- list(
  optparse::make_option(c("-n", "--nsims"), action = "store", default = 1, type="integer", help = "number of simulations per sentinel assignment strategy"),
  optparse::make_option(c("-b", "--boots"), action = "store", default = 2, type="integer", help = "number of bootstrap samples to use"),
  optparse::make_option(c("-f", "--filename"), action = "store", default = NULL, type="character", help = "sentinel assignment filename")
)

opt <- optparse::OptionParser(option_list = option_list) %>% optparse::parse_args()

nsimmax <- opt$nsims
nbootmax <- opt$boots
fn <- opt$filename

# nsimmax <- 1
# nbootmax <- 2
# fn <- "generated_data/nsims1/sentinelSamples_absOpt_buff10-20-30_seed544400_nsims1.csv"

#### settings ####
test_version <- FALSE
nsims <- 1:nsimmax 
nboots <- 1:nbootmax
mrc_vec = c("rr", "inf")
radii_vec <- c(10, 20, 30, 30)
buffstring <- paste(radii_vec, collapse="-")
quant_vec <- c(0.025, 0.25, 0.5, 0.75, 0.975)

#### paths ####
out_dir <- "generated_data/"
ss_dir <- paste0(out_dir, "sentSamp_nsims", nsimmax, "/")
tmp_dir <- paste0(out_dir, "tmp_nsims", nsimmax, "/")
dir.create(ss_dir, showWarnings = FALSE)
dir.create(tmp_dir, showWarnings = FALSE)
ss_filenames <- fn ## grep(paste0("nsims", max(nsims)), list.files(path = out_dir, pattern = "sentinelSamples"), value = TRUE)

#### helper ####
quibble <- function(x, q = c(0.25, 0.5, 0.75)) {
  tibble("{{ x }}" := c(quantile(x, q, na.rm = TRUE), mean(x, na.rm = TRUE)), "{{ x }}_q" := c(q, "mean"))
}

#### main ####

all_ids <- expand.grid(sim_id = nsims, rep_id = nboots, mrc = mrc_vec) %>% 
      dplyr::arrange(mrc, sim_id, rep_id)

## prepare cross-BGD total quantities ##
bootcat <- readr::read_csv("data/boot_cat_denominators.csv", col_types = "cccdd")
## total infections & pop in bangladesh
denominators <- dplyr::filter(bootcat, metric == "inf") %>%
  dplyr::group_by(rep) %>%
  dplyr::summarise(tot_inf = sum(inf_cat_total), tot_pop = sum(pop_cat_total))
tot_pop <- denominators[1,]$tot_pop ## same for all reps
denom_inf <- dplyr::select(denominators, rep, tot_inf)

## write surveillance zone outputs to file ##

if(test_version){
  ss_filenames <- ss_filenames[1]
}


for (ix in 1:length(ss_filenames)){

  ss_fn <- ss_filenames[ix]
  strat_code <- stringr::str_remove(ss_fn, "sentinelSamples_")
  out_fn <- paste0(out_dir, "survZoneBootData_", strat_code)

  print(paste("**** Starting", ss_fn, "****"))

  if(!file.exists(out_fn)){
    sentinel_df <- read_csv(paste0(ss_dir, ss_fn))

    # strategy_survzone_data_ls <- lapply(1:nrow(all_ids), function(i){
    for(i in 1:nrow(all_ids)){

      print(paste("** In", i, "of", nrow(all_ids), "combinations **"))
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
        dplyr::mutate(sim = sim_id,
                      implied_inf_toAgg = ifelse(is.na(pop_toAgg), NA, sample_implied_inf)) %>% ## make duplicate cells NA for aggregation purposes
        dplyr::rename(implied_inf_toPlt = sample_implied_inf,
                      rate_toPlt = sample_rate)

      tmp_fn <- paste0(tmp_dir, "tmpout_", i, ".csv")
      readr::write_csv(boot_buff_df, path = tmp_fn)
      
      rm(buff_sp, buff_mp, boot_buff_df, oneboot, onesent)
      gc()
    #   return(boot_buff_df)
    # })
    }

    tmp_fns <- list.files(path = tmp_dir, pattern = "tmpout")
    strategy_survzone_data_ls <- lapply(1:length(tmp_fns), function(j){
      readr::read_csv(paste0(tmp_dir, tmp_fns[j]))
    })

    strategy_survzone_data <- data.table::rbindlist(strategy_survzone_data_ls, use.names=TRUE, fill=TRUE)
    print(paste("**** Writing", out_fn, "****"))
    readr::write_csv(strategy_survzone_data, path = out_fn)

    rm(strategy_survzone_data_ls)
    gc()

  } else{
    strategy_survzone_data <- readr::read_csv(out_fn, col_types = readr::cols(rep = "c")) %>%
      dplyr::mutate(category = factor(category, levels = c("High", "Moderate", "Mild")))
  }
  
  print(paste("********", strat_code, "*********\n"))
  
  ## Calculate joint confidence intervals across sim (sentinel assignment) and rep (INLA sample) ##

  #### Part 1: Infections and population (number & percent) captured in cholera surveillance zones ####

  reps_survzone <- strategy_survzone_data %>%
    dplyr::group_by(metric, sim, rep) %>%
    dplyr::summarise(pop_survzone = sum(pop_toAgg, na.rm = TRUE),
                    inf_survzone = sum(implied_inf_toAgg, na.rm = TRUE)) %>%
    dplyr::mutate(prop_pop_survzone = pop_survzone/tot_pop)

  tab_survzone <- reps_survzone %>%
    dplyr::left_join(denom_inf, by = c("rep")) %>%
    dplyr::mutate(perc_inf_survzone = inf_survzone/pop_survzone*100, ## percent of surveillance zone with recent infections
                  perc_inf_tot = inf_survzone/tot_inf*100 ## percent of total infections
                  ) %>%
    dplyr::group_by(metric) %>%
    dplyr::summarise(quibble(pop_survzone, quant_vec),
                    quibble(prop_pop_survzone, quant_vec),
                    quibble(inf_survzone, quant_vec),
                    quibble(perc_inf_survzone, quant_vec),
                    quibble(perc_inf_tot, quant_vec)) %>%
    dplyr::rename(q = pop_survzone_q) %>%
    dplyr::select(-prop_pop_survzone_q, -inf_survzone_q, -perc_inf_survzone_q, -perc_inf_tot_q) %>%
    dplyr::arrange(metric, q) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(metric, q)

  #### Part 2: Distribution of infections and population captured in cholera surveillance zones by risk category ####

  reps_survzone_cat <- strategy_survzone_data %>%
    dplyr::group_by(metric, sim, rep, category) %>%
    dplyr::summarise(pop_cat_survzone = sum(pop_toAgg, na.rm = TRUE),
                    inf_cat_survzone = sum(implied_inf_toAgg, na.rm = TRUE)
                    ) %>%
    dplyr::left_join(reps_survzone, by = c("metric", "sim", "rep")) %>%
    dplyr::mutate(distr_inf_cat_survzone = inf_cat_survzone/inf_survzone*100,
                  distr_pop_cat_survzone = pop_cat_survzone/pop_survzone*100)

  tab_survzone_cat <- reps_survzone_cat %>%
    dplyr::group_by(metric, category) %>%
    dplyr::summarise(quibble(pop_cat_survzone, quant_vec),
                    quibble(inf_cat_survzone, quant_vec),
                    quibble(pop_survzone, quant_vec),
                    quibble(inf_survzone, quant_vec),
                    quibble(distr_inf_cat_survzone, quant_vec),
                    quibble(distr_pop_cat_survzone, quant_vec)) %>%
    dplyr::rename(q = pop_cat_survzone_q) %>%
    dplyr::select(-inf_cat_survzone_q, -pop_survzone_q, -inf_survzone_q, -distr_inf_cat_survzone_q, -distr_pop_cat_survzone_q) %>%
    dplyr::arrange(metric, category, q) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(metric, category, q)

  #### Part 3: Distribution of infections and population by risk category in cholera surveillance zones relative to all of Bangladesh ####
  # bootcat are the denominators
  tab_survzone_in_bang <- dplyr::left_join(reps_survzone_cat, bootcat, by = c("metric", "category", "rep")) %>%
    dplyr::mutate(perc_inf_cat_survzone = inf_cat_survzone/inf_cat_total*100,
                  perc_pop_cat_survzone = pop_cat_survzone/pop_cat_total*100) %>%
    dplyr::group_by(metric, category) %>%
    dplyr::summarise(quibble(inf_cat_survzone, quant_vec),
                    quibble(inf_cat_total, quant_vec),
                    quibble(pop_cat_survzone, quant_vec),
                    quibble(pop_cat_total, quant_vec),
                    quibble(perc_inf_cat_survzone, quant_vec),
                    quibble(perc_pop_cat_survzone, quant_vec)) %>%
    dplyr::rename(q = inf_cat_survzone_q) %>%
    dplyr::select(-inf_cat_total_q, -pop_cat_survzone_q, -pop_cat_total_q, -perc_inf_cat_survzone_q, -perc_pop_cat_survzone_q) %>%
    dplyr::arrange(metric, category, q) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(metric, category, q)


  save(tab_survzone, tab_survzone_cat, tab_survzone_in_bang, file = stringr::str_replace(paste0(out_dir, "outputTables_", strat_code), ".csv", ".RData"))

  # rm(strategy_survzone_data_ls)
  # gc()
}

# Rprof("prof.out")
# Rprof(NULL)
# summaryRprof("prof.out")$by.self


