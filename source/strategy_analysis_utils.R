
#' @description get icddrb hospital data for sentinel list
#' @return one row dataframe with icddrb sentinel site
get_icddrb_sentinel <- function(filename = "data/hosp_GPS_wicddrb.csv"){
  
  rc <- read_csv(filename) %>%
    dplyr::rename(division = Division, org_lvl = Type, org_name = Name) %>%
    dplyr::filter(org_lvl == "icddrb") %>%
    dplyr::select(division, org_lvl, org_name, LAT, LON)

  return(rc)
}


#' @description Load clean INLA bootstrapped data estimates
#' @param boots_fn filename for bootstrapped data
#' @param sero_fn filename for sero grid data
#' @param i rep number, ranging from 1 to 1000
load_boots_rep <- function(boots_fn = "data/all-boots.rds", sero_fn = "data/entropy-map.rds", i = 1){

  sero <- load_sero_grid(sero_fn) %>%
    dplyr::select(grid_id, pop_toAgg, pop_toPlt)

  boots <- readRDS(boots_fn) %>%
    dplyr::filter(rep == i) %>%
    tibble::as_tibble() %>%
    dplyr::select(grid_id, sample_rate, sample_total, sample_rr, rep) %>%
    dplyr::filter(grid_id %in% sero$grid_id) %>% ## 4 cells are missing
    dplyr::left_join(sero, by = c("grid_id")) %>%
    dplyr::rename(sample_implied_inf = sample_total) %>%
    dplyr::arrange(rep, grid_id)

  return(boots)
}


#' @description Get thresholds
#' @param metric string to choose 'rr' relative risk or 'inf' absolute number of infections or 'prop' proportion infected thresholds
#' @return numeric vector with length 2 where first element is the low-medium threshold and second element is the medium-high threshold
get_thresholds <- function(metric = "rr", sero_fn = "data/entropy-map.rds"){
  sero <- load_sero_grid(sero_fn)

  if(metric == "rr"){
    thresh <- round(quantile(sero$rr_median, probs = c(0.25, 0.75), na.rm = TRUE), 3)
  } else if(metric == "inf"){
    thresh <- round(quantile(sero$implied_inf_toAgg, probs = c(0.25, 0.75), na.rm = TRUE), 3)
  } else if (metric == "prop"){
    thresh <- round(quantile(sero$prop_inf_toAgg, probs = c(0.25, 0.75), na.rm = TRUE), 3)
  } else{
    stop("You have provided an invalid metric. Please check get_thresholds call.")
  }

  if(!is.vector(thresh) | !is.numeric(thresh)){
    stop("Thresholds object is not a vector or is not numeric. Please review get_thresholds.")
  } else if (length(thresh) != 2){
    stop("Thresholds object is not length 2. Please review get_thresholds.")
  }

  return(thresh)
}


#' @description Add threshold categories to dataframe
#' @param metric string to choose 'rr' relative risk or 'inf' absolute number of infections (default = 'rr')
#' @param sero_fn file path to original sero RDS file (default = 'data/entropy-map.rds')
#' @param boot_obj bootstrapped sample object, one rep from all-boots.rds
add_threshold_categories <- function(boot_obj, metric = "rr", sero_fn = "data/entropy-map.rds"){
  thresholds <- get_thresholds(metric, sero_fn)
  std_crs_txt <- get_crs(crs = "standard")

  if(metric == "rr"){
    boot_obj <- dplyr::rename(boot_obj, value = sample_rr) %>%
      dplyr::mutate(metric = "rr")
  } else if(metric == "inf"){
    boot_obj <- dplyr::rename(boot_obj, value = sample_implied_inf) %>%
      dplyr::mutate(metric = "inf")
  } else{
    stop("You have provided an invalid metric. Please check add_threshold_categories call. Only 'rr' and 'inf' are valid options.")
  }

  boot_obj <- dplyr::mutate(boot_obj, category = ifelse(value < thresholds[1], "Mild", 
                                            ifelse(value >= thresholds[1] & value < thresholds[2], "Moderate", "High"))) %>%
    dplyr::mutate(category = factor(category, levels = c("High", "Moderate", "Mild"))) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = std_crs_txt)

  return(boot_obj)
}