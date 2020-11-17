## This function will generate a dataframe that is the same size and with the same stratification as orig_sample, according to the column weights specified in weight_column 
generate_weighted <- function(orig_sample, allhosp, weight_column){
  num_hosp_types <- get_orglvl_counts(orig_sample)

  if(!weight_column %in% names(allhosp)){
    stop(glue::glue("The column specified for weights ({weight_column}) is missing from the allhosp dataframe."))
  }
  
  new_sample <- allhosp %>%
    group_by(type) %>%
    dplyr::group_modify(function(x, y) {
      wt <- x %>% 
        dplyr::select(tidyselect::all_of(weight_column)) %>% 
        unlist %>%
        unname
      dplyr::slice_sample(x, 
        n = num_hosp_types[which(num_hosp_types$type == y$type[1]),]$n,
        weight_by = wt) 
    }) %>%
    ungroup %>%
    dplyr::select(names(orig_sample))

  return(new_sample)

}

#' @description Generate a dataframe with the same number of facilities and org_lvl stratification as the orig_sample. Random selection
#' @param orig_sample dataframe with original set of sentinel locations. Must have org_lvl column
#' @param full_df dataframe with all potential sentinel locations. Must have org_lvl column
generate_random <- function(orig_sample, full_df){
  num_hosp_types <- get_orglvl_counts(orig_sample)

  new_sample <- full_df %>%
    group_by(org_lvl) %>%
    dplyr::group_modify(function(x, y) {
      dplyr::slice_sample(x, 
        n = num_hosp_types[which(num_hosp_types$org_lvl == y$org_lvl[1]),]$n) 
    }) %>%
    ungroup 

  dummy <- dplyr::group_by(new_sample, org_lvl) %>% count
  if(!all(num_hosp_types == dummy)){
    warning("The generate_random procedure failed")
  }

  return(new_sample)
}


#' @description Generate a dataframe with the same number of facilities and org_lvl and division stratification as the orig_sample. Division selection
#' @param orig_sample dataframe with original set of sentinel locations. Must have org_lvl column
#' @param full_df dataframe with all potential sentinel locations. Must have org_lvl column
generate_division <- function(orig_sample, full_df){
  num_hosp_types <- get_orglvl_counts(orig_sample) %>%
    tidyr::uncount(n) %>%
    dplyr::arrange(rnorm(nrow(orig_sample)))
  num_div_types <- get_division_counts(orig_sample) %>%
    tidyr::uncount(n) 
  strata <- tibble::as_tibble(cbind(num_div_types, num_hosp_types)) %>%
    group_by(division, org_lvl) %>% 
    count 

  new_sample <- full_df %>%
    group_by(division, org_lvl) %>%
    dplyr::group_modify(function(x, y) {
      dplyr::slice_sample(x,
        n = max(c(strata[which(strata$division == y$division & strata$org_lvl == y$org_lvl),]$n, 0)))
    }) %>%
    ungroup

  dummy <- dplyr::group_by(new_sample, division, org_lvl) %>% count
  if(!all(strata == dummy)){
    warning("The generate_division procedure failed")
  }

  return(new_sample)
}

#' @description Return dataframe with the number of facilities by org_lvl
#' @param orig_sample dataframe with original set of sentinel locations. Must have org_lvl column
get_orglvl_counts <- function(orig_sample){
  if (!is.data.frame(orig_sample)){
    stop("orig_sample must be a dataframe.")
  } else if(!"org_lvl" %in% names(orig_sample)){
    stop("The column org_lvl is missing from orig_sample.")
  } 
  orig_sample %>% group_by(org_lvl) %>% count
}

#' @description Return dataframe with the number of facilities by division
#' @param orig_sample dataframe with original set of sentinel locations. Must have division column
get_division_counts <- function(orig_sample){
  if (!is.data.frame(orig_sample)){
    stop("orig_sample must be a dataframe.")
  } else if(!"division" %in% names(orig_sample)){
    stop("The column division is missing from orig_sample.")
  } 
  orig_sample %>% group_by(division) %>% count
}