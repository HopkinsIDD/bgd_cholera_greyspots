# ## This function will generate a dataframe that is the same size and with the same stratification as orig_sample, according to the column weights specified in weight_column 
# ## This is an old version of this function -- obsolete
# generate_weighted <- function(orig_sample, allhosp, weight_column){
#   num_hosp_types <- get_orglvl_counts(orig_sample)

#   if(!weight_column %in% names(allhosp)){
#     stop(glue::glue("The column specified for weights ({weight_column}) is missing from the allhosp dataframe."))
#   }
  
#   new_sample <- allhosp %>%
#     group_by(type) %>%
#     dplyr::group_modify(function(x, y) {
#       wt <- x %>% 
#         dplyr::select(tidyselect::all_of(weight_column)) %>% 
#         unlist %>%
#         unname
#       dplyr::slice_sample(x, 
#         n = num_hosp_types[which(num_hosp_types$type == y$type[1]),]$n,
#         weight_by = wt) 
#     }) %>%
#     ungroup %>%
#     dplyr::select(names(orig_sample))

#   return(new_sample)

# }

#' @description Generate a dataframe with the same number of facilities and org_lvl stratification as the orig_sample. Random selection
#' @param orig_sample dataframe with original set of sentinel locations. Must have org_lvl column
#' @param full_df dataframe with all potential sentinel locations. Must have org_lvl column
generate_random <- function(orig_sample, full_df){
  num_hosp_types <- get_orglvl_counts(orig_sample)
  dummy <- data.frame()
  nfails = 0
  max_counter = 1

  while(!isTRUE(dplyr::all_equal(num_hosp_types, dummy, convert = TRUE))){
    if(nrow(dummy)>0){
      nfails = nfails+1
      warning(glue::glue("****Try {nfails} of generate_random procedure failed****"))
      print(num_hosp_types)
      print(dummy)
    }
    if(nfails>max_counter){
      warning(glue::glue("There were more than {max_counter+1} generate_random failures. Redrawing strata..."))
      strata <- get_strata(orig_sample)
      nfails = 0
    }

    new_sample <- full_df %>%
      group_by(org_lvl) %>%
      dplyr::group_modify(function(x, y) {
        dplyr::slice_sample(x, 
          n = num_hosp_types[which(num_hosp_types$org_lvl == y$org_lvl[1]),]$n) 
      }) %>%
      ungroup %>%
      dplyr::select(division, org_lvl, org_name, org_code, district, upazila, LAT, LON)

    dummy <- dplyr::group_by(new_sample, org_lvl) %>% count %>% ungroup
    
  }

  return(new_sample)
}


#' @description Generate a dataframe with the same number of facilities and org_lvl and division stratification as the orig_sample. Division selection
#' @param orig_sample dataframe with original set of sentinel locations. Must have org_lvl column
#' @param full_df dataframe with all potential sentinel locations. Must have org_lvl column
generate_division <- function(orig_sample, full_df){
  strata <- get_strata(orig_sample)
  dummy <- data.frame()
  nfails = 0
  max_counter = 1
  
  while(!isTRUE(dplyr::all_equal(strata, dummy, convert = TRUE))){
    if(nrow(dummy)>0){
      nfails = nfails+1
      warning(glue::glue("****Try {nfails} of generate_division procedure failed****"))
      print(strata)
      print(dummy)
    }
    if(nfails>max_counter){
      warning(glue::glue("There were more than {max_counter+1} generate_division failures. Redrawing strata..."))
      strata <- get_strata(orig_sample)
      nfails = 0
    }

    new_sample <- full_df %>%
      group_by(division, org_lvl) %>%
      dplyr::group_modify(function(x, y) {
        dplyr::slice_sample(x,
          n = max(c(strata[which(strata$division == y$division & strata$org_lvl == y$org_lvl),]$n, 0)))
      }) %>%
      ungroup %>%
      dplyr::select(division, org_lvl, org_name, org_code, district, upazila, LAT, LON) %>%
      dplyr::arrange(division, org_lvl)

    dummy <- dplyr::group_by(new_sample, division, org_lvl) %>% count %>% ungroup
  }

  return(new_sample)
}

#' @description Generate a dataframe with the same number of facilities and org_lvl. Optimized selection
#' @param orig_sample dataframe with original set of sentinel locations. Must have org_lvl column
#' @param full_df dataframe with all potential sentinel locations. Must have org_lvl and wt columns
generate_optimized <- function(orig_sample, full_df){

  if(!"wt" %in% names(full_df)){
    stop(glue::glue("The wt column is missing from the full_df dataframe."))
  }

  strata <- get_orglvl_counts(orig_sample)
  dummy <- data.frame()
  nfails = 0
  max_counter = 1

  while(!isTRUE(dplyr::all_equal(strata, dummy, convert = TRUE))){
    if(nrow(dummy)>0){
      nfails = nfails+1
      warning(glue::glue("****Try {nfails} of generate_optimized procedure failed****"))
      print(strata)
      print(dummy)
    }

    new_sample <- full_df %>%
      group_by(org_lvl) %>%
      dplyr::group_modify(function(x, y) {
        dplyr::slice_max(x,
          order_by = wt, 
          n = max(c(strata[which(strata$org_lvl == y$org_lvl),]$n, 0)), 
          with_ties = FALSE)
      }) %>%
      ungroup %>%
      dplyr::select(division, org_lvl, org_name, org_code, district, upazila, wt, LAT, LON) %>%
      dplyr::arrange(division, org_lvl)

    dummy <- dplyr::group_by(new_sample, org_lvl) %>% count %>% ungroup
  }

  return(new_sample)
}

#' @description Generate a dataframe with the same number of facilities and org_lvl and division stratification as the orig_sample. Optimized-division selection
#' @param orig_sample dataframe with original set of sentinel locations. Must have org_lvl column
#' @param full_df dataframe with all potential sentinel locations. Must have org_lvl and wt columns
generate_division_optimized <- function(orig_sample, full_df){

  if(!"wt" %in% names(full_df)){
    stop(glue::glue("The wt column is missing from the full_df dataframe."))
  }

  strata <- get_strata(orig_sample)
  dummy <- data.frame()
  nfails = 0
  max_counter = 1

  while(!isTRUE(dplyr::all_equal(strata, dummy, convert = TRUE))){
    if(nrow(dummy)>0){
      nfails = nfails+1
      warning(glue::glue("****Try {nfails} of generate_division_optimized procedure failed****"))
      print(strata)
      print(dummy)
    }
    if(nfails>max_counter){
      warning(glue::glue("There were more than {max_counter+1} generate_division_optimized failures. Redrawing strata..."))
      strata <- get_strata(orig_sample)
      nfails = 0
    }

    new_sample <- full_df %>%
      group_by(division, org_lvl) %>%
      dplyr::group_modify(function(x, y) {
        dplyr::slice_max(x,
          order_by = wt, 
          n = max(c(strata[which(strata$division == y$division & strata$org_lvl == y$org_lvl),]$n, 0)), 
          with_ties = FALSE)
      }) %>%
      ungroup %>%
      dplyr::select(division, org_lvl, org_name, org_code, district, upazila, wt, LAT, LON) %>%
      dplyr::arrange(division, org_lvl)

    dummy <- dplyr::group_by(new_sample, division, org_lvl) %>% count %>% ungroup
  }

  return(new_sample)
}

#' @description Generate a dataframe with the same number of facilities and org_lvl stratification as the orig_sample and at least one facility per division. Optimized-equity selection.
#' @param orig_sample dataframe with original set of sentinel locations. Must have org_lvl column
#' @param full_df dataframe with all potential sentinel locations. Must have org_lvl and wt columns
generate_equity_optimized <- function(orig_sample, full_df){

  if(!"wt" %in% names(full_df)){
    stop(glue::glue("The wt column is missing from the full_df dataframe."))
  }

  strata <- get_strata_equity(orig_sample)
  strataNAs <- dplyr::filter(strata, is.na(division))
  strataDivs <- dplyr::filter(strata, !is.na(division))
  dummy <- data.frame()
  strataCheck <- dplyr::select(strataNAs, -division) %>% ungroup
  nfails = 0
  max_counter = 1

  while(!isTRUE(dplyr::all_equal(strataCheck, dummy, convert = TRUE))){ 
    if(nrow(dummy)>0){
      nfails = nfails+1
      warning(glue::glue("****Try {nfails} of generate_equity_optimized procedure failed****"))
      print(strataCheck)
      print(dummy)
    }
    if(nfails>max_counter){
      warning(glue::glue("There were more than {max_counter+1} generate_equity_optimized failures. Redrawing strata..."))
      strata <- get_strata_equity(orig_sample)
      strataNAs <- dplyr::filter(strata, is.na(division))
      strataDivs <- dplyr::filter(strata, !is.na(division))
      strataCheck <- dplyr::select(strataNAs, -division) %>% ungroup
      nfails = 0
    }
    ## 
    new_sample_NAs <- full_df %>%
      group_by(org_lvl) %>%
      dplyr::group_modify(function(x, y) {
        dplyr::slice_max(x,
          order_by = wt, 
          n = max(c(strataNAs[which(strataNAs$org_lvl == y$org_lvl),]$n, 0)), 
          with_ties = FALSE)
      }) %>%
      ungroup
    dummy <- dplyr::group_by(new_sample_NAs, org_lvl) %>% count %>% ungroup

    new_sample_Divs <- full_df %>%
      dplyr::filter(!org_code %in% new_sample_NAs$org_code) %>%
      group_by(division, org_lvl) %>%
      dplyr::group_modify(function(x, y){
        dplyr::slice_max(x,
          order_by = wt,
          n = max(c(strata[which(strata$division == y$division & strata$org_lvl == y$org_lvl),]$n, 0)))
      }) %>%
      ungroup

    new_sample <- dplyr::bind_rows(new_sample_NAs, new_sample_Divs) %>%
      dplyr::select(division, org_lvl, org_name, org_code, district, upazila, wt, LAT, LON) %>%
      dplyr::arrange(division, org_lvl)

  }

  return(new_sample)
}

#' @description Generate a dataframe with the same number of facilities and org_lvl and division stratification as the orig_sample. Weighted-division selection. This is obsolete and replaced by generate_division_optimized.
#' @param orig_sample dataframe with original set of sentinel locations. Must have org_lvl column
#' @param full_df dataframe with all potential sentinel locations. Must have org_lvl and wt columns
generate_division_weighted <- function(orig_sample, full_df){

  if(!"wt" %in% names(full_df)){
    stop(glue::glue("The wt column is missing from the full_df dataframe."))
  }

  strata <- get_strata(orig_sample)
  dummy <- data.frame()
  nfails = 0
  max_counter = 1

  while(!isTRUE(dplyr::all_equal(strata, dummy))){
    if(nrow(dummy)>0){
      nfails = nfails+1
      warning(glue::glue("****Try {nfails} of generate_division_weighted procedure failed****"))
      print(strata)
      print(dummy)
    }
    if(nfails>max_counter){
      warning(glue::glue("There were more than {max_counter+1} generate_division_weighted failures. Redrawing strata..."))
      strata <- get_strata(orig_sample)
      nfails = 0
    }

    new_sample <- full_df %>%
      group_by(division, org_lvl) %>%
      dplyr::group_modify(function(x, y) {
        dplyr::slice_sample(x,
          n = max(c(strata[which(strata$division == y$division & strata$org_lvl == y$org_lvl),]$n, 0)),
          weight_by = x$wt) 
      }) %>%
      ungroup %>%
      dplyr::select(division, org_lvl, org_name, org_code, district, upazila, wt, LAT, LON) %>%
      dplyr::arrange(division, org_lvl)

    dummy <- dplyr::group_by(new_sample, division, org_lvl) %>% count %>% ungroup
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

#' @description Return dataframe with the number of facilities by division and org_lvl that matches the orig_sample distribution of facilities by division and distribution of facilities by org_lvl
#' @param orig_sample dataframe with original set of sentinel locations. Must have division column
#' @param match_joint should the joint distribution of facilities by division and org_lvl match that of the orig_sample (default = FALSE)
get_strata <- function(orig_sample, match_joint = FALSE){
  if(!match_joint){
    num_hosp_types <- get_orglvl_counts(orig_sample) %>%
      tidyr::uncount(n) %>%
      dplyr::arrange(rnorm(nrow(orig_sample))) ## swap org_lvls across districts
    num_div_types <- get_division_counts(orig_sample) %>%
      tidyr::uncount(n) 
    strata <- tibble::as_tibble(cbind(num_div_types, num_hosp_types)) %>%
      group_by(division, org_lvl) %>% 
      count %>% 
      ungroup()
  } else{
    strata <- orig_sample %>%
      group_by(division, org_lvl) %>%
      count %>%
      ungroup()
  }
  return(strata)
}

#' @description Return dataframe with at least one facility per division that matches the orig_sample distribution of facilities by org_lvl and orig_sample number of facilities
#' @param orig_sample dataframe with original set of sentinel locations. Must have division column
get_strata_equity <- function(orig_sample){
  divs <- dplyr::distinct(orig_sample, division)
  num_divs <- nrow(divs)
  num_hosp_types <- get_orglvl_counts(orig_sample) %>%
    tidyr::uncount(n) %>%
    dplyr::arrange(rnorm(nrow(orig_sample))) %>%
    ungroup()
  strata_div <- tibble::as_tibble(cbind(divs, num_hosp_types[1:num_divs,])) %>%
    dplyr::mutate(n = 1)
  strata_na <- data.frame(division = NA, org_lvl = num_hosp_types[(num_divs+1):nrow(num_hosp_types),]$org_lvl) %>%
    group_by(division, org_lvl) %>%
    count %>%
    ungroup()
  strata_eq <- dplyr::bind_rows(strata_div, strata_na)
  
  return(strata_eq)
}
