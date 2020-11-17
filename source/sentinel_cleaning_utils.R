#' @description Clean the original sentinel spreadsheet to match org registry data
clean_sentinels_data <- function(df){
  df %>%
    dplyr::mutate(division = dplyr::recode(Division,
                      Barisal = "Barishal",
                      Chittagong = "Chattogram")) %>%
    dplyr::rename(org_lvl = Type) %>%
    dplyr::select(-Division) %>%
    dplyr::filter(org_lvl != "icddrb")
}

#' @description Clean the combined org registry spreadsheet to match original sentinel spreadsheet
clean_allhosp_data <- function(df){
  df %>%
    dplyr::mutate(org_lvl = recode(org_lvl,
                      Upazila = "subdistrict",
                      District = "district",
                      Regional = "tertiary",
                      National = "tertiary")) %>%
    dplyr::select(-agency, -org_func, -org_type)
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
    warning("generate_division failed")
  }

  return(new_sample)
}