## This function will generate a dataframe that is the same size and with the same stratification as orig_sample, according to the column weights specified in weight_column 
generate_weighted <- function(orig_sample, allhosp, weight_column){
  num_hosp_types <- orig_sample %>% group_by(type) %>% count

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

## This function will generate a dataframe that is the same size and with the same stratification as orig_sample -- a random stratified sample
generate_random <- function(orig_sample, allhosp){
  num_hosp_types <- orig_sample %>% group_by(type) %>% count

  new_sample <- allhosp %>%
    group_by(type) %>%
    dplyr::group_modify(function(x, y) {
      dplyr::slice_sample(x, 
        n = num_hosp_types[which(num_hosp_types$type == y$type[1]),]$n) 
    }) %>%
    ungroup %>%
    dplyr::select(names(orig_sample))

  return(new_sample)
}

