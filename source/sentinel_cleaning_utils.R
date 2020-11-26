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
    dplyr::rename(LAT = lat, LON = lon) %>%
    dplyr::select(-agency, -org_func, -org_type)
}

#' @description Get crs text
#' @param crs string to choose 'standard' BGD UTM projection or 'gadm' projection
get_crs <- function(crs = "standard"){
  if(crs == "standard"){
    crs_txt <- "+proj=tmerc +lat_0=0 +lon_0=90 +k=0.9996 +x_0=500000 +y_0=0 +a=6377276.345 +b=6356075.41314024 +towgs84=283.7,735.9,261.1,0,0,0,0 +units=km +no_defs" ## BGD UTM projection (Transverse Mercator)
  } else if(crs == "gadm"){
    crs_txt <- "+proj=longlat +datum=WGS84 +no_defs"
  } else{
    warning("crs argument was invalid. Returning standard crs.")
    crs_txt <- "+proj=tmerc +lat_0=0 +lon_0=90 +k=0.9996 +x_0=500000 +y_0=0 +a=6377276.345 +b=6356075.41314024 +towgs84=283.7,735.9,261.1,0,0,0,0 +units=km +no_defs" ## BGD UTM projection
  }
  return(crs_txt)
}

#' @description Get potential sentinels with buffers
#' @param df dataframe with healthcare facilities, LAT & LON coordinates, and org_lvl
#' @param radii numeric vector with radius in km for subdistrict, district, and tertiary care facilities
get_facilities_buffers <- function(df, radii){

  if(!("LON" %in% names(df)) | !("LAT" %in% names(df))){
    stop("The dataframe object df does not have the required columns LON and LAT.")
  } else if (!("org_lvl" %in% names(df))){
    stop("The df object does not have the required column org_lvl.")
  } else if(!is.numeric(radii) | length(radii) != 3){
    stop("The object radii is not a numeric vector of length 3.")
  }

  std_crs_txt <- get_crs("standard")
  gadm_crs_txt <- get_crs("gadm")

  buffer_radii <- data.frame(org_lvl = c("subdistrict", "district", "tertiary"), buff_radius = radii)
  df_sf_gadm <- sf::st_as_sf(df, coords = c("LON", "LAT"), crs = gadm_crs_txt, remove = FALSE) %>%
    left_join(buffer_radii, by = c("org_lvl"))
  df_sf <- sf::st_transform(df_sf_gadm, std_crs_txt)

  buff_sp <- sf::st_buffer(df_sf, dist = df_sf$buff_radius, nQuadSegs = 30, endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1) %>%
    dplyr::select(-buff_radius) %>%
    dplyr::mutate(org_name = df_sf$org_name,
                  org_code = df_sf$org_code,
                  division = df_sf$division,
                  org_lvl = df_sf$org_lvl)

  return(buff_sp)
}

#' @description Load shapefile
#' @param dir path to shapefile locations
#' @param admin numeric value for admin level shapefile
#' @param crs string denoting crs
load_shapefile <- function(dir = "data/", admin = 0, crs = "standard"){

  fn <- paste0(dir, "BGD_adm", admin, ".rds")
  shp <- readRDS(fn) %>% sf::st_as_sf()
  crs_txt <- get_crs(crs)
  rc <- sf::st_transform(shp, crs_txt)
  
  return(rc)
}

#' @description Load population raster 
#' @param fn filename to pop raster
load_pop_raster <- function(fn = "data/BGD_ppp_2015_adj_v2_UTM.tif"){

  pop <- raster::raster(fn)
  if(as.character(raster::crs(pop)) != get_crs("standard")){
    pop <- raster::projectRaster(from = pop, crs = get_crs("standard"), method = "bilinear")
  }

  return(pop)
}

#' @description Load relative risk grid 
#' @param fn filename to rr grid
load_sero_grid <- function(fn = "data/entropy-map.rds"){

  ## Import seroincidence estimates in Bangladesh based on a nationally-representative 2015 serosurvey
  ## New filename is entropy-map.rds and old is grid-cell-data-w-entropy.rds
  ## Notes:
    ## this file has duplicate grid cells here for mapping purposes. 
    ## one set of cols (toPlt) is the original file and ideal for plotting at grid cell level
    ## one set of cols (toAgg) is best used for summing across number of infections
    ## data from Steve from June 2020 (revisions to Lancet ID)
  sero_import <- readRDS(fn) %>% 
    dplyr::select(grid_id, pop, rr_median, rr_lb, rr_ub, inc_mean, inc_median, inc_lb, inc_ub, rr_med_bound, rr_lb_bound, rr_ub_bound, rr_int_log_diff, inc_interval_width, implied_inf, entropy2) %>%
    dplyr::mutate(prop_inf_toPlt = ifelse(pop>0, implied_inf/pop, 0)) %>%
    dplyr::mutate(pop_toAgg = pop,
                  implied_inf_toAgg = implied_inf,
                  prop_inf_toAgg = prop_inf_toPlt,
                  rr_median_toAgg = rr_median) %>%
    dplyr::rename(pop_toPlt = pop,
                  implied_inf_toPlt = implied_inf) 
  dup_ix <- which(duplicated(sero_import$grid_id))
  sero_import[dup_ix,]$pop_toAgg <- NA
  sero_import[dup_ix,]$implied_inf_toAgg <- NA
  sero_import[dup_ix,]$prop_inf_toAgg <- NA
  sero_import[dup_ix,]$rr_median_toAgg <- NA ## even when taking the mean, we don't want some cells to be double weighted

  std_crs_txt <- get_crs("standard")
  sero <- sf::st_transform(sero_import, crs = std_crs_txt)

  return(sero)
}

#' @description get weights for facilities dataset based on a passed raster or grid
#' @param weights_grid raster or sf dataframe with data for weights
#' @param df_buff facilities dataframe with buffer geometries
#' @param weight_var string with weight variable name (pop, rel_risk, abs_risk)
#' @param weight_transform transform to apply to weights (default = none)
get_facilities_weights <- function(weights_grid, df_buff, weight_var, weight_transform = "none"){
  if (any(grepl("raster", class(weights_grid), ignore.case = TRUE)) & weight_var == "pop"){

    ## This method is not preferred because extract seems to pull different data than the st_intersection code (e.g., there are discrepancies in the population within a buffer even though the data and buffers are the same)
    wts <- terra::extract(weights_grid, df_buff, fun=sum, na.rm = TRUE, touches = TRUE)
    wts_df <- df_buff %>%  
      dplyr::mutate(wt = wts[,1])

  } else if ("data.frame" %in% class(weights_grid) & weight_var == "pop"){

    wts_df <- sf::st_intersection(weights_grid, df_buff) %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(org_name, org_code, division, district, upazila, org_lvl) %>%
      dplyr::summarise(pop = sum(pop_toAgg, na.rm = TRUE), ncells = n(), LAT = first(LAT), LON = first(LON)) %>% 
      dplyr::ungroup() %>%
      dplyr::mutate(wt = pop/ncells) %>% ## pop density
      dplyr::select(-pop, -ncells)
  
  } else if ("data.frame" %in% class(weights_grid) & weight_var == "rr"){

    wts_df <- sf::st_intersection(weights_grid, df_buff) %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(org_name, org_code, division, district, upazila, org_lvl) %>%
      dplyr::summarise(wt = mean(rr_median_toAgg, na.rm = TRUE), LAT = first(LAT), LON = first(LON)) %>% 
      dplyr::ungroup() 

  } else if ("data.frame" %in% class(weights_grid) & weight_var == "implied_inf"){

    wts_df <- sf::st_intersection(weights_grid, df_buff) %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(org_name, org_code, division, district, upazila, org_lvl) %>%
      dplyr::summarise(implied_inf = sum(implied_inf_toAgg, na.rm = TRUE), ncells = n(), LAT = first(LAT), LON = first(LON)) %>% 
      dplyr::ungroup() %>%
      dplyr::mutate(wt = implied_inf/ncells) %>% ## infections per area
      dplyr::select(-implied_inf, -ncells)
  }

  if (weight_transform == "sq"){
    wts_df <- wts_df %>%
      dplyr::mutate(wt = wt^2)
  }
  
  return(wts_df)
}
