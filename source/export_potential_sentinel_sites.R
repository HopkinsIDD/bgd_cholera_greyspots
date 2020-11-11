## This script identifies all potential sentinel surveillance sites according to the Bangladesh Organization Registry and geocodes them using Google Earth and Open Street Maps (OSM)

library(tidyverse)
library(readxl)
library(tidygeocoder)
library(ggmap)

## 1) Register for a Google Earth API key and enable geocoding capabilities (https://cloud.google.com/maps-platform/ or see ?ggmap::register_google)
## 2) Make a file that is called "google_api_key.R" that contains one line `ggmap::register_google(key = <your-api-key-here>)`
source("source/google_api_key.R") 

hc_path <- "data/healthcare_facilities/"
fns <- paste0(hc_path, list.files(hc_path, pattern = "Registry Report"))

hc_all <- map_dfr(fns, function(x) {
  read_xls(x, skip = 9, col_names=c("org_name", "org_code", "division", "district", "upazila", "agency", "org_type", "org_func", "org_lvl", "mobile_num", "email", "approved", "revenue_bed_number", "development_bed_number", "electricity_src"), col_types="text")
})

hc_potent <- hc_all %>%
  dplyr::filter(grepl("Hospital/Clinic", org_func)) %>%
  dplyr::filter(org_lvl %in% c("National", "Regional", "Divisional", "District", "Ward", "Union", "Upazila"))

org_types <- hc_potent %>% distinct(org_type) %>% arrange(org_type)
# write_csv(org_types, "data/healthcare_facilities/org_types.csv")

## include only the relevant hospital types
hc_only <- hc_potent %>%
  dplyr::filter(grepl("200-250 bed", org_type) |
                grepl("300-500 bed", org_type) |
                grepl("General Hospital", org_type) |
                org_type %in% c("District Hospital", "Medical College Hospital", "Upazila Health Complex"))

# final_list %>% group_by(division, org_type, org_lvl) %>% count %>% write_csv("data/healthcare_facilities/summary_potential_sentinels.csv")

## geocode by google earth API
geod <- hc_only %>%
  dplyr::mutate(address = glue::glue("{org_name}, {division}, {district}, {upazila}, Bangladesh")) %>%
  dplyr::mutate(address2 = glue::glue("{org_name}, Bangladesh")) %>%
  dplyr::mutate(address3 = glue::glue("{org_name}, {division}, Bangladesh")) %>%
  dplyr::mutate(uqid = seq_along(address)) %>%
  ggmap::mutate_geocode(address2)

## geocode by open street maps
full1 <- tidygeocoder::geo(address = geod$address, method="osm", verbose = TRUE)
full2 <- tidygeocoder::geo(address = geod$address2, method="osm", verbose = TRUE)
full3 <- tidygeocoder::geo(address = geod$address3, method="osm", verbose = TRUE)

## combine all identified OSM coords for more complete set
coords <- full_join(full1 %>% 
                dplyr::rename(lat1=lat, lon1=long) %>% 
                dplyr::mutate(uqid=seq_along(address)), 
                full2 %>% 
                dplyr::rename(lat2=lat, lon2=long) %>% 
                dplyr::mutate(uqid=seq_along(address)), by = c("uqid")) %>% 
  full_join(full3 %>% 
              dplyr::rename(lat3=lat, lon3=long) %>% 
              dplyr::mutate(uqid = seq_along(address)), by = c("uqid")) %>%
  dplyr::mutate(lat_osm = ifelse(!is.na(lat1), lat1, ifelse(!is.na(lat3), lat3, ifelse(!is.na(lat2), lat2, NA)))) %>%
  dplyr::mutate(lon_osm = ifelse(!is.na(lon1), lon1, ifelse(!is.na(lon3), lon3, ifelse(!is.na(lon2), lon2, NA)))) %>%
  select(uqid, lat_osm, lon_osm)

## prefer Google Earth coordinates then OSM coordinates
all <- full_join(geod %>% dplyr::rename(lat_goo = lat, lon_goo = lon), coords) %>%
  dplyr::select(-contains("address"), -mobile_num, -email, -approved, -electricity_src, -contains("bed_number")) %>%
  dplyr::mutate(lat = ifelse(!is.na(lat_goo), lat_goo, lat_osm),
                lon = ifelse(!is.na(lon_goo), lon_goo, lon_osm)) %>%
  dplyr::select(-contains("lat_"), -contains("lon_"), -uqid) %>%
  dplyr::filter(!is.na(lat))

## out of 504 possible sentinels sites, only 13 unable to be found by either API
write_csv(all, "data/healthcare_facilities/geocoded_potential_sentinels.csv")