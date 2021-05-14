reload_source <- function(){
  library(sf)
  library(dplyr)
  library(exactextractr)
  library(raster)
  library(tidyr)
  library(ggplot2)
  library(ggpubr)
  library(ggsn)
  library(mapview)
  library(readr)
  library(purrr)
  library(patchwork)
  library(formattable)
  library(data.table)
  library(fasterize)
  library(lwgeom)
  library(cowplot)
  library(colorspace)
  library(scales)
  library(magrittr)
  library(ggpol)
  library(numform)
  library(ggforce)
  library(ggpattern) 
  
  source("source/util.R")
  
}

## map buffer zones
map_buffers <- function(map0_shp, map2_shp, hosp_coord, polyfile){
  # mapview(polyfile)
  plt <- ggplot() + 
    geom_sf(data = map0_shp, fill = NA, lwd = 0.1, alpha = .1) + 
    geom_sf(data = polyfile, color = "red", alpha = .4, show.legend = "point") +
    geom_sf(data = hosp_coord, shape = 3, color = "black", size = 1, lty = 1) +
    coord_sf(datum = NA) + 
    labs(x = "") + labs(y = "") + 
    theme_void() + 
    ggsn::north(map2_shp) +
    ggsn::scalebar(map2_shp, dist = 50, dist_unit = "km", transform = TRUE, model = "WGS84", st.size = 2)
  return(plt)
}

## transforms sf file to Bangladesh Transverse Mercator projection
transform_to_btm <- function(my_sf){
  st_transform(my_sf, crs="+proj=tmerc +lat_0=0 +lon_0=90 +k=0.9996 +x_0=500000 +y_0=0 +a=6377276.345 +b=6356075.41314024 +towgs84=283.7,735.9,261.1,0,0,0,0 +units=m +no_defs")
  
}

## aligns Zila names with gadm name standards
fix_adm2names <- function(dat){
  #bang.map$adm2name[which(!bang.map$adm2name %in% awd$adm2name)]
  dat %>% mutate(adm2name = recode(adm2name,
                                   "Barguna" = "Borgona",
                                   "Munshiganj" = "Munshigonj",
                                   "Mymensingh" = "Nasirabad",
                                   "Netrokona" = "Netrakona",
                                   "Narsingdi" = "Narshingdi",
                                   "Kushtia" = "Kustia",
                                   "Chapai Nawabganj" = "Nawabganj",
                                   "Gaibandha" = "Gaibanda",
                                   "Rangpur" = "Rongpur",
                                   "Habiganj" = "Hobiganj",
                                   "Maulvibazar" = "Moulvibazar",
                                   "Cox's bazar"="Cox's Bazar",
                                   "Jhalokati"="Jhalakati",
                                   "Bandarban"="Bandarbon",
                                   "Khagrachhari"="Khagrachari",
                                   "Rangamati"="Parbattya Chattagram",
                                   "Gopalganj"="Gopalgonj",
                                   "Manikganj"="Manikgonj",
                                   "Narayanganj"="Naray Angonj",
                                   "Chuadanga"="Choua Danga",
                                   "Satkhira"="Shatkhira",
                                   "Joypurhat"="Jaipurhat",
                                   "Chapainawabganj"="Nawabganj",
                                   "Sirajganj"="Sirajgonj",
                                   "Sunamganj"="Sun Amgonj"
                                   ))
}

