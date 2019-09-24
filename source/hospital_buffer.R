#Greyspot analysis
#############################Upload package libraries 
source("source/util.R")
reload_source()
out_wd <- "generated_data/"
fig_wd <- "figures/"
d <- seq(5, 60, by = 5) ## define buffer size

#############################Load data
##load bangladesh shapefiles
bang.map<-readRDS("data/BGD_adm2.rds") %>% st_as_sf() %>% dplyr::mutate(adm2name=NAME_2)
bang.map0<-readRDS("data/BGD_adm0.rds") %>% st_as_sf() 
##load sentinel surv hospital location data file
hosp<-read_csv("data/hosp_GPS.csv")
##load BGD population raster
bgd_pop <- raster("data/BGD_ppp_2015_adj_v2.tif")
bgd_pop_v <- velox(bgd_pop)

############################# Buffers
##hospital locations transformed to populate buffers
hosp_loc <- sf::st_as_sf(hosp %>% dplyr::select(LON,LAT), coords = c("LON","LAT"),crs="+proj=longlat +datum=WGS84 +no_defs")
hosp_transformed <- st_transform(hosp_loc,"+proj=tmerc +lat_0=0 +lon_0=90 +k=0.9996 +x_0=500000 +y_0=0 +a=6377276.345 +b=6356075.41314024 +towgs84=283.7,735.9,261.1,0,0,0,0 +units=km +no_defs")

##to generate sf objects where buffer polygons are combined into a single multipolygon
for (i in unique(d)) {
  #population within d radius of hospital
  buff <- st_buffer(hosp_transformed, dist=i, nQuadSegs = 30, endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1)
  dub_buffer <- st_transform(buff, 4326)

  ## singlepolygons per sentinel site
  pop_dist_buff <-bgd_pop_v$extract(sp=dub_buffer, fun=function(x) sum(x, na.rm=T))
  #pop_dist_buff_df<-data.frame(cbind(1:22,pop_dist_buff)) %>% dplyr::rename(HOSP=X1,pop=X2) #to create separate df with pop data
  sp_file <- dub_buffer %>% dplyr::mutate(pop=pop_dist_buff)
  st_write(sp_file, paste0(out_wd, "buff_sf_singlepoly_", i, "km.shp"))

  ## multipolygons
  #combine buffer polygons per hospital into single multipolygon
  dub_buffer_mp <- st_sf(st_union(dub_buffer))
  #measure population in all buffers of the multipolygon
  pop_dist_buff_mp <- bgd_pop_v$extract(sp=dub_buffer_mp, fun=function(x) sum(x, na.rm=T))
  mp_file <- dub_buffer_mp %>% dplyr::mutate(pop = pop_dist_buff_mp)
  st_write(mp_file, paste0(out_wd, "buff_sf_multipoly_", i, "km.shp"))
  
  ## save plot
  buff_map <- map_buffers(bang.map, bang.map, hosp_loc, mp_file)
  ggsave(paste0(fig_wd, "buff_", i, "km.pdf"), buff_map, width = 4, height = 4)
}

