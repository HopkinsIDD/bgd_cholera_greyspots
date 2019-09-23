#Greyspot analysis
#############################Upload package libraries 
source("source/util.R")
reload_source()


#############################Load data
##load bangladesh shapefiles
bang.map<-readRDS("data/BGD_adm2.rds") %>% st_as_sf() %>% mutate(adm2name=NAME_2)
bang.map0<-readRDS("data/BGD_adm0.rds") %>% st_as_sf() 
##load sentinel surv hospital location data file
hosp<-read_csv("data/hosp_GPS.csv")
##load BGD population raster
bgd_pop <- raster("data/Bangladesh 100m Population/BGD_ppp_2015_adj_v2.tif")
bgd_pop_v <- velox(bgd_pop)


#############################Buffers
##hospital locations transformed to populate buffers
hosp_loc <- sf::st_as_sf(hosp %>% dplyr::select(LON,LAT),coords = c("LON","LAT"),crs="+proj=longlat +datum=WGS84 +no_defs")
hosp_transformed <- st_transform(hosp_loc,"+proj=tmerc +lat_0=0 +lon_0=90 +k=0.9996 +x_0=500000 +y_0=0 +a=6377276.345 +b=6356075.41314024 +towgs84=283.7,735.9,261.1,0,0,0,0 +units=km +no_defs")

##to generate sf objects where buffer polygons are combined into a single multipolygon
d <- c(10,25,50) #define buffer size
buff.sf.multi <- list()
for (i in unique(d)) {
  #population within d radius of hospital
  buff<-st_buffer(hosp_transformed, dist=i, nQuadSegs = 30,endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1)
  dub_buffer <- st_transform(buff, 4326)
  x<-st_union(dub_buffer) #combine buffer polygons per hospital into single multipolygon
  order<-paste('buff',i,sep='.')
  buff.sf.multi[[order]]<- x
}

saveRDS(buff.sf.multi,"healthcare_util_BGD/data/buff_sf_multipolygons.rds")

##to generate sf objects where buffer polygons overlap and to get population size in each buffer
d <- c(10,25,50)
buff.sf <- list()
for (i in unique(d)) {
  
  #population within d radius of hospital
  buff<-st_buffer(hosp_transformed, dist=i, nQuadSegs = 30,endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1)
  dub_buffer <- st_transform(buff, 4326)
  
  #measure population within each buffer
  pop_dist_buff<-bgd_pop_v$extract(sp=dub_buffer, fun=function(x) sum(x, na.rm=T))
  #pop_dist_buff_df<-data.frame(cbind(1:22,pop_dist_buff)) %>% dplyr::rename(HOSP=X1,pop=X2) #to create separate df with pop data
  dub_buffer <- dub_buffer %>% dplyr::mutate(pop=pop_dist_buff)
 
  order<-paste('buff',i,sep='.')
  buff.sf[[order]] <- dub_buffer
}

saveRDS(buff.sf,"healthcare_util_BGD/data/buff_sf_polygons.rds")

##to map buffers
mapview(dub_buffer)

ggplot() + 
  geom_sf(data=bang.map0,fill=NA,lwd=0.1, alpha=.1) + 
  geom_sf(data=buff.sf.multi[[10]],color="red",alpha=.4,show.legend="point") +
  geom_sf(data=hosp_loc,shape=3,color="black",size=1,lty=4) +
  coord_sf(datum=NA) + 
  labs(x="") + labs(y="") + 
  theme_void() + 
  ggsn::north(bang.map) +
  scalebar(bang.map, dist = 50, dist_unit = "km",transform = TRUE, model = "WGS84", st.size = 3)

