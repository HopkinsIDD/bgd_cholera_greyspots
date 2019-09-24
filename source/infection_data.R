######code to process infection data
######things to estimate
###number and percent of infections in catchment
###number and percent of people in high risk infection areas (by rr and case number), medium, low that are in sentinel polygons 
###percent of population captured in clinical surveillance that are in high risk areas 


#load data
clinical <- readRDS("data/gam_cholera_bgd_est.rds") 

#create risk categories
##look at cases - distribution of number of cases in country and categorize case numbers to plot
hist(clinical$chol_est_case,main = "Distribution of cholera cases", xlim = c(0,500), xlab = "Number of cases")
summary(clinical$chol_est_case)

#relative risk > 1.5 is high, 1.5-0.5 is medium, < 0.5 is low 
#number of cases > 32 is high, 32-4 is medium, < 4 is low (based on percentiles)
clinical <- clinical %>% dplyr::mutate(rr_risk_cat=ifelse(rr<0.5,"Low",ifelse(rr>=0.5&rr<1.5,"Moderate","High")),
                                       case_risk_cat=ifelse(chol_est_case<4,"Low",ifelse(chol_est_case>=4&chol_est_case<32,"Moderate","High"))) %>% 
  dplyr::mutate(rr_risk_cat=factor(rr_risk_cat, levels = c("High","Moderate","Low")),
                case_risk_cat=factor(case_risk_cat, levels = c("High","Moderate","Low")))


#load buffers
buff <- st_read("bgd_cholera_greyspots/generated_data/buff_sf_multipoly_30km.shp")

#intersect shapefile of buffers with raster
pi <- st_intersection(clinical, buff)

#map 
ggplot() + 
  geom_sf(data=bang.map0,fill=NA,lwd=0.1, alpha=.1) + 
  geom_sf(data = pi,aes(fill=case_risk_cat),color=NA,lwd=0) +
  geom_sf(data=hosp_loc,shape=3,color="black",size=1,lty=4) +
  scale_colour_manual(values = c("#e41a1c","#377eb8","#4daf4a","#984ea3")) +
  coord_sf(datum=NA) + 
  labs(x="") + labs(y="") + 
  theme_void() + 
  ggsn::north(bang.map) +
  scalebar(bang.map, dist = 50, dist_unit = "km",transform = TRUE, model = "WGS84", st.size = 3)



###################################### for later
##create rasters
r <- raster(clinical, res = .01)
r <- fasterize(clinical, r, field = "chol_est_case", fun="sum")
cases <- velox(r)
rf <- writeRaster(r, filename="data/BGD_chol_cases.tif", format="GTiff", overwrite=TRUE)

#load raster data
awd <- raster("data/BGD_awd_incidence.tif")

##create mapping grid
my_grid_dim <- 100
my_grid <- st_make_grid(bang.map0, n = c(my_grid_dim, my_grid_dim)) %>%
  st_sf(grid_id = 1:length(.))
grid_cents_btm <- my_grid %>%
  transform_to_btm() %>%
  group_by(grid_id) %>%
  st_centroid() %>%
  st_coordinates %>%
  data.frame %>% mutate(n = 100)

#get population in each grid cell
pop_grid <- bgd_pop_v$extract(sp = my_grid, fun = function(x) sum(x, na.rm = TRUE))
my_grid <- my_grid %>% dplyr::mutate(pop=c(pop_grid))

#####################################

