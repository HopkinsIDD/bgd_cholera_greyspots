######code to process infection data

source("source/util.R")
reload_source()

##load sero data
#variable rr2_cor = population weighted relative risk
#mean2_cor*pop = number of infections
sero <- readRDS("Elonia/grey_out_BGD/data/village_preds_all.rds") %>%
  dplyr::mutate(inf=mean2_cor*pop) 

##load buffers
buff.10 <- st_read("bgd_cholera_greyspots/generated_data/buff_sf_multipoly_10km.shp")
buff.30 <- st_read("bgd_cholera_greyspots/generated_data/buff_sf_multipoly_30km.shp")
buff.50 <- st_read("bgd_cholera_greyspots/generated_data/buff_sf_multipoly_50km.shp")

##transform sero data to match buffer data crs
sero <- st_transform(sero, crs = crs(buff.10))
##intersect sero data with bangladesh map
sero <- st_intersection(sero,bang.map0)
saveRDS(sero.bgd,"Elonia/grey_out_BGD/data/village_preds_all_bgdintersect.rds")

##create risk categories
#look at distribution of number of infections and population weighted RR in country and categorize 
summary(sero$inf)
summary(sero$rr2_cor)
#RR > 1.5 is high, 1.5-0.5 is medium, < 0.5 is low 
#Number of infections > 5000 is high, 5000-2000 is medium, < 2000 is low (based on percentiles)
sero <- sero %>% dplyr::mutate(rr_risk_cat=ifelse(rr2_cor<0.5,"Low",ifelse(rr2_cor>=0.5&rr2_cor<1.5,"Moderate","High")),
                               inf_risk_cat=ifelse(inf<2000,"Low",ifelse(inf>=2000&inf<5000,"Moderate","High"))) %>% 
  dplyr::mutate(rr_risk_cat=factor(rr_risk_cat, levels = c("High","Moderate","Low")),
                inf_risk_cat=factor(inf_risk_cat, levels = c("High","Moderate","Low")))

##intersect shapefile of buffers with raster
int.10 <- st_intersection(sero, buff.10) %>% dplyr::mutate(buff="10 km")
int.30 <- st_intersection(sero, buff.30) %>% dplyr::mutate(buff="30 km")
int.50 <- st_intersection(sero, buff.50) %>% dplyr::mutate(buff="50 km")
#combining intersections of different buffers and sero data into 1 file
catch <- rbind(int.10,int.30,int.50)

##map 
gg = ggplot() + 
  geom_sf(data=bang.map0,fill=NA,lwd=0.1, alpha=.1) + 
  geom_sf(data =int.30,aes(fill=rr_risk_cat),color=NA,lwd=0) +
  geom_sf(data=hosp_loc,shape=3,color="black",size=1,lty=4) +
  scale_colour_manual(values = c("#e41a1c","#377eb8","#4daf4a","#984ea3")) +
  coord_sf(datum=NA) + 
  labs(x="") + labs(y="") + 
  theme_void() + 
  ggsn::north(bang.map) +
  scalebar(bang.map, dist = 50, dist_unit = "km",transform = TRUE, model = "WGS84", st.size = 3)
ggsave("bgd_cholera_greyspots/figures/test_infectriskcatmap_30km.pdf") 

##############################estimates & tables
##number and percent of total infections in catchment
#total number of infections in BGD
tot_inf <- sum(sero$inf)
#calculate number and percent of infections
est1 <- catch %>% 
  group_by(buff) %>%
  dplyr::summarise(no_inf=sum(inf),pop=sum(pop)) %>%
  ungroup %>%
  dplyr::mutate(perc_inf=(no_inf/tot_inf)) %>% 
  dplyr::mutate(perc_inf=percent(perc_inf),no_inf=comma(no_inf),pop=comma(pop)) %>%
  dplyr::rename("Buffer size"=buff,"Percent infected"=perc_inf,"Population size"=pop, "Number infected"=no_inf) %>% 
  data.frame() %>%
  as_tibble() %>% dplyr::select(-c(.))
#the table
formattable(est1)
formattable(est1, align =c("l","c","c","c"), list(
  Buffer.size = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  Population.size = color_tile("#71CA97", "#DeF7E9"),
  Number.infected = color_tile("#71CA97", "#DeF7E9"),
  Percent.infected = color_tile("#71CA97", "#DeF7E9")
))
  
##number and percent of people in high risk infection areas (by rr and number of infections), medium, low that are in sentinel polygons 
#by RR
tot_inf_riskcat <- catch %>% 
  group_by(rr_risk_cat) %>%
  dplyr::summarise(tot_inf=sum(inf)) %>%
  ungroup %>% data.frame() %>%
  as_tibble(tot_inf_riskcat) %>% dplyr::select(-c(.))
est2 <- catch %>% 
  group_by(buff,rr_risk_cat) %>%
  dplyr::summarise(no_inf=sum(inf)) %>%
  ungroup %>% left_join(.,tot_inf_riskcat) %>%
  dplyr::mutate(perc_inf=(no_inf/tot_inf)) %>% 
  dplyr::mutate(perc_inf=percent(perc_inf),no_inf=comma(no_inf),tot_inf=comma(tot_inf)) %>%
  dplyr::rename("Buffer size"=buff,"Percent infected"=perc_inf,"Number infected"=no_inf, "Risk category"=rr_risk_cat, "Total infected"=tot_inf) %>% 
  data.frame() %>%
  as_tibble() %>% dplyr::select(-c(.))
formattable(est2, align =c("l","c","c","c","r"), list(
  Buffer.size = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  Risk.category = color_tile("#71CA97", "#DeF7E9")
))
#by number of infections
tot_inf_riskcat2 <- catch %>% 
  group_by(inf_risk_cat) %>%
  dplyr::summarise(tot_inf=sum(inf)) %>%
  ungroup %>% data.frame() %>%
  as_tibble() %>% dplyr::select(-c(.))
est3 <- catch %>% 
  group_by(buff,inf_risk_cat) %>%
  dplyr::summarise(no_inf=sum(inf)) %>%
  ungroup %>% left_join(.,tot_inf_riskcat2) %>%
  dplyr::mutate(perc_inf=(no_inf/tot_inf)) %>% 
  dplyr::mutate(perc_inf=percent(perc_inf),no_inf=comma(no_inf),tot_inf=comma(tot_inf)) %>%
  dplyr::rename("Buffer size"=buff,"Percent infected"=perc_inf,"Number infected"=no_inf, "Risk category"=inf_risk_cat, "Total infected"=tot_inf) %>% 
  data.frame() %>%
  as_tibble() %>% dplyr::select(-c(.))
formattable(est3, align =c("l","c","c","c","r"), list(
  Buffer.size = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  Risk.category = color_tile("#9945A6", "#DBB3E1")
))

##percent of population captured in sentinel polygons that are in high risk areas (rr)
tot_pop <- sum(sero$pop)
#restrict data to just high risk areas
sero.rr.high <- sero %>% dplyr::filter (rr_risk_cat=="High")
#re-intersect shapefile of buffers with sf files 
int.10.hi <- st_intersection(sero.rr.high, buff.10) %>% dplyr::mutate(buff="10 km")
int.30.hi <- st_intersection(sero.rr.high, buff.30) %>% dplyr::mutate(buff="30 km")
int.50.hi <- st_intersection(sero.rr.high, buff.50) %>% dplyr::mutate(buff="50 km")
#combining intersections of different buffers and sero data into 1 file
catch.hi <- rbind(int.10.hi,int.30.hi,int.50.hi)
est4 <- catch.hi %>% 
  group_by(buff,rr_risk_cat) %>%
  dplyr::summarise(no_inf=sum(inf),pop=sum(pop)) %>%
  ungroup %>% 
  dplyr::mutate(perc_pop=(pop/tot_pop)) %>% 
  dplyr::mutate(perc_pop=percent(perc_pop),no_inf=comma(no_inf),pop=comma(pop)) %>%
  dplyr::rename("Buffer size"=buff,"Percent of population"=perc_pop,"Number infected"=no_inf, "Risk category"=rr_risk_cat, "Population"=pop) %>% 
  data.frame() %>%
  as_tibble() %>% dplyr::select(-c(.))
formattable(est4, align =c("l","c","c","c","r"), list(
  Buffer.size = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  Risk.category = color_tile("#71CA97", "#DeF7E9")
))
  
  
  
  












###################################### for later
###################################### clinical data
#load data
clinical <- readRDS("data/gam_cholera_bgd_est.rds") #clinical incidence data

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




