library(here)

## bring in the population raster from world pop
bgd_pop <- here("data",
                "BGD_ppp_2015_adj_v2.tif") %>%
  raster()
## load national polygon
bgd0 <- readRDS("data/BGD_adm0.rds") %>% st_as_sf() %>%
   transform_to_btm()
## making a 5 by 5 grid over bangladesh and then assiging
## ids
my_grid <- st_make_grid(bgd0,cellsize=c(5000,5000)) %>%
  st_sf(grid_id = 1:length(.))
## get grid centroids in btm
grid_cents <- my_grid %>%
  group_by(grid_id) %>%
  st_centroid() %>%
  st_coordinates %>%
  data.frame
## using velox to extract pop data quiclky with tensor-flow
bgd_pop_v <-  bgd_pop %>% 
  transform_to_btm() %>%
  velox()
## get population in each grid cell
pop_grid <- bgd_pop_v$extract(sp = my_grid,
                              fun = function(x) sum(x, na.rm = TRUE)) %>%
  as.numeric
## setting minimum to 100 people per 5km by 5km to help computation
pop_grid <- ifelse(pop_grid < 100, 100,pop_grid)
## adding population to grid cells
my_grid_pop <- my_grid %>%
  mutate(pop = pop_grid,
         logpop = log(pop_grid))
