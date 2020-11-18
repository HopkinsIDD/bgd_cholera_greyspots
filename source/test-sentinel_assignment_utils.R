library(tidyverse)
source("source/sentinel_cleaning_utils.R")
source("source/sentinel_assignment_utils.R")

# sentinels <- data.frame(id = 1:10, name = letters[1:10], org_lvl = c(rep("primary", 6), rep("secondary", 2), rep("tertiary", 2)))
# allhosp <- data.frame(id = 1:100, name = paste0("a", 1:100), org_lvl = c(rep("primary", 40), rep("secondary", 30), rep("tertiary", 30)), weights = runif(100))

sentinels <- read_csv("data/hosp_GPS_wicddrb.csv") %>%
  clean_sentinels_data()
allhosp <- read_csv("data/healthcare_facilities/geocoded_potential_sentinels.csv") %>%
  clean_allhosp_data()
radii_vec <- c(10, 20, 30)


## test for generate_random
rdm <- generate_random(sentinels, allhosp)

## test for generate_division
div <- generate_division(sentinels, allhosp)


shp <- load_shapefile(admin = 0)
allhosp_buff <- get_facilities_buffers(allhosp, radii_vec)
small <- allhosp_buff %>% group_by(division) %>% slice_sample(n=1) %>% ungroup

plt <- ggplot() + geom_sf(data=shp, fill=NA, lwd=0.1, alpha=.1) + geom_sf(data=allhosp_buff%>% slice_sample(n=5), color="red", alpha=.4, show.legend="point") +coord_sf(datum=NA) + theme_void()
plt


pop <- load_pop_raster()
sero <- load_sero_grid()


test3 <- terra::extract(pop, small, fun=summary_fun, na.rm = TRUE)
test <- get_facilities_weights(pop, small, "pop", sum)
test2 <- get_facilities_weights(sero, small, "pop", sum)
test %>% arrange(org_name)
test2 %>% arrange(org_name)
?st_i# ## test for generate_weighted
# new1 <- generate_weighted(sentinels, allhosp, "weights")

# ## check
# newS1 <- new1 %>% group_by(type) %>% count
# oldS1 <- sentinels %>% group_by(type) %>% count
# print(paste("generate_weighted passes test:", newS1 == oldS1))