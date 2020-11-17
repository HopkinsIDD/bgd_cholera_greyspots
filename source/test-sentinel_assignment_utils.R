library(tidyverse)
source("source/sentinel_cleaning_utils.R")
source("source/sentinel_assignment_utils.R")

# sentinels <- data.frame(id = 1:10, name = letters[1:10], org_lvl = c(rep("primary", 6), rep("secondary", 2), rep("tertiary", 2)))
# allhosp <- data.frame(id = 1:100, name = paste0("a", 1:100), org_lvl = c(rep("primary", 40), rep("secondary", 30), rep("tertiary", 30)), weights = runif(100))

sentinels <- read_csv("data/hosp_GPS_wicddrb.csv") %>%
  clean_sentinels_data()
allhosp <- read_csv("data/healthcare_facilities/geocoded_potential_sentinels.csv") %>%
  clean_allhosp_data()


## test for generate_random
rdm <- generate_random(sentinels, allhosp)

## test for generate_division
div <- generate_division(sentinels, allhosp)



# ## test for generate_weighted
# new1 <- generate_weighted(sentinels, allhosp, "weights")

# ## check
# newS1 <- new1 %>% group_by(type) %>% count
# oldS1 <- sentinels %>% group_by(type) %>% count
# print(paste("generate_weighted passes test:", newS1 == oldS1))