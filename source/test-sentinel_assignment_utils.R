library(tidyverse)

sentinels <- data.frame(id = 1:10, name = letters[1:10], type = c(rep("primary", 6), rep("secondary", 2), rep("tertiary", 2)))
allhosp <- data.frame(id = 1:100, name = paste0("a", 1:100), type = c(rep("primary", 40), rep("secondary", 30), rep("tertiary", 30)), weights = runif(100))


## test for generate_weighted
new1 <- generate_weighted(sentinels, allhosp, "weights")

## check
newS1 <- new1 %>% group_by(type) %>% count
oldS1 <- sentinels %>% group_by(type) %>% count
print(paste("generate_weighted passes test:", newS1 == oldS1)))


## test for generate_random
new2 <- generate_random(sentinels, allhosp)

## check
newS2 <- new2 %>% group_by(type) %>% count
oldS2 <- sentinels %>% group_by(type) %>% count
print(paste("generate_random passes test:", all(newS2 == oldS2)))
