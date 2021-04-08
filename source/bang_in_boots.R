library(tidyverse)

source("source/sentinel_cleaning_utils.R")

#######################
boots_fn <- "data/all-boots.rds"
boots_og <- readRDS(boots_fn) ## 1000 reps, 6163 grid ids
sero_og <- load_sero_grid() ## has duplicate grid_ids if geometry is split due to islands

## note that grid_id 94, 1097, 5504, 6118 are missing from sero
unique(boots_og$grid_id)[which(!unique(boots_og$grid_id) %in% unique(sero_og$grid_id))]
boots <- dplyr::filter(boots_og, !(grid_id %in% c(94, 1097, 5504, 6118)))

pop_grid <- dplyr::filter(sero_og, !is.na(pop_toAgg)) %>%
  dplyr::select(grid_id, pop_toAgg) %>%
  dplyr::rename(pop = pop_toAgg) %>%
  sf::st_drop_geometry()

#######################
quibble <- function(x, q = c(0.25, 0.5, 0.75)) {
  tibble("{{ x }}" := c(quantile(x, q, na.rm = TRUE)), "{{ x }}_q" := c(q))
}
quant_vec <- c(0.25, 0.75)

#######################
boot_q <- dplyr::group_by(boots, rep) %>%
  dplyr::summarise(quibble(sample_total, quant_vec),
                  quibble(sample_rr, quant_vec)) %>%
  dplyr::rename(q = sample_total_q) %>%
  dplyr::select(rep, q, sample_total, sample_rr) 

boot_q_wide <- tidyr::pivot_wider(boot_q, names_from = q, values_from = c(sample_total, sample_rr))

boot_cat <- dplyr::select(boots, rep, grid_id, sample_total, sample_rr) %>%
  dplyr::left_join(boot_q_wide, by = c("rep")) %>%
  dplyr::mutate(
    cat_rr = ifelse(sample_rr < sample_rr_0.25, "Mild",
   ifelse(sample_rr >= sample_rr_0.25 & sample_rr < sample_rr_0.75, "Moderate", "High")),
    cat_inf = ifelse(sample_total < sample_total_0.25, "Mild", ifelse(sample_total >= sample_total_0.25 & sample_total < sample_total_0.75, "Moderate", "High"))) %>%
  dplyr::mutate(cat_rr = factor(cat_rr, levels = c("High", "Moderate", "Mild")),
                cat_inf = factor(cat_inf, levels = c("High", "Moderate", "Mild"))) %>%
  dplyr::left_join(pop_grid, by = c("grid_id"))
  # dplyr::left_join(dplyr::select(sero_og, grid_id, pop_toAgg), by = c("grid_id")) %>% dplyr::rename(pop = pop_toAgg)

bootcat_inf <- boot_cat %>%
  dplyr::rename(category = cat_inf) %>%
  dplyr::group_by(rep, category) %>%
  dplyr::summarise(inf_cat_total = sum(sample_total, na.rm = TRUE),
                  pop_cat_total = sum(pop, na.rm = TRUE)) %>%
  dplyr::mutate(metric = "inf")
bootcat_rr <- boot_cat %>%
  dplyr::rename(category = cat_rr) %>%
  dplyr::group_by(rep, category) %>%
  dplyr::summarise(inf_cat_total = sum(sample_total, na.rm = TRUE),
                  pop_cat_total = sum(pop, na.rm = TRUE)) %>%
  dplyr::mutate(metric = "rr")

bootcat <- dplyr::bind_rows(bootcat_inf, bootcat_rr) %>%
  dplyr::select(rep, metric, category, inf_cat_total, pop_cat_total) 

#### mini check with Lancet microbe paper ####
test <- bootcat %>% group_by(rep, metric) %>% summarise(bgd = sum(inf_cat_total), pop=sum(pop_cat_total)) %>% mutate(perc=bgd/pop*100)
quantile(test$perc, probs=c(.025, .5, .975)) 
mean(test$perc) ## matches 17·3% (95% CI 10·5–24·1)
#############################################

readr::write_csv(bootcat, "data/boot_cat_denominators.csv")
