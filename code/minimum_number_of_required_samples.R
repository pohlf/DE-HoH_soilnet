minimum_number_of_required_samples <- function() {
  require(tidyverse)
  
  summary_coef_var <- read_csv("data/coef_of_var.csv") %>%
    group_by(removal, Layer) %>%
    summarise(min = min(coef_var),
              max = max(coef_var),
              mean = mean(coef_var),
              n = max(n)) %>%
    ungroup() 
  
  # estimate threshold
  mnrs <- 
    summary_coef_var %>%
    arrange(Layer) %>%
    mutate(removal_n = removal*n) %>%
    select(Layer, removal_n, mean) %>%
    mutate(mean = mean*100) %>%
    mutate(mean_diff = mean - lag(mean)) %>%
    dplyr::filter(mean_diff > (-1)) %>%
    group_by(Layer) %>%
    slice_min(mean_diff) %>%
    mutate(removal_n = round(removal_n))
  
  summary_coef_var <- summary_coef_var %>%
    left_join(., mnrs %>% select(Layer, 
                                 n_threshold = removal_n), by = c("Layer"))
  
  write_csv(summary_coef_var, "data/mnrs.csv")
}
