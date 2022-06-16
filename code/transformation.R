transformation <- function() {
  # estimate parameters of cdf matching 
  l_results <- list()
  l_fit <- list()
  df <- read_csv("data/measurements_with_mnrs.csv")
  
  for(id in unique(df$ID)) {

    # select and prepare data per sensor
    data_id <- df %>%
      dplyr::filter(ID == id) %>%
      dplyr::filter(n >= n_threshold) %>%
      dplyr::select(Date, value, mean = mean_arithmetic) %>%
      arrange(value) %>%
      mutate(mean_ranked = sort(mean)) %>%
      mutate(diff = value - mean_ranked)
    
    #fit dpseg model
    fit <- dpseg(x = data_id$value, y = data_id$diff, jumps=0, P=0.004, minl = 15)
    
    #get slope + intercept per segment
    results <- tibble(fit$segments) %>%
      dplyr::select(x1,x2,intercept,slope) 
    
    results <- cbind(value = df %>%
                       dplyr::filter(ID == id) %>% pull(value), 
                     results[cut(df %>%
                                   dplyr::filter(ID == id) %>% pull(value), 
                                 c(-Inf, results$x2), 
                                 labels = FALSE), -(1:2)]
    )
    
    results <- results %>%
      mutate(diff_pred = value * slope + intercept)
    
    results_full_id <- df %>%
      dplyr::filter(ID == id) %>%
      arrange(value) %>%
      left_join(.,results, by = "value") %>%
      mutate(diff_pred = value * slope + intercept) %>%
      mutate(SWC_cdf = value - diff_pred)
    
    # save
    l_results[[id]] <- results_full_id
    l_fit[[id]] <- fit
  }
  
  df_results <- l_results %>% 
    reduce(full_join) %>%
    distinct()
  saveRDS(l_fit, "data/list_of_plr_functions.RData")
  saveRDS(l_results, "data/list_of_results.RData")
  write_csv(df_results, "data/measurements_transformed.csv")
}