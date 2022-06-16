run_transformation_test_training <- function() {
  ### ------------------------------------------------------------------------
  ### DATA PREP
  ### ------------------------------------------------------------------------
  
  # read data
  df_results_per_sensor <- read_csv("data/measurements_with_mnrs.csv")
  
  # filter data
  true_results_per_sensor <- df_results_per_sensor %>%
    dplyr::filter(n >= n_threshold)
  
  # split data into test + training 
  true_results_per_sensor <- true_results_per_sensor %>%
    arrange(ID, Date) %>%
    mutate(test = rep(seq(0,1), length.out = n()))
  
  training_data <- true_results_per_sensor %>% dplyr::filter(test == 0) 
  test_data <- true_results_per_sensor %>% dplyr::filter(test == 1) 
  
  ### ------------------------------------------------------------------------
  ### ESTIMATE UPSCALING PARAMETERS WITH TRAINING DATA
  ### ------------------------------------------------------------------------
  
  #LINREG
  training_results_linreg <- 
    training_data %>% 
    group_by(ID) %>% 
    do(tidy(lm(mean_arithmetic ~ value, data= .)))
  
  # RATIO + ABS DIFF
  training_results_ad_ratio <- 
    training_data %>%
    mutate(beta = value / mean_arithmetic,
           ad = value - mean_arithmetic) %>%
    group_by(Layer, ID) %>%
    mutate(beta_median = median(beta, na.rm = T),
           ad_median = median(ad, na.rm = T))
  
  # merge
  training_results <- 
    training_results_linreg %>%
    dplyr::select(ID, term, estimate) %>%
    pivot_wider(names_from = term, values_from = estimate) %>%
    rename(a = `(Intercept)`,
           b = value) %>%
    left_join(training_results_ad_ratio %>% dplyr::select(ID, ad_median, beta_median)) %>%
    distinct()
  
  # CDF MATCHING
  test_data <- transformation_test_trainig(training_data, test_data)
  
  ### ------------------------------------------------------------------------
  ### TEST ESTIMATES
  ### ------------------------------------------------------------------------
  
  test_results <- test_data %>%
    left_join(.,training_results) %>%
    group_by(Layer, ID) %>%
    mutate(SWC_absdif = value - ad_median,
           SWC_ratio = value / beta_median, 
           SWC_linreg = b * value + a) %>%
    ungroup()
  
  aggregated_results <- test_results %>% 
    dplyr::select(Layer, mean_arithmetic, SWC_absdif, SWC_ratio, SWC_linreg, SWC_cdf) %>%
    pivot_longer(-c(Layer, mean_arithmetic)) %>%
    mutate(name = str_remove(name, "SWC_"))
  write_csv(aggregated_results, "data/test_appendix_results.csv")
  
  ### ------------------------------------------------------------------------
  ### GOODNESS OF FIT 
  ### ------------------------------------------------------------------------
  
  gof <- aggregated_results %>%
    #rename to ensure correct order later
    mutate(name = ifelse(name == "cdf", "zcdf", name),
           name = ifelse(name == "linreg", "xlingreg", name)) %>%
    drop_na() %>%
    group_by(name, Layer) %>%
    summarise(rmse = sqrt( mean( (value - mean_arithmetic) ^2)),
              mae = mean( abs(mean_arithmetic - value) ),
              r = cor(value, mean_arithmetic),
              ns = 1 - (sum((value - mean_arithmetic) ^2) /  sum((value - mean(value)) ^2))) 
  write_csv(gof, "data/test_appendix_goodness_of_fit.csv")
  
  # adjust results for table
  gof_print <- gof %>%
    rename(Method = name) %>%
    pivot_longer(-c(Method, Layer)) %>%
    pivot_wider(names_from = c(name, Method), values_from = value)
  new_order <- c("Layer", sort(colnames(gof_print)[2:length(colnames(gof_print))]))
  
  gof_print <- gof_print %>%
    relocate(all_of(new_order)) %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    mutate_if(is.numeric, format, nsmall = 2)
  colnames(gof_print) <- gsub(".*_","",colnames(gof_print))
  write.csv(gof_print, "data/test_appendix_goodness_of_fit_wide.csv", quote = FALSE, row.names = FALSE)
}
