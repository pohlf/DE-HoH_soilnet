run_low_sample_size_test <- function() {
  
  df <- read_csv("data/measurements_transformed.csv")
  
  bias_function <- function(depth, df, k){
    
    d <- depth
    ID_list <- df %>%
      dplyr::filter(Layer == d) %>%
      dplyr::filter(n >= n_threshold) %>%
      pull(ID) %>%
      unique()
    
    n_max <- df %>% 
      dplyr::filter(Layer == d) %>%
      summarise(max(n)) %>% pull()
    
    combs <- combinations(v = ID_list, k = k, replace = FALSE, nsample = 200)
    df_summary <- data.frame()
    
    pb <- txtProgressBar(min = 0, max = nrow(combs), style = 3)
    #print("Layer:", depth)
    for(i in 1:nrow(combs)) {
      #progress bar
      setTxtProgressBar(pb, i)
      
      df_ids <- df %>%
        dplyr::filter(Layer == d) %>%
        dplyr::filter(n >= n_max * 0.50) %>%
        dplyr::filter(ID %in% combs[i,1:k])
      
      df_ids_dates <- df_ids %>%
        group_by(Date) %>%
        summarize(n=n()) %>%
        dplyr::filter(n == k) %>%
        dplyr::select(-n) %>%
        left_join(., df_ids, by = "Date")
      
      GOF <- df_ids_dates %>%
        group_by(Date) %>%
        mutate(Mean_CDF = mean(SWC_cdf, na.rm = T),
               Mean = mean(value, na.rm = T)) %>%
        ungroup() %>%
        dplyr::select(Date, Layer, mean_arithmetic, Mean_CDF, Mean) %>%
        pivot_longer(-c(Date, Layer, mean_arithmetic)) %>%
        group_by(Layer, name) %>%
        summarise(rmse = caret::RMSE(value, mean_arithmetic),
                  #mae = mean( abs(mean_arithmetic - value) ),
                  R2 = caret::R2(value, mean_arithmetic),
                  #ns = 1 - (sum((value - mean_arithmetic) ^2) /  sum((value - mean(value)) ^2)),
                  n = n(), .groups = "keep") %>%
        ungroup() 
      
      summary <- GOF %>%
        mutate(ID = paste(combs[i,1:k], collapse = ","))
      df_summary <- rbind(df_summary, summary)
    }
    close(pb)
    return(df_summary)
  }
  
  depth_list <- unique(df$Layer)
  
  summary <- future_lapply(depth_list, bias_function, df, k = 1, future.seed = T)
  df_summary_1 <- reduce(summary, rbind)
  
  summary <- future_lapply(depth_list, bias_function, df, k = 3, future.seed = T)
  df_summary_3 <- reduce(summary, rbind)
  
  summary <- future_lapply(depth_list, bias_function, df, k = 2, future.seed = T)
  df_summary_2 <- reduce(summary, rbind)
  
  summary <- future_lapply(depth_list, bias_function, df,k = 4, future.seed = T)
  df_summary_4 <- reduce(summary, rbind)
  
  summary <- future_lapply(depth_list, bias_function, df, k = 5, future.seed = T)
  df_summary_5 <- reduce(summary, rbind)
  
  summary <- future_lapply(depth_list, bias_function, df, k = 6, future.seed = T)
  df_summary_6 <- reduce(summary, rbind)
  
  df_summary <- rbind(df_summary_1, df_summary_2, df_summary_3, df_summary_4, df_summary_5,df_summary_6) %>% 
    mutate(k = str_count(ID, pattern = ",") + 1)
  
  write_csv(df_summary, "data/gof_low_sample_size_test.csv")
 
}