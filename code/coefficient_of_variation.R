coefficient_of_variation <- function(df) {
  require(tidyverse)
  #set.seed(497204)
  
  # 1. RANDOMLY SELECT DAYS WITH MAXIMUM AMOUNT OF SENSORS PER LAYER
  df_selected <- df %>%
    dplyr::select(Layer, n, Date, Sensors) %>%
    group_by(Layer) %>% 
    slice_max(n, n = 20) %>% 
    sample_n(20)
  
  n_max <- df %>%
    dplyr::select(Layer, n, Date, Sensors) %>%
    group_by(Layer) %>%
    summarise(n = max(n))
  
  # 2. BOOTSTRAP COEF OF VAR 
  
  # vector with removal stages of data
  df_coef_var <- data.frame()
  
  for(j in 1:nrow(df_selected)) {
    # extract cols and date for each test
    cols <- unlist(str_split(df_selected$Sensors[j], pattern = ","))
    test_date <- df_selected$Date[j]
    
    # take n from n_max to make sure that removal stage is always same even though n can be n_max-1
    n <- n_max %>% dplyr::filter(Layer == df_selected$Layer[j]) %>% pull(n)
    
    # reduce data to selected cols and the test date 
    test_day <- df %>% 
      dplyr::filter(Date == test_date) %>%
      dplyr::filter(ID %in% cols)
    
    vector <- test_day$value
    removal <- seq(1, 1/n, by = -1/n)
    #df to store results
    coef_var <- data.frame(removal = removal)
    
    for(i in 1:length(removal)) {
      x <- replicate(1000, {
        mean(sample(vector, size = n*removal[i], replace = TRUE))
      })
      coef_var[i,"coef_var"] <- sd(x) / mean(x)
      coef_var[i,"removal"] <- removal[i]
    }
    
    coef_var$Layer <- df_selected$Layer[j]
    coef_var$Date <- df_selected$Date[j]
    coef_var$n <- df_selected$n[j]
    coef_var$Sensors <- df_selected$Sensors[j]
    
    df_coef_var <- rbind(df_coef_var, coef_var)
  }
  
  write_csv(df_coef_var, "data/coef_of_var.csv")
}
