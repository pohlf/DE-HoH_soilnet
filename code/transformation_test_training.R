transformation_test_trainig <- function(training_data, test_data){

  l <- list()
  for(id in unique(training_data$ID)) {
    # correct group size
    test_id <- test_data %>%
      dplyr::filter(ID == id)
    
    training_id <- training_data %>%
      dplyr::filter(ID == id)
    
    if(nrow(test_id) > nrow(training_id)) {
      test_id <- test_id %>% slice(-n())
    } else if (nrow(test_id) < nrow(training_id)) {
      training_id <- training_id %>% slice(-n())
    }
    
    #fit
    mean_training_ranked <- training_id %>%
      arrange(mean_arithmetic) %>%
      pull(mean_arithmetic)
    
    training_id_arranged <- training_id %>% arrange(value) 
    training_id_arranged$diff <- training_id_arranged$value - mean_training_ranked
    
    #fit <- lm(training_id_arranged$diff ~ stats::poly(training_id_arranged$value,3))
    #summary(fit)
    
    fit <- dpseg::dpseg(x=training_id_arranged$value, 
                        y=training_id_arranged$diff, 
                        jumps=0, P=0.004, minl = 15)
    
    # predict
    mean_test_ranked <- test_id %>%
      arrange(mean_arithmetic) %>%
      pull(mean_arithmetic)
    
    test_id_arranged <- test_id %>% arrange(value) 
    
    diff_pred <- predict(fit, newdata = test_id_arranged)
    test_id_arranged$SWC_cdf = test_id_arranged$value - diff_pred$y
    
    # save
    l[[id]] <- test_id_arranged
  }
  
  test_results <- l %>% reduce(full_join)
  return(test_results)
}