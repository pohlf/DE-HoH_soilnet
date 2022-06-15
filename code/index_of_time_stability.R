index_of_time_stability <- function() {
  df <- read_csv("data/measurements_with_mnrs.csv") %>%
    dplyr::filter(n >= n_threshold) %>%
    #group_by(Date, Layer) %>%
    mutate(RD = (value - mean_arithmetic) / mean_arithmetic) %>%
    #ungroup() %>%
    group_by(Layer, ID) %>%
    summarise(MRD = mean(RD, na.rm = T),
              SDRD = sd(RD, na.rm = T),
              n = sum(!is.na(RD))) %>%
    ungroup() %>%
    mutate(ITS = sqrt(MRD^2 + SDRD^2))
  write_csv(df, "data/its.csv")
}