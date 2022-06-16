plot_ts_results <- function() {
  df <- read_csv("data/measurements_transformed.csv")
  
  ID_list <- df %>%
    dplyr::filter(Layer %in% c("10cm", "50cm")) %>%
    group_by(ID, Layer) %>%
    summarise(n = n()) %>%
    group_by(Layer) %>%
    arrange(-n) %>%
    top_n(3) %>%
    ungroup() %>%
    pull(ID)
  
  df_plot <- df %>%
    mutate(mean_t = if_else(n >= n_threshold, mean_arithmetic, NA_real_),
           mean_f = if_else(n < n_threshold, mean_arithmetic, NA_real_)) %>%
    dplyr::select(Date, Layer, ID, Sensor = SWC_cdf, mean_t, mean_f) %>%
    dplyr::filter(ID %in% ID_list) %>%
    group_by(Date, Layer) %>%
    mutate(median = median(Sensor)) %>%
    ungroup() %>%
    pivot_longer(-c(Date,Layer,ID,Sensor)) %>%
    mutate(name = factor(name, levels = c("mean_f", "median", "mean_t")))
  
  a <- df_plot %>%  
    ggplot(aes(Date, colour = name)) +
    geom_point(aes(y = Sensor, shape = "Sensor"), size = 0.3, colour = my_blue) +
    ggplot_theme() +
    geom_line(aes(y = value), size = 0.6) +
    xlab(NULL) + ylab(bquote(theta~"[vol. %]")) +
    facet_wrap(~Layer, ncol = 1) +
    scale_colour_manual(NULL, 
                        labels = c(expression(bar(theta)["x"]), expression(bar(theta)["x,sp"]), expression(bar(theta)["ref"])),
                        values = c("grey60", "#ef8a62", "black")) +
    #scale_linetype_manual(NULL, 
    #                    labels = c("Ich habe ansonsten noch eine Frage zur Variablenbezeichnung an dich Uncorrected", "Median", "Reference"),
    #                    values = c("dashed", "solid", "solid")) +
    scale_shape_manual(NULL, values = 19, labels = c(expression(theta["x,sp"]))) 
  
  b <- df_plot %>%
    pivot_wider() %>%
    dplyr::select(-mean_f) %>%
    pivot_wider(names_from = ID, values_from = Sensor)  %>%
    pivot_longer(-c(Date,Layer,mean_t,median)) %>%
    rename(sensor = name) %>%
    ggplot(aes(mean_t)) +
    geom_point(aes(y = value, colour = "Sensor"), size = 0.3) +
    geom_point(aes(y = median, colour = "median"), size = 0.3) +
    facet_wrap(~Layer, ncol = 1) +
    geom_abline(slope = 1) +
    ggplot_theme() + 
    ylab(bquote(theta~"[vol. %]")) + 
    xlab(expression(bar(theta)["ref"] ~ "[vol. %]")) +
    scale_color_manual(NULL, 
                       values = c("#ef8a62", my_blue), 
                       labels = c(expression(theta["x,sp"]))) 
  
  df_plot %>%
    pivot_wider() %>%
    dplyr::select(-mean_f) %>%
    pivot_wider(names_from = ID, values_from = Sensor)  %>%
    pivot_longer(-c(Date,Layer,mean_t)) %>%
    group_by(name, Layer) %>%
    drop_na() %>%
    summarise(R2 = caret::R2(value,mean_t),
              RMSE = caret::RMSE(value,mean_t))
  
  ggarrange(a,b,ncol=2, common.legend = T, widths = c(0.7,0.3), legend = "bottom", 
            labels = "AUTO", font.label = list(size = 18)) 
  
  df_plot %>%
    pivot_wider() %>%
    dplyr::select(-mean_f) %>%
    pivot_wider(names_from = ID, values_from = Sensor)  %>%
    pivot_longer(-c(Date,Layer,mean_t)) %>%
    group_by(name, Layer) %>%
    drop_na() %>%
    summarise(R2 = caret::R2(value,mean_t),
              RMSE = caret::RMSE(value,mean_t))
}