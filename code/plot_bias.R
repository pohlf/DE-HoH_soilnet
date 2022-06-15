plot_bias <- function(df, my_blue, my_grey) {
  
  df_plot <- df %>%
    dplyr::filter(Layer %in% c("10cm", "50cm")) %>%
    mutate(mean_t = if_else(n < n_threshold, NA_real_, mean_arithmetic))
  
  
  # A
  a <- df_plot %>%
    ggplot(aes(Date)) +
    geom_point(aes(y = value), size = 0.3, colour = my_grey ) +
    geom_point(aes(y = SWC_cdf), size = 0.3, colour = my_blue) +
    geom_line(aes( y = mean_t), size = 0.5) +
    facet_wrap(~Layer) +
    ggplot_theme(center_axis_labels = T) +
    xlab(NULL) + ylab(bquote(theta~"[vol. %]"))
  
  # B
  ID_list <- df_plot %>%
    group_by(Layer, ID) %>%
    summarise(n = n()) %>%
    group_by(Layer) %>%
    arrange(-n) %>%
    top_n(3) %>%
    ungroup() %>%
    pull(ID)
  
  b <- df_plot %>%
    dplyr::filter(ID %in% ID_list) %>%
    mutate(ID =as.numeric(gsub(".*_","",ID))) %>%
    dplyr::select(Layer, ID, value, SWC_cdf) %>%
    pivot_longer(-c(Layer, ID)) %>%
    mutate(name = factor(name, levels = c("value", "SWC_cdf"), labels = c("measured", "transformed"))) %>%
    ggplot(aes(x = value, y = as.factor(ID), fill = name)) +
    geom_density_ridges(bandwidth = 1.2, scale = 1.5) +
    facet_grid(vars(Layer), vars(name), scales = "free_y") +
    ggplot_theme() +
    scale_fill_manual(NULL, values = c(my_grey, my_blue)) +
    ylab("Sensor ID") + xlab(bquote(theta~"[vol. %]")) +
    theme(legend.position = "none") +
    #scale_y_discrete(expand = c(0.1,0.1)) +
    expand_limits(y = 5)
  
  # C
  
  c <- df_plot %>%
    #dplyr::filter(ID %in% ID_list) %>%
    mutate(ID =as.numeric(gsub(".*_","",ID))) %>%
    dplyr::select(Layer, ID, value, SWC_cdf, mean_t) %>%
    #pivot_longer(-c(Layer, ID, mean_t)) %>%
    ggplot(aes(mean_t)) +
    geom_point(aes(y = value), size = 0.3, colour = my_grey ) +
    geom_point(aes(y = SWC_cdf), size = 0.3, colour = my_blue)+
    geom_abline(slope = 1) +
    scale_colour_manual(NULL, values = c(my_grey, my_blue)) +
    facet_wrap(~Layer) +
    ggplot_theme() +
    xlab(bquote(theta["ref"]~"[vol. %]")) + ylab(bquote(theta~"[vol. %]"))
  
  
  # PLOT
  plots <- align_plots(a, b, align = 'v', axis = 'l')
  top_row <- plot_grid(plots[[1]], labels = "A", label_size = 18)
  bottom_row <- plot_grid(plots[[2]],c, labels = c('B', 'C'), rel_widths = c(0.5,0.5), label_size = 18)
  plot_grid(top_row, bottom_row, ncol = 1)
  
}
