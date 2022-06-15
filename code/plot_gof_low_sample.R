plot_gof_low_sample <- function(blue_palette, grey_palette) {
  
  df <- read_csv("data/gof_low_sample_size_test.csv")
  
  plot_df <- df %>%
    dplyr::filter(n > 100) %>%
    rename(Data = name,
           RMSE = rmse
           ) %>%
    pivot_longer(-c(Layer, Data, n, ID, k)) 
  
  plot_df %>%
    dplyr::filter(Layer %in% c("10cm","50cm")) %>%
    ggplot(aes(x = value)) +
    stat_ecdf(data = .%>%dplyr::filter(Data == "Mean"), aes(colour = as.factor(k)), geom = "step") +
    scale_color_manual("measured",values = grey_palette) +
    ggnewscale::new_scale_color() +
    stat_ecdf(data = .%>%dplyr::filter(Data == "Mean_CDF"), aes(colour = as.factor(k)),geom = "step") +
    ggplot_theme() +
    scale_color_manual("transformed",values = blue_palette) +
    facet_grid(vars(Layer), vars(name),scales = "free_x") +
    scale_x_continuous(expand = c(0,0), breaks = scales::pretty_breaks()) +
    xlab(NULL) + ylab("cumulative probability")
  }

