plot_gof_vs_temporal_stability <- function(df, my_blue, my_grey) {
  require(tidyverse)
  df_gof <- df %>%
    #dplyr::filter(Layer %in% c("10cm", "50cm")) %>%
    dplyr::filter(n >= n_threshold) %>%
    dplyr::select(Date, ID, value, mean = mean_arithmetic) %>%
    group_by(ID) %>%
    summarise(R2 = caret::R2(value, mean),
              RMSE = caret::RMSE(value, mean)) 
  
  df <- read_csv("data/its.csv")
  
  df %>%
    left_join(., df_gof) %>%
    dplyr::filter(n > 365) %>%
    mutate(MRD = abs(MRD)) %>%
    dplyr::filter(Layer %in% c("10cm", "50cm")) %>%
    pivot_longer(-c(Layer, ID, R2, RMSE,n), names_to = "STAB") %>%
    pivot_longer(-c(Layer,ID,STAB,value,n), values_to = "gof") %>%
    ggplot(aes(value, gof, colour = n)) +
    geom_point() +
    facet_grid(vars(name),vars(STAB),  scales = "free", switch = "y") +
    ggplot_theme(font_size = 12) +
    #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr"), colour = "black") +
    #geom_smooth(method = "lm", colour = "black") +
    #geom_smooth(colour = "black") +
    ylab(NULL) + xlab(NULL) +
    scale_color_distiller("data availibity", 
                          type = "seq", 
                          direction = 1, 
                          limits = c(0,3000), 
                          guide = guide_colourbar(barwidth = 0.7, barheight = 7.5)) +
    theme(legend.title = element_text(angle = 90, vjust= 1),
          legend.justification = "bottom")+
    scale_x_continuous(expand = c(0,0.01), limits = c(0.014, NA)) +
    scale_y_continuous(expand = c(0,0.04)) +
    expand_limits(y = c(0,1), x = 0)
}