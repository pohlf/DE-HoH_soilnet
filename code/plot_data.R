plot_data <- function(blue) {
  require(ggpubr)
  require(cowplot)
  require(tidyverse)
  require(ggridges)
  require(tsibble)
  
  df <- read_csv("data/measurements_with_mnrs.csv") 
  
  # 1. MISSING VALUE PLOT 
  df_filled <- df %>%
    dplyr::select(Date, ID, Layer, value,n) %>%
    as_tsibble(key = c(ID,Layer), index = Date) %>% 
    fill_gaps(.full = TRUE) 
  
  every_nth = function(n) {
    return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
  }
  
  df_p <- df_filled %>%
    tibble() %>%
    drop_na() %>%
    group_by(ID) %>%
    mutate(n = 1200) %>%
    ungroup() %>%
    mutate(ID =as.numeric(gsub(".*_","",ID))) %>%
    dplyr::filter(Layer %in% c("50cm","10cm")) %>%
    group_by(Layer) %>%
    arrange(ID) %>%
    mutate(ID = as.factor(ID)) %>%
    ungroup() %>%
    mutate(isna = factor(if_else(is.na(value), 1, 0)))
  
  list_label <- c()
  seq_uneven <- seq(1,35,3)
  for(i in seq_uneven) {
    list_label <- append(list_label, c(i, "" , ""))
  }
  
  a <- df_p %>% 
    mutate(ID = as.numeric(ID)) %>%
    ggplot(aes(x = Date, y = ID, alpha = isna)) +
    geom_raster(aes(fill = "n")) +
    scale_alpha_manual(values = c(1,0)) + 
    ggplot_theme(center_axis_labels = T) +
    facet_grid(vars(Layer), scales = "free", space = "free") +
    scale_x_date(expand = c(0,0), breaks = scales::pretty_breaks(n = 7), position = "top") +
    scale_y_continuous(breaks = seq(1,length(list_label),1), labels = list_label ,expand = c(0,0)) +
    labs(x = NULL, y= "Sensor number") +
    theme(axis.text.y = element_text(size = 7.5),
          panel.grid.major.y = element_line(size = rel(0.5)),
          #panel.grid.minor.y = element_line(size = rel(0.5)),
          legend.position = "none") +
    scale_fill_manual(values = blue)
  
  # 2. COEF VAR 
  
  c <- read_csv("data/mnrs.csv") %>%
    dplyr::filter(Layer %in% c("10cm","50cm")) %>%
    ggplot(aes(removal*n)) +
    #geom_vline(aes(xintercept = threshold_removal), linetype = "dashed") +
    geom_ribbon(aes(ymin = min, ymax = max), fill = "gray60") +
    geom_line(aes(y = mean)) +
    geom_vline(aes(xintercept = n_threshold), linetype = "dashed", size = 0.3) +
    facet_wrap(~Layer, ncol = 1) +
    ggplot_theme(center_axis_labels = T)+
    ylab("CV") + xlab("active sensors") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3), 
                       labels = scales::percent_format(accuracy = 1),
                       limits = c(0,0.3),
                       expand = c(0,0)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3), 
                       expand = c(0,0), limits = c(0,31)) +
    theme(panel.grid.minor = element_line(size = rel(0.5)),
          panel.grid.major = element_line(size = rel(0.5)))
  
  # 3.???? MRD
  
  df_its <- read_csv("data/its.csv") %>%
    mutate(ID =gsub(".*_","",ID))
  
  reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
    new_x <- paste(x, within, sep = sep)
    stats::reorder(new_x, by, FUN = fun)
  }
  
  scale_x_reordered <- function(..., sep = "___") {
    reg <- paste0(sep, ".+$")
    ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
  }
  
  b <- df_its %>%
    dplyr::filter(Layer %in% c("10cm", "50cm")) %>%
    ggplot(aes(reorder_within(x = ID,
                              by = MRD,
                              within = Layer,
                              fun = median), 
               MRD)) +
    geom_pointrange(aes(ymin = MRD-SDRD, ymax = MRD+SDRD, colour = n), size = 0.6, fatten = 1.25) +
    geom_line(aes(y = ITS, group = Layer, linetype = "dashed")) +
    facet_grid(~Layer, scale = "free_x", space = "free_x") +
    xlab("Sensor number") + ylab("rel. difference") +
    scale_color_distiller("data availability [days]", 
                          type = "seq", 
                          direction = 1, 
                          limits = c(0,3000), 
                          guide = guide_colourbar(barwidth = 0.7, barheight = 7.5, order = 2))  +
    scale_x_reordered() +
    scale_y_continuous(
      limits = c(-0.65, 0.65), 
      expand = c(0,0)) +
    scale_linetype_manual(NULL, values = "dashed", label = "ITS") +
    ggplot_theme(center_axis_labels = T) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7.5),
          legend.title = element_text(angle = 90, vjust= 1),
          legend.justification = "bottom",
          panel.grid.major.x = element_blank()) 
  
  # first align the top-row plot (p3) with the left-most plot of the
  # bottom row (p1)
  plots <- align_plots(a + theme(legend.position = "none"), c, align = 'v', axis = 'l')
  top_row <- plot_grid(plots[[1]], labels = "A", label_size = 18)
  bottom_row <- plot_grid(plots[[2]], b + theme(legend.position = "none"), labels = c('B', 'C'), rel_widths = c(0.25,0.75), label_size = 18)
  aa <- plot_grid(top_row, bottom_row, ncol = 1, rel_heights = c(0.5,0.5))
  
  #aa <- plot_grid(top_row, bottom_row, ncol = 1, rel_heights = c(0.5,0.5))
  plot_grid(aa, get_legend(b), rel_widths = c(0.95,0.075), nrow = 1)
  }



