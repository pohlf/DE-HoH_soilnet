cdf_matching_poly_vs_plr <- function() {
  
  l_fit <- readRDS("data/list_of_plr_functions.RData")
  l_results <- readRDS("data/list_of_results.RData")
  wanted <- c("10cm_30", "10cm_29", "10cm_23", "50cm_16", "50cm_15", "50cm_13")

  df <- tibble()
  df_segs <- tibble()
  
  for(i in 1:length(l_results)) {
    if(unique(l_results[[i]]$ID) %in% wanted) {
      sensor <- data.frame(predx = predict(l_fit[[i]])$x, 
                           predy = predict(l_fit[[i]])$y,
                           x = l_fit[[i]]$xy$x,
                           y = l_fit[[i]]$xy$y,
                           sensor = unique(l_results[[i]]$ID))
      segs <- cbind(l_fit[[i]]$segments, sensor = unique(l_results[[i]]$ID))
      df <- rbind(df, sensor)
      df_segs <- rbind(df_segs, segs)
    }
  }
  
  ggplot(df, mapping = aes(x,y)) +
    geom_point(mapping = aes(colour = "Obs. diff."), size = 0.3) +
    geom_line(mapping = aes(predx, predy, colour = "PLR")) +
    geom_vline(data = df_segs, mapping = aes(xintercept = x1), size = 0.2) +
    ggplot_theme() +
    stat_smooth(method="lm", se=TRUE, fill=NA,
                formula=y ~ poly(x, 5, raw=TRUE),
                mapping = aes(colour="PolyReg5"), size = 0.4) +
    stat_smooth(method="lm", se=TRUE, fill=NA,
                formula=y ~ poly(x, 3, raw=TRUE),
                mapping = aes(colour="PolyReg3"), size = 0.4) + 
    ylab(expression(Delta ~ "[%]")) +
    xlab(expression(bar(theta)["ref"] ~ "[vol. %]")) +
    #scale_color_manual("", values = c("gray50", "red", "blue", "lightblue"),
    #guide = guide_legend(override.aes = list(
    #   size = c(3,1,1),
    #  linetype = c("blank", rep("solid", 2)),
    #   shape = c(16, NA, NA))),
    #) +
    scale_color_brewer("", type = "div", palette = 1,
                       guide = guide_legend(override.aes = list(
                         size = c(3,1,1,1),
                         linetype = c("blank", rep("solid", 3)),
                         shape = c(16, NA, NA, NA)))) + 
    theme(legend.position = "bottom",
          panel.grid.major.x = element_blank()) +
    facet_wrap(~sensor, scales = "free")

}
