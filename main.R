rm(list=ls())
library(tidyverse)
library(lubridate)
library(ggpubr)
library(cowplot)
library(broom)
library(tsibble)
library(arrangements)
library(future.apply)
library(dpseg)
library(ggridges)
plan(multisession)

set.seed(123456)                         shape = c(16, NA, NA, NA)))) + 
  theme(legend.position = "bottom") +

sapply(list.files("code", full.names = T), source)

# 0. read data
# data can be downloaded from https://www.ufz.de/record/dmp/archive/12770/de/
df <- read_csv("/home/fpohl/Nextcloud/Cloud/Git/de-hoh_soilnet/data/de-hoh_soil_moisture_1D_04_2014-04_2021.csv") %>%
  #remove coordx+y cols
  dplyr::select(-c(coord_x,coord_y)) %>%
  # add arithmetic mean and count of working sensors per day
  group_by(Date, Layer) %>%
  mutate(mean_arithmetic = mean(value),
         Sensors = paste(unique(ID), collapse = ','),
         n = n()) %>%
  ungroup() 

# print number of sensors per layer
df %>%
  dplyr::select(Layer, ID) %>%
  distinct() %>%
  group_by(Layer) %>%
  summarise(n())

# 1. CALCULATE COEFFICIENT OF VARIATION (CV) 
coefficient_of_variation(df)

# 2. ESTIMATE MINIMUM NUMBER OF REQUIRED SAMPLES (MNRS)
minimum_number_of_required_samples()

df_mnrs <- read_csv("data/mnrs.csv") %>%
  dplyr::select(Layer, n_threshold) %>%
  left_join(df,.) %>%
  dplyr::select(-Sensors) %>%
  distinct()
write_csv(df_mnrs, "data/measurements_with_mnrs.csv")

# 3. CHARACTERISTICS OF TEMPORAL STABILITY
index_of_time_stability()

# 4. TRANSFORMATION
transformation()

# 5. RUN TEST FOR LOW SAMPLE SIZE
run_low_sample_size_test()

### PLOTS
16:00
# colours
my_blue <- RColorBrewer::brewer.pal(9,"Blues")[7]
my_grey <- "grey80"
grey_palette <- RColorBrewer::brewer.pal(9,"Greys")[3:8]
blue_palette <- RColorBrewer::brewer.pal(9,"Blues")[3:8]

# FIG.2
plot_data(blue = my_blue)
ggsave("plots/fig2_data_plot.png", width = 21, height = 13, unit = "cm", dpi = 300, bg = "white")

# FIG.3
plot_bias(my_blue, my_grey)
ggsave("plots/fig3_bias_plot.png", width = 21, height = 13, unit = "cm", dpi = 300, bg = "white")

# FIG.4
plot_gof_vs_temporal_stability(my_blue, my_grey)
ggsave("plots/fig4_gof_vs_stability.png", dpi = 300, width = 15, height = 8, unit = "cm")

# FIG.5
plot_gof_low_sample(blue_palette, grey_palette)
ggsave("plots/fig5_gof_avg_ecdf.png", dpi = 300, width = 15, height = 9, unit = "cm")

# FIG.6 + VALUES FOR TAB.1
plot_ts_results()
ggsave("plots/fig6_ts_swc.png", dpi = 300, width = 18, height = 10, unit = "cm", bg = "white")

# APPENDIX A
# available upon request from Floris Herrmanns floris.hermanns@ufz.de

# APPENDIX B
cdf_matching_poly_vs_plr()
ggsave("plots/appendixb_cdf_poly_vs_plr.png", dpi = 300, width = 15, height = 10, unit = "cm")

# APPENDIX C 
run_transformation_test_training()
