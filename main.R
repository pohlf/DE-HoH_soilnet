rm(list=ls())
library(tidyverse)
library(lubridate)
library(ggpubr)
library(ggthemes)
library(ggforce)
library(cowplot)
library(lubridate)
library(ggpmisc)
set.seed(123456)

sapply(list.files("code", full.names = T), source)

# 0. read data
# correct this to data management portal as soon as uploaded: 
df <- read_csv("/home/fpohl/Nextcloud/Cloud/Git/Soilnet_CDF/data/data_filtered_before_cor.csv") %>%
  # add arithmetic mean and count of working sensors per day
  group_by(Date, Layer) %>%
  mutate(mean_arithmetic = mean(value),
         Sensors = paste(unique(ID), collapse = ','),
         n = n()) %>%
  ungroup() 

# 1. CALCULATE COEFFICIENT OF VARIATION (CV) 
coefficient_of_variation(df)

# 2. ESTIMATE MINIMUM NUMBER OF REQUIRED SAMPLES (MNRS)
minimum_number_of_required_samples()
read_csv("data/mnrs.csv") 

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

# colours
my_blue <- RColorBrewer::brewer.pal(9,"Blues")[7]
my_grey <- "grey80"
grey_palette <- RColorBrewer::brewer.pal(9,"Greys")[3:8]
blue_palette <- RColorBrewer::brewer.pal(9,"Blues")[3:8]

# FIG.2
plot_data(blue = my_blue)
ggsave("data_plot.png", width = 21, height = 13, unit = "cm", dpi = 300, bg = "white")

# FIG.3
plot_bias(df_results, my_blue, my_grey)
ggsave("bias_plot.png", width = 21, height = 13, unit = "cm", dpi = 300, bg = "white")

# FIG.4
plot_gof_vs_temporal_stability(df_results, my_blue, my_grey)
ggsave("gof_vs_stability.png", dpi = 300, width = 15, height = 8, unit = "cm")

# FIG.5
plot_gof_low_sample(blue_palette, grey_palette)
ggsave("gof_avg_ecdf.png", dpi = 300, width = 15, height = 9, unit = "cm")

# FIG.6