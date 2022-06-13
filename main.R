rm(list=ls())
library(tidyverse)
library(lubridate)
library(ggpubr)
set.seed(928)

# correct this to data management portal as soon as uploaded: 
df <- read_csv("/home/fpohl/Nextcloud/Cloud/Git/Soilnet_CDF/data/data_filtered_before_cor.csv")

# add arithmetic mean and count of working sensors per day
df2 <- df %>%
  group_by(Date, Layer) %>%
  mutate(mean_arithmetic = mean(value),
         n = n()) %>%
  ungroup()

