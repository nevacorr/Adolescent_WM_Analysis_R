library(tractable)
library(tidyverse)
library(dplyr)

# Remove all variables in the environment
rm(list = ls())

data_dir = "/Users/nevao/Documents/Adol_WM_Data/Z_scores_time_2_1_split"
metric = "md"
data_filename = paste0("Z_time2_", metric, "_1_splits_6regs.csv")

source("load_multinode_tract_data.R")

df_z = load_data(data_dir, data_filename, metric)

# Visualize the data

tractable::plot_tract_profiles(
  df = df_z, 
  metrics = "z", 
  # bundles = c("Left.IFOF", "Right.IFOF"),
  group_col = "sex",
  n_groups = 2,
  figsize   = c(8, 8),
  pal_name = "Spectral"
)

