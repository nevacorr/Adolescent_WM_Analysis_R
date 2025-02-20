library(tractable)
library(tidyverse)
library(dplyr)

# Remove all variables in the environment
rm(list = ls())

data_dir = "/Users/nevao/Documents/Adol_WM_Data/Z_scores_time_2_1_split"
metric = "fa"
data_filename = paste0("Z_time2_", metric, "_1_split_all_tracts.csv")

source("load_multinode_tract_data.R")

df_z = load_data(data_dir, data_filename, metric)

# Visualize the data

plot_tract_profiles(
  df = df_z, 
  y = "z", 
  group_col = "sex",
  n_groups = 2,
  save_figure = TRUE,
  ribbon_alpha = 0.20,
  group_pal = "Set1",
  width=10,
  height=10
)

# Convert sex to factors
df_z <- df_z %>%
  mutate(
    sex = factor(sex)
  )

# Get unique tract names
unique_tracts <- unique(df_z$tractID)

df_z <- df_z %>% filter(!is.na(z))



# # Run tractable_single_tract() for each tract and collect stats


print(results_df)
