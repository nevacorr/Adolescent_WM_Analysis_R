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

df_z_male = subset(df_z, sex != "F")
df_z_female = subset(df_z, sex != "M")

source("run_tractable_single_tract_model.R")
source("apply_fdr_correction.R")

# # Run tractable_single_tract() for each tract and collect stats
results_df <-  run_tractable_single_tract_model(df_z, unique_tracts, 1)
results_df <- apply_fdr_correction(results_df, "sex_p")
print(metric)
print("with sex as covariate")
print(results_df)
results_df_male <-  run_tractable_single_tract_model(df_z_male, unique_tracts, 0)
results_df_male <- apply_fdr_correction(results_df_male, "intercept_p")
print(metric)
print("with only male data")
print(results_df_male)
results_df_female <-  run_tractable_single_tract_model(df_z_female, unique_tracts, 0)
results_df_female <- apply_fdr_correction(results_df_female, "intercept_p")
print(metric)
print("with only female data")
print(results_df_female)
print("\n")

significant_tracts_sex_difference <- results_df %>%
  filter(FDR_corrected <= 0.05) %>% 
  pull(tract)

significant_tracts_male <- results_df_male %>% 
  filter(FDR_corrected <= 0.05 ) %>% 
  pull(tract)

significant_tracts_female <- results_df_female %>% 
  filter(FDR_corrected <= 0.05 ) %>% 
  pull(tract)

print(paste("Tracts with significant post-covid sex differences in", metric, ":"))
cat(significant_tracts_sex_difference, sep = "\n")

print(paste("Tracts with significantly different",metric,"values post-covid for males:"))
cat(significant_tracts_male, sep = "\n")

print(paste("Tracts with significantly different",metric,"values post-covid for females:"))
cat(significant_tracts_female, sep = "\n")