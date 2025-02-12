library(tractable)
library(tidyverse)
library(dplyr)

# Remove all variables in the environment
rm(list = ls())

data_dir = "/Users/nevao/Documents/Adol_WM_Data/Z_scores_time_2_1_split"
metric = "md"
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

# Function to extract full statistics from GAM model
extract_gam_stats <- function(model, tract) {
  model_summary <- summary(model)  # Get model summary
  
  # Extract R-squared, Deviance explained, EDF, F-statistic, p-values
  intercept_p_value <- model_summary$s.table[1, "p-value"]  # Intercept p-value
  sex_p_value <- model_summary$s.table[2, "p-value"]  # p-value for sexM term
  rsq <- model_summary$r.sq  # R-squared
  deviance_explained <- model_summary$dev.expl  # Deviance explained
  edf <- model_summary$edf[1]  # Effective degrees of freedom for the smooth term
  f_statistic <- model_summary$s.table[1, "F"]  # F-statistic for the intercept
  
  tibble(
    tractID = tract,
    rsq = rsq,
    deviance_explained = deviance_explained,
    edf = edf,
    intercept_p_value = intercept_p_value,
    sex_p_value = sex_p_value,
    f_statistic = f_statistic
  )
}

# Run tractable_single_tract() for each tract and collect stats
tract_summary <- map_df(unique_tracts, ~ {
  model <- tractable_single_tract(
    df        = df_z,
    tract     = .x,
    target    = "z",
    regressors = c("sex")
  )
  extract_gam_stats(model, .x)  # Extract all statistics
})

# Print the summary
print(tract_summary)
  



