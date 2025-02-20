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

results_df <- data.frame(
  metric = character(),
  tract = character(),
  intercept_p = numeric(),
  sex_p = numeric(),
  R_sq = numeric(),
  deviance_explained = numeric(),
  stringsAsFactors = FALSE
)

# # Run tractable_single_tract() for each tract and collect stats

for (tract in unique_tracts) {
  # Fit the model
  model <-  tractable_single_tract(
    df = df_z,
    tract = tract,
    target = 'z',
    regressors = c("sex"),
    node_group = "sex"
  )

  model_summary = summary(model)

  sex_p_value = model_summary$p.table["sexM", "Pr(>|t|)"]
  intercept_p_value = model_summary$p.table["(Intercept)", "Pr(>|t|)"]
  R_sq = model_summary$r.sq
  dev_exp = model_summary$dev.expl
  
  # Put results in dataframe
  results_df <- rbind(results_df, data.frame(
    metric = metric,
    tract = tract,
    intercept_p = sprintf("%.3f", intercept_p_value),
    sex_p = sprintf("%.3f",sex_p_value),
    R_sq = sprintf("%.3f",R_sq),
    deviance_explained = sprintf("%.3f",dev_exp)
  ))
  
}

print(results_df)
