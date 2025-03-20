library(tractable)
library(tidyverse)
library(dplyr)

# Remove all variables in the environment
rm(list = ls())

data_dir = "/Users/nevao/Documents/Adol_WM_Data/Z_scores_time_2_100_splits"
metric = "md"
splits = 88
data_filename = paste0("Z_time2_", metric, "_", splits, "_splits.csv")

# read data file
z_orig <- read.csv(file.path(data_dir, data_filename))

# remove first column
z_orig <- select(z_orig, -X)

source("average_across_splits.R")

# average for each subject across all splits
z_orig <- average_across_splits(z_orig)

source("load_multinode_tract_data.R")

df_z = reformat_data(z_orig)

df_z <- df_z[df_z$tractID == "Left.SLF", ]

df_z_male = subset(df_z, sex != "F")

df_z_male <- df_z_male %>% select(-sex, -age)

df_z_male$subjectID <- factor(df_z_male$subjectID)

plot_tract_profiles(
  df = df_z_male, 
  y = "z", 
  tracts="Left.SLF",
  save_figure = TRUE,
  ribbon_alpha = 0.20,
  width=10,
  height=10
)

model <-  tractable_single_tract(
  df = df_z_male,
  tract = "Left.SLF",
  target = 'z'
)

model_summary = summary(model)
print(model_summary)

# Get model predictions (fitted values)
predicted_values <- predict(model)

df_z_male$predicted_values <- predicted_values

# Plot the predicted values instead of raw z-scores
plot_tract_profiles(
  df = df_z_male, 
  y = "predicted_values",  
  tracts = "Left.SLF",
  save_figure = TRUE,
  ribbon_alpha = 0.20,
  width = 10,
  height = 10
)

