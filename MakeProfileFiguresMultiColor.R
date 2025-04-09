library(tractable)
library(tidyverse)
library(dplyr)

# Remove all variables in the environment
rm(list = ls())

source("average_across_splits.R")
source("load_multinode_tract_data.R")
source("make_plots_with_tractable.R")
source("run_tractable_single_tract_model.R")
source("apply_fdr_correction.R")
source("plot_from_tractable_sourcecode_edited.R")

data_dir = "/Users/nevao/Documents/Adol_WM_Data/Z_scores_time_2_100_splits"
metric <-  "md"
splits <-  100
data_filename = paste0("Z_time2_", metric, "_", splits, "_splits.csv")

# read data file
z_orig <- read.csv(file.path(data_dir, data_filename))

# remove first column
z_orig <- select(z_orig, -X)

if ("split" %in% colnames(z_orig)) {
  z_orig <- average_across_splits(z_orig)
}

df_z = reformat_data(z_orig)

plot_tracts(df_z, 1)

# Convert sex to factor
df_z <- df_z %>%
  mutate(
    sex = factor(sex)
  )

df_z$subjectID <- factor(df_z$subjectID)

# Get unique tract names
unique_tracts <- unique(df_z$tractID)

df_z <- df_z %>% filter(!is.na(z))

df_z_male = subset(df_z, sex != "F")
df_z_female = subset(df_z, sex != "M")

# Plot each tract individually
male_pvalues = read.csv(paste0(metric, "_node_stats_male.csv"))
female_pvalues = read.csv(paste0(metric, "_node_stats_female.csv"))

df_z_for_prof_plots = df_z
df_z_for_prof_plots$tractID <- gsub("\\.", " ", df_z_for_prof_plots$tractID)
unique_tracts_for_plots <- unique(df_z_for_prof_plots$tractID)
for (t in unique_tracts_for_plots) {
  plot_specific_tracts_new_format(df_z_for_prof_plots, t, 1, t, 5, 4, metric, pvalues)
}