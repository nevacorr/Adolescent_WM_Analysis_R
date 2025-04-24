library(tractable)
library(tidyverse)
library(dplyr)

# Remove all variables in the environment
rm(list = ls())

source("average_across_splits.R")
source("load_multinode_tract_data.R")
source("make_plots_with_tractable.R")
source("run_tractable_single_tract_and_itsadug.R")
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

# # Run tractable_single_tract() for each tract and collect stats
output <-  run_tractable_single_tract_and_itsadug(df_z, unique_tracts, metric)
print(metric)
print(output$results_df)
print("\n")
print(output$node_pvalues)

# significant_÷t_tracts_sex_difference, sep = "\n")
# 
# tractnames = c("Left.Thalamic.Radiation", "Right.Thalamic.Radiation")
# plot_specific_tracts(df_z,tractnames, 1, "_sig_m_and_f", 6.667, 3.333, metric)
# tractnames = c("Callosum.Forceps.Major","Callosum.Forceps.Minor","Left.Arcuate",
#                 "Right.Arcuate", 
#                 "Left.IFOF", "Right.IFOF","Right.ILF", "Right.Corticospinal")
# plot_specific_tracts(df_z,tractnames, 1, "_sig_f_only", 10, 10, metric)

