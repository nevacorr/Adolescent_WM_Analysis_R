library(tractable)
library(tidyverse)
library(dplyr)
library(gratia)
library("mgcv")
library("tidymv")
library("fitdistrplus")
library("ggplot2")
library("itsadug")
library("mgcViz")
library("ggpubr")
library("rstatix")

# Remove all variables in the environment
rm(list = ls())

source("average_across_splits.R")
source("load_multinode_tract_data.R")
source("make_plots_with_tractable.R")
source("run_tractable_single_tract_and_itsadug.R")
source("apply_fdr_correction.R")
source("plot_from_tractable_sourcecode_edited.R")
source("plot_spline_diff.R")
source("plot_gam_splines.R")
source("plot_gam_splines_one_group.R")
source("make_spline_diff_df.R")
source("calc_spline_diff.R")
source("switch_tract_name.R")
source("run_single_tract.R")
source("calc_gam_stats.R")
source("make_spline_single_group_df.R")


data_dir = "/Users/nevao/Documents/Adol_WM_Data/Z_scores_time_2_100_splits"
metric <-  "md"
splits <-  100
data_filename = paste0("Z_time2_", metric, "_", splits, "_splits.csv")


# define path of image output
output_image_path <- "/Users/nevao/R_Projects/AdolWMAnalysis/tract profile plots"
# define path of node stats output
output_stats_path <- "/Users/nevao/R_Projects/AdolWMAnalysis/tract stats files"

# read data file - zscores representing deviation from normative model
z_orig <- read.csv(file.path(data_dir, data_filename))

# remove first column
z_orig <- select(z_orig, -X)

# If there is a split column, average z-scores across the splits for each subject
if ("split" %in% colnames(z_orig)) {
  z_orig <- average_across_splits(z_orig)
}

# convert data to long format and add sex and age columns
df_z = reformat_data(z_orig)

# plot all tracts by sex
plot_tracts(df_z, 1, output_image_path)

# convert sex to factor
df_z <- df_z %>%
  mutate(
    sex = factor(sex)
  )

# convert subjectID to factor
df_z$subjectID <- factor(df_z$subjectID)

# Get unique tract names
unique_tracts <- unique(df_z$tractID)

# remove rows without zscores
df_z <- df_z %>% filter(!is.na(z))

# Run single_tract() for each tract and collect stats
run_single_tract(df_z, unique_tracts, metric)

