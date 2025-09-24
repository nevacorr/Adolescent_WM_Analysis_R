library(tractable)
library(tidyverse)
library(mgcv)
library(itsadug)
library(fitdistrplus)
library(dplyr)
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

# Remove all variables in the environment
rm(list = ls())

source("average_across_splits.R")
source("load_multinode_tract_data.R")
source("make_plots_with_tractable.R")
source("run_tractable_single_tract_model.R")
source("apply_fdr_correction.R")
source("plot_from_tractable_sourcecode_edited.R")
source("run_single_tract.R")
source("calc_spline_diff.R")
source("make_spline_single_group_df.R")
source("make_spline_diff_df.R")
source("plot_spline_diff.R")
source("calc_gam_stats.R")

data_dir = "/Users/nevao/Documents/Adol_WM_Data/Z_scores_time_2_100_splits"
metric <-  "fa"
splits <-  100
data_filename = paste0("Z_time2_", metric, "_", splits, "_splits.csv")

# define path of image output
output_image_path <- "/Users/nevao/R_Projects/AdolWMAnalysis/tract profile plots"
# define path of node stats output
output_stats_path <- "/Users/nevao/R_Projects/AdolWMAnalysis/tract stats files"

# read z score data file
z_orig <- read.csv(file.path(data_dir, data_filename))

# remove first column
z_orig <- select(z_orig, -X)

if ("split" %in% colnames(z_orig)) {
  z_orig <- average_across_splits(z_orig)
}

# convert data to long format and add sex and age columns
df_z = reformat_data(z_orig)

# plot all tracts by sex
plot_tracts(df_z, 1, output_image_path)

# Convert sex to factor
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

# create separate dataframe for each sex
df_z_male = subset(df_z, sex != "F")
df_z_female = subset(df_z, sex != "M")

# Run tractable_single_tract() for each tract and collect stats
output <-  run_tractable_single_tract_model(df_z, df_z, unique_tracts, 1, metric, output_image_path, NULL)
results_df <- apply_fdr_correction(output$results_df, "sex_p")
print(metric)
print("with sex as covariate")
print(results_df)
dev.off()
output_male <-  run_tractable_single_tract_model(df_z, df_z_male, unique_tracts, 0, metric, output_image_path, 'male')
results_df_male <- apply_fdr_correction(output_male$results_df, "intercept_p")
print(metric)
print("with only male data")
print(results_df_male)
output_female <-  run_tractable_single_tract_model(df_z, df_z_female, unique_tracts, 0, metric, output_image_path, 'female')
results_df_female <- apply_fdr_correction(output_female$results_df, "intercept_p")
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

node_vals_male = output_male$node_muncy_pvalues_all
node_vals_female = output_female$node_muncy_pvalues_all
write.csv(node_vals_female, file.path(output_stats_path, paste0(metric, "_node_sig_stats_muncy_female.csv")), row.names = FALSE)
write.csv(node_vals_male, file.path(output_stats_path, paste0(metric, "_node_sig_stats_muncy_male.csv")), row.names = FALSE)
