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
source("plot_gam_model.R")
source("calculate_node_significance.R")

data_dir = "/Users/nevao/Documents/Adol_WM_Data/Z_scores_time_2_100_splits"
metric <-  "md"
splits <-  100
data_filename = paste0("Z_time2_", metric, "_", splits, "_splits.csv")

# define path of image output
output_image_path <- "/Users/nevao/R_Projects/AdolWMAnalysis/tract profile plots"
# define path of node stats output
output_stats_path <- "/Users/nevao/R_Projects/AdolWMAnalysis/tract stats files"

# read data file
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

# # Run tractable_single_tract() for each tract and collect stats
output <-  run_tractable_single_tract_and_itsadug(df_z, unique_tracts, metric)
print(metric)
print(output$results_df)

significant_tracts_sex_difference <- output$results_df %>%
  filter(sex_p_fdr <= 0.05) %>% 
  pull(tract)

significant_tracts_male <- output$results_df %>% 
  filter(male_p_fdr <= 0.05 ) %>% 
  pull(tract)

significant_tracts_female <- output$results_df %>% 
  filter(female_p_fdr <= 0.05 ) %>% 
  pull(tract)

print(paste("Tracts with significant post-covid sex differences in", metric, ":"))
cat(significant_tracts_sex_difference, sep = "\n")

print(paste("Tracts with significantly different",metric,"values post-covid for males:"))
cat(significant_tracts_male, sep = "\n")

print(paste("Tracts with significantly different",metric,"values post-covid for females:"))
cat(significant_tracts_female, sep = "\n")

node_vals = output$node_pvalues

# Create separate data frames
node_vals_female <- subset(node_vals, sex == "F")
node_vals_male <- subset(node_vals, sex == "M")

node_vals_female <- node_vals_female %>% select(-sex, -Metric)
node_vals_male <- node_vals_male %>% select(-sex, -Metric)

# Output file paths
output_node_file_female <- file.path(output_stats_path, paste0(metric, "_node_stats_gam_female.csv"))
output_node_file_male <- file.path(output_stats_path, paste0(metric, "_node_stats_gam_male.csv"))

# Write to CSV
write.csv(node_vals_female, output_node_file_female, row.names = FALSE)
write.csv(node_vals_male, output_node_file_male, row.names = FALSE)

