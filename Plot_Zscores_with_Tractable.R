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

# # Run tractable_single_tract() for each tract and collect stats
results_df <- apply_fdr_correction(output$results_df, "sex_p")
print(metric)
print("with sex as covariate")
print(results_df)
output_male <-  run_tractable_single_tract_model(df_z_male, unique_tracts, 0, metric)
results_df_male <- apply_fdr_correction(output_male$results_df, "intercept_p")
print(metric)
print("with only male data")
print(results_df_male)
output_female <-  run_tractable_single_tract_model(df_z_female, unique_tracts, 0, metric)
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

tractnames = c("Left.Thalamic.Radiation", "Right.Thalamic.Radiation")
plot_specific_tracts(df_z,tractnames, 1, "_sig_m_and_f", 6.667, 3.333, metric)
tractnames = c("Callosum.Forceps.Major","Callosum.Forceps.Minor","Left.Arcuate",
                "Right.Arcuate", 
                "Left.IFOF", "Right.IFOF","Right.ILF", "Right.Corticospinal")
plot_specific_tracts(df_z,tractnames, 1, "_sig_f_only", 10, 10, metric)



node_vals_male = output_male$node_pvalues
node_vals_female = output_female$node_pvalues
write.csv(node_vals_female, paste0(metric, "_node_stats_female.csv"), row.names = FALSE)
write.csv(node_vals_male, paste0(metric, "_node_stats_male.csv"), row.names = FALSE)
