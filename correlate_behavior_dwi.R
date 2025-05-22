
library(mgcv)
library(dplyr)
library(tidyr)
library(itsadug)
library(gratia)
library(ggplot2)

# Remove all variables in the environment
rm(list = ls())

source("load_multinode_tract_data.R")
source("average_across_splits.R")

data_dir = "/Users/nevao/Documents/Adol_WM_Data/Z_scores_time_2_100_splits"
metric <-  "md"
splits <-  100
data_filename = paste0("Z_time2_", metric, "_", splits, "_splits.csv")
selected_tracts <- c("Left.Uncinate", "Right.Uncinate", "Right.Thalamic.Radiation",
                     "Left.Thalamic.Radiation", "Left.IFOF", "Right.IFOF", 
                     "Left.ILF", "Right.ILF", "Left.SLF", "Right.SLF", 
                     "Left.Arcuate","Right.Arcuate", "Callosum.Forceps.Minor")

# read data file
z_orig <- read.csv(file.path(data_dir, data_filename))

# remove first column
z_orig <- select(z_orig, -X)

if ("split" %in% colnames(z_orig)) {
  z_orig <- average_across_splits(z_orig)
}

df_z_brain = reformat_data(z_orig)

behav_df <- read.csv('Z_scores_all_meltzoff_cogn_behav_visit2.csv')

# Keep only 'participant_id' and 'FlankerSU' columns
behav_df <- behav_df[, c("participant_id", "FlankerSU")]

#  Rename column z to z_brain in df_z_brain
df_z_brain <- df_z_brain %>%
  rename(z_brain = z)

# Merge behav_df into df_z_brain by matching subjectID to participant_id
# This will add the FlankerSU column to df_z_brain
df_z_all_data <- df_z_brain %>%
  left_join(behav_df, by = c("subjectID" = "participant_id"))

#  Remove rows where FlankerSU is NA
df_z_all_data <- df_z_all_data %>%
  filter(!is.na(FlankerSU))

df_z_all_data <- df_z_all_data %>%
  filter(tractID %in%  "Callosum.Forceps.Minor")

df_z_all_data$tractID <- as.factor(df_z_all_data$tractID)
df_z_all_data$subjectID <- as.factor(df_z_all_data$subjectID)

# Create empty lists to store results
tract_list <- unique(df_z_all_data$tractID)
pvals <- numeric(length(tract_list))
tract_names <- character(length(tract_list))

# Loop over each tract
# for (i in 1)
for (i in seq_along(tract_list))
  {
  print(paste('modeling tract', tract_list[i]))
  
  tract <- tract_list[i]
  
  # Filter the data for this tract
  df_sub <- df_z_all_data %>% filter(tractID == tract)
  
  df_sub$ar_start <- df_sub[['nodeID']] == 0
  
  # Fit the GAM model without autocorrelation
  
  model_no_rho <- gam(z_brain ~ FlankerSU + s(nodeID, k=10) + s(subjectID, bs = "re"),
               data = df_sub, method = "REML", rho = 0, AR.start = ar_start)
  
  # determine autocorrelation parameter, rho
  rho <- itsadug::start_value_rho(model_no_rho)
  
  # Fit the GAM model with autocorrelation
  
  model_with_rho <- gam(z_brain ~ FlankerSU + s(nodeID, k=10) + s(subjectID, bs = "re"),
                        data = df_sub, method = "REML", rho = rho, AR.start = ar_start)
  
  print(summary(model_with_rho))
  
  # Extract p-value for FlankerSU
  pvals[i] <- summary(model_with_rho)$p.table["FlankerSU", "Pr(>|t|)"]
  tract_names[i] <- tract
}

# Combine results into a dataframe
gam_results <- data.frame(
  tractID = tract_names,
  p_value = pvals,
  p_fdr = p.adjust(pvals, method = "fdr")
)

# View results
print(gam_results)