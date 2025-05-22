
library(mgcv)
library(dplyr)
library(tidyr)

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
for (i in 1)
# for (i in seq_along(tract_list)) 
  {
  print(paste('modeling tract', tract_list[i]))
  
  tract <- tract_list[i]
  
  # Filter the data for this tract
  df_sub <- df_z_all_data %>% filter(tractID == tract)
  
  # Fit the GAM model
  model <- gam(FlankerSU ~ sex + s(z_brain, k = 10) + s(subjectID, bs = "re"),
               data = df_sub, method = "REML")
  
  model <- gam(z_brain ~ FlankerSU + s(nodeiD, k=10) + s(subjectID, bs = "re")
               data = df_sub, method = "REML")
  
  # k needs to be large enough
  # take into account autocorrelation in the error (tractable code))
  # will get a pvalue associated with the flanker
  
  # Extract p-value for smooth term
  pvals[i] <- summary(model)$s.table["s(z_brain)", "p-value"]
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