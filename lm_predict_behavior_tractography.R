library(lme4)
library(dplyr)
library(tidyr)
library(broom.mixed)
library(ggplot2)

# Remove all variables in the environment
rm(list = ls())

source("load_multinode_tract_data.R")
source("average_across_splits.R")
source("plot_behav_vs_measure.R")

data_dir = "/Users/nevao/Documents/Adol_WM_Data/Z_scores_time_2_100_splits"
metric <-  "md"
behavior <- "FlankerSU"
splits <-  100
data_filename = paste0("Z_time2_", metric, "_", splits, "_splits.csv")

# read data file
z_orig <- read.csv(file.path(data_dir, data_filename))

# remove first column
z_orig <- select(z_orig, -X)

if ("split" %in% colnames(z_orig)) {
  z_orig <- average_across_splits(z_orig)
}

df_z_brain = reformat_data(z_orig)

behav_df <- read.csv('Z_scores_all_meltzoff_cogn_behav_visit2.csv')

# Keep only 'participant_id' and behavior columns
behav_df <- behav_df[, c("participant_id", behavior)]

#  Rename column z to z_brain in df_z_brain
df_z_brain <- df_z_brain %>%
  rename(z_brain = z)

df_brainz_by_tract <- df_z_brain %>%
  group_by(subjectID, tractID, age, sex) %>%
  summarize(mean_dwi = mean(z_brain, na.rm = TRUE), .groups = "drop")

# df_brainz_by_tract <- df_brainz_by_tract %>% 
#   filter(tractID %in% selected_tracts)

# Merge behav_df into df_z_brain by matching subjectID to participant_id
# This will add the behavior column to df_z_brain
df_z_all_data <- df_brainz_by_tract %>%
  left_join(behav_df, by = c("subjectID" = "participant_id"))

#  Remove rows where z score or behavior is NA
df_z_all_data <- df_z_all_data %>% drop_na()

# Convert variables to factors
df_z_all_data$subjectID <- as.factor(df_z_all_data$subjectID)
df_z_all_data$tractID <- as.factor(df_z_all_data$tractID)
df_z_all_data$sex <- as.factor(df_z_all_data$sex)
df_z_all_data$age <- as.factor(df_z_all_data$age)

# Get list of tracts
tracts <- unique(df_z_all_data$tractID)

# Run LM model per tract
results <- lapply(tracts, function(tract) {
  data_subset <- df_z_all_data %>% filter(tractID == tract)
  
  # Construct formula string
  formula_str <- paste(behavior, "~ mean_dwi + sex + age")
  
  # Convert to formula
  model <- lm(as.formula(formula_str), data = data_subset)
  
  broom::tidy(model) %>% mutate(tractID = tract)
})

# Combine results
results_df <- bind_rows(results)

# Correct for multiple comparisons
results_df <- results_df %>%
  group_by(term) %>%
  mutate(p.adjust = p.adjust(p.value, method = "fdr")) %>%
  filter(term == "mean_dwi") %>%
  arrange(p.adjust)

extra_title = 'p_uncorr=0.078'

p <- plot_behav_vs_measure(df_z_all_data, 'Callosum.Forceps.Minor',  'mean_dwi', behavior,
                      toupper(metric), extra_title)
print(p)
print('computations complete')