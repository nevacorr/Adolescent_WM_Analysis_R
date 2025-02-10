library(tractable)
library(tidyverse)
library(stringr)
library(dplyr)
library(RColorBrewer)

# Remove all variables in the environment
rm(list = ls())

data_dir = "/Users/nevao/Documents/Adol_WM_Data"

MPF_v1_filename = "genzMPF_tractProfiles_visit1.csv"
FA_v1_filename = "genzFA_tractProfiles_visit1.csv"
MD_v1_filename = "genzMD_tractProfiles_visit1.csv"

MPF_v1 <- read.csv(file.path(data_dir, MPF_v1_filename))
FA_v1 <- read.csv(file.path(data_dir, FA_v1_filename))
MD_v1 <- read.csv(file.path(data_dir, MD_v1_filename))

# Reshape each dataframe and add a 'type' column
df_mpf_long <- MPF_v1 %>%
  pivot_longer(cols = matches(".*_"),  
               names_to = c("tractID", "nodeID"), 
               names_sep = "_", 
               values_to = "mpf")

df_fa_long <- FA_v1 %>%
  pivot_longer(cols = matches(".*_"),
               names_to = c("tractID", "nodeID"),
               names_sep = "_", values_to = "fa")

df_md_long <- MD_v1 %>%
  pivot_longer(cols = matches(".*_"),
               names_to = c("tractID", "nodeID"),
               names_sep = "_",
               values_to = "md")

# Join the dataframes by "Subject", "tractID", and "node"
df_combined <- df_mpf_long %>%
  full_join(df_fa_long, by = c("Subject", "tractID", "nodeID")) %>%
  full_join(df_md_long, by = c("Subject", "tractID", "nodeID"))

# Remove "sub-genz" from the "Subject" column
df_combined <- df_combined %>%
  mutate(Subject = str_replace(Subject, "sub-genz", ""))

# Replace "." with space in the "tractID" column
df_combined$tractID <- str_replace_all(df_combined$tractID, "\\.", " ")

# Using mutate to add an Age column based on Subject numbers
df_combined <- df_combined %>%
  mutate(Age = case_when(
    Subject >= 100 & Subject < 200 ~ 9,
    Subject >= 200 & Subject < 300 ~ 11,
    Subject >= 300 & Subject < 400 ~ 13,
    Subject >= 400 & Subject < 500 ~ 15,
    Subject >= 500 & Subject < 600 ~ 17
  ))

df_combined$nodeID <- as.integer(df_combined$nodeID)

# View the combined dataframe
# print(df_combined)

# Find rows with any NA values
# rows_with_na <- df_combined[rowSums(is.na(df_combined)) > 0, ]
# print(rows_with_na)

d2 <- df_combined %>%
  select(tractID, Subject) %>%
  distinct() %>%
  mutate(dummy = 1) %>%
  left_join(
    df_combined %>%
      filter(nodeID == 2) %>%
      select(tractID, Subject, node_2 = fa) %>%
      mutate(dummy = 1),
    by = c("tractID", "Subject", "dummy")
  ) %>%
  left_join(
    df_combined %>%
      filter(nodeID == 99) %>%
      select(tractID, Subject, node_99 = fa) %>%
      mutate(dummy = 1),
    by = c("tractID", "Subject", "dummy")
  ) %>%
  select(-dummy)

# Join d1 with d2 to get the fa_node2 and fa_node99 values 
d1_with_d2 <- df_combined %>%
  left_join(d2 %>% select(tractID, Subject, node_2, node_99), by = c("tractID", "Subject"))

#  Replace NaN values in fa based on nodeID conditions
d1_updated <- d1_with_d2 %>%
  mutate(
    fa = case_when(
      is.na(fa) & nodeID == 1 ~ node_2,     # Replace NaN fa with node_2 fa if nodeID fa is 1
      is.na(fa) & nodeID == 100 ~ node_99,  # Replace NaN fa with node_99 fa if nodeID fa is 100
      TRUE ~ fa                            # Keep the original fa if no replacement is needed
    )
  ) %>%
  select(-node_2, -node_99)  # Drop the temporary columns node_2 and node_99

# Remove all rows with any NaN or NA values
df_cleaned <- na.omit(d1_updated)

# Find rows with any NA values
# rows_with_na <- df_cleaned[rowSums(is.na(df_cleaned)) > 0, ]
# print(rows_with_na)

# Get a list of unique values in the tractID column
unique_tracts <- unique(d1_updated$tractID)

tractable::plot_tract_profiles(
  df          = df_cleaned,
  group_col   = "Age",
  n_groups    = 5,
  metrics     = c("md", "mpf", "fa"),
  bundles     = unique_tracts,
  figsize   = c(8, 8),
  pal_name = "Spectral"
)

gam_fit_cst <- tractable::tractable_single_bundle(
  df             = df_cleaned,
  tract          = "Right ILF",
  participant_id = "Subject",
  dwi_metric     = "fa",
  covariates     = c("Age"),
  k              = "auto"
)

summary(gam_fit_cst)

results <- list()

# Loop through each tract in the list
for (tract in unique_tracts) {
 
  fit <- tractable::tractable_single_bundle(
    df             = df_cleaned,
    tract          = tract,      
    participant_id = "Subject",
    group_by       = "Age",
    dwi_metric     = "fa",
    covariates     = c("Age"),
    k              = "auto"
  )
  
  results[[tract]] <- fit
}
