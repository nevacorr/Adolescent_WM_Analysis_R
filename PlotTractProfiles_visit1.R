library(tractable)
library(tidyverse)
library(stringr)
library(dplyr)

# Remove all variables in the environment
rm(list = ls())

data_dir = "/Users/nevao/Documents/Adol_WM_Data"

MPF_v1_filename = "genzMPF_tractProfiles_visit1.csv"
FA_v1_filename = "genzFA_tractProfiles_visit1.csv"
MD_v1_filename = "genzMD_tractProfiles_visit1.csv"

MPF_v1 <- read.csv(file.path(data_dir, MPF_v1_filename))
FA_v1 <- read.csv(file.path(data_dir, FA_v1_filename))
MD_v1 <- read.csv(file.path(data_dir, MD_v1_filename))

sarica <- tractable::read_afq_sarica()

# Assuming you have dataframes df_mpf, df_f, and df_m
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
    Subject >= 100 & Subject < 200 ~ factor(9),
    Subject >= 200 & Subject < 300 ~ factor(11),
    Subject >= 300 & Subject < 400 ~ factor(13),
    Subject >= 400 & Subject < 500 ~ factor(15),
    Subject >= 500 & Subject < 600 ~ factor(17)
  ))

df_combined$nodeID <- as.integer(df_combined$nodeID)
#df_combined$Age <- as.integer(df_combined$Age)
# Add the "group" column and set all values to 1, with class "factor"
#df_combined$group <- factor(1)

# View the combined dataframe
print(df_combined)

unique_count <- length(unique(sarica$age))
print(unique_count)

#df_combined <- df_combined %>%
#  filter(!(tractID %in% c("Left Uncinate", "Right Arcuate")))

# Find rows with any NA values
rows_with_na <- df_combined[rowSums(is.na(df_combined)) > 0, ]
print(rows_with_na)

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

# Step 1: Join d1 with d2 to get the fa_node2 and fa_node99 values (no need for nodeID in d2)
d1_with_d2 <- df_combined %>%
  left_join(d2 %>% select(tractID, Subject, node_2, node_99), by = c("tractID", "Subject"))

# Step 2: Replace NaN values in fa based on nodeID conditions
d1_updated <- d1_with_d2 %>%
  mutate(
    fa = case_when(
      is.na(fa) & nodeID == 1 ~ node_2,     # Replace NaN with node_2 if nodeID is 1
      is.na(fa) & nodeID == 100 ~ node_99,  # Replace NaN with node_99 if nodeID is 100
      TRUE ~ fa                            # Keep the original fa if no replacement is needed
    )
  ) %>%
  select(-node_2, -node_99)  # Drop the temporary columns node_2 and node_99

# Remove all rows with any NaN or NA values
df_cleaned <- na.omit(d1_updated)

# Find rows with any NA values
rows_with_na <- df_cleaned[rowSums(is.na(df_cleaned)) > 0, ]
print(rows_with_na)

tractable::plot_tract_profiles(
  df          = df_cleaned,
  group_col   = "Age",
  n_groups    = 5,
  metrics     = c("fa", "md"),
  bundles     = list("Right Corticospinal", "Right SLF")
)

