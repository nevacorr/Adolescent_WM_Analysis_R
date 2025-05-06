library(tractable)
library(tidyverse)
library(dplyr)
library(gratia)
library(ggplot2)

# Remove all variables in the environment
rm(list = ls())

source("average_across_splits.R")
source("load_multinode_tract_data.R")

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

# convert data to long format and add sex and age columns
df_z = reformat_data(z_orig)

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

tract='Left.Arcuate'
model <-  tractable_single_tract(
  df = df_z,
  tract = tract,
  target = 'z',
  node_k = 10
)

sm <- smooth_estimates(model)

node_sm <- subset(sm, as.character(.smooth) == "s(nodeID)")

# Step 3: Calculate 95% confidence intervals
node_sm$lower <- node_sm$.estimate - 1.96 * node_sm$.se
node_sm$upper <- node_sm$.estimate + 1.96 * node_sm$.se

# Step 4: Mark nodes as significant if CI does not cross zero
node_sm$significant <- with(node_sm, lower > 0 | upper < 0)

p <- ggplot(node_sm, aes(x = nodeID, y = .estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Smooth estimate over nodeID",
       y = "Estimated smooth", x = "NodeID")

plot(p)

mystop=1