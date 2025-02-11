
load_data <- function(data_dir, data_filename, metric) {

  # read data file
  z_orig <- read.csv(file.path(data_dir, data_filename))
  
  # remove first column
  z_orig <- select(z_orig, -X)
  
  # reformat to long and add tractID and nodeID columns
  z_df <- z_orig %>%
    rename(subjectID = participant_id) %>% # Rename participant_id column %>%
    pivot_longer(
      cols = -subjectID, #Keep subjectID fixed
      names_to = c("tractID", "nodeID"),
      names_sep = "_", # Split column names at underscore
      values_to = "z"
    ) %>%
    mutate(nodeID = as.integer(nodeID) - 21) # Renumber nodes so they start at 0 
  
  # add gender and sex columns and compute values for every row
  z_df <- z_df %>% 
    mutate(
      age = case_when(
        subjectID >= 100 & subjectID < 200 ~ 12,
        subjectID >= 200 & subjectID < 300 ~ 14,
        subjectID >= 300 & subjectID < 400 ~ 16
      ),
      sex = if_else(subjectID %% 2 == 1, "M", "F") #If subject number is odd, sex is male, if even sex is female
    )
  
  return(z_df)
}