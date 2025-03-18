
average_across_splits <- function(df) {
  
  # average columns with the same value for split
  df_avg <- df %>%
    group_by(participant_id) %>% 
    summarise(across(-split, ~mean(.x, na.rm = TRUE)), .groups = "drop")
  
  return(df_avg)
}