# This function generates population level GAM predictions for a single group
# (male or female) along a tract. It computes adjusted confidence intervals
# and plots a curve and saves it to file and returns a dataframe with predicted 
# fits, CI bounds, and simplified significance flag (whether value is within CI)

make_spline_single_group_df <- function(gam_model, 
                                tract_data, 
                                tract,
                                output_image_path,
                                output_stats_path,
                                sex_str) {
  
  sex_char <- ifelse(sex_str == "male", "M", "F")
  
  # Make sure the plot folder exists
  dir.create(file.path(output_image_path, "ind_sex_plots"), showWarnings = FALSE)
  
  # Make a plot output file
  full_path <- file.path(output_image_path, "ind_sex_plots",
                         paste0(tract, "model_plot", sex_char, ".png"))
  png(full_path, width=800, height=600, res=150)
  
  set.seed(123)
  # determine predicted differences
  df_sex_val <- plot_smooth(gam_model,
                       view = "nodeID",
                       cond = list(sex = sex_char),   # only include sex of interest
                       n = 60, # 60 nodes
                       se = 1.96,
                       main = tract,
                       sim.ci = TRUE,
                       rm.ranef = TRUE
  )
  
  df_sex_val$fv$subjectID <- "population_mean"
  
  # Close device
  dev.off()
  
  # Determine pvalue: 1 = significant, 0 = not
  df_sex_val$fv$pvalue <- ifelse(df_sex_val$fv$ll > 0 | df_sex_val$fv$ul < 0, 0.01, 0.6)
  
  # Sim.ci resulted in 60 nodes being increased to 200 nodes. Map back to original
  # 60 nodes
  # Extract the grid of nodeIDs used in prediction
  pred_nodes <- df_sex_val$fv$nodeID
  
  # Get original observed node IDs (assuming 1:60)
  orig_nodes <- sort(unique(tract_data$nodeID))  
  
  # Match closest prediction values to observed nodes
  closest_idx <- sapply(orig_nodes, function(n) which.min(abs(pred_nodes - n)))

  # Subset the dataframe to those closest nodes
  df_sex_val_closest <- df_sex_val$fv[closest_idx, ]
  
  # Overwrite nodeID to be exactly 0:59
  df_sex_val_closest$nodeID <- 0:(length(orig_nodes) - 1)
  
  # Reset row names
  rownames(df_sex_val_closest) <- NULL
  
  # Keep only node number and significance
  sig_table <- df_sex_val_closest[, c("nodeID", "pvalue")]
  
  # Write pvalues to CSV
  sig_file <- file.path(output_stats_path, paste0(tract, "_pvalue_", sex_str, ".csv"))
  write.csv(sig_table, sig_file, row.names = FALSE)
  
  return(df_sex_val_closest)
  
}