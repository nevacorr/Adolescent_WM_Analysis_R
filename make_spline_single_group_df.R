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
  
  # Make sure the plot folder exists
  dir.create(file.path(output_image_path, "ind_sex_plots"), showWarnings = FALSE)
  
  # Make a plot output file
  full_path <- file.path(output_image_path, "ind_sex_plots",
                         paste0(tract, "model_plot", sex_str, ".png"))
  png(full_path, width=800, height=600, res=150)
  
  set.seed(123)
  # determine predicted differences
  df_sex_val <- plot_smooth(gam_model,
                       view = "nodeID",
                       cond = list(sex = sex_str),   # only include sex of interest
                       n = 60, # 60 nodes
                       se = 1.96,
                       main = tract,
                       sim.ci = TRUE,
                       rm.ranef = TRUE
  )
  
  df_sex_val$fv$subjectID <- "population_mean"
  
  # Close device
  dev.off()
  
  # Determine significance: 1 = significant, 0 = not
  df_sex_val$fv$signif <- ifelse(df_sex_val$fv$ll > 0 | df_sex_val$fv$ul < 0, 1, 0)
  
  # Keep only node number and significance
  sig_table <- df_sex_val$fv[, c("nodeID", "signif")]
  
  # Write significance table to CSV
  sig_file <- file.path(output_stats_path, paste0(tract, "_significance_", sex_str, ".csv"))
  write.csv(sig_table, sig_file, row.names = FALSE)
  
  return(df_sex_val$fv)
  
}