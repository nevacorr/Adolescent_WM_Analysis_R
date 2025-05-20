make_spline_single_group_df <- function(gam_model, 
                                tract_data, 
                                tract,
                                output_image_path,
                                sex_str) {

  num_comparisons <- 60
  
  adj_alpha <- 0.05 / num_comparisons

  z_adj <- qnorm(1 - adj_alpha / 2)

  # Make a plot output file
  full_path <- file.path(output_image_path, 
                         paste0(tract, "model_plot", sex_str, ".png"))
  png(full_path, width=800, height=600, res=150)
  
  # determine predicted differences
  df_sex_val <- plot_smooth(gam_model,
                       view = "nodeID",
                       se = z_adj,
                       main = tract,
                       rm.ranef = T
  )
  
  # Close device
  dev.off()
  
  # Check if the adjusted confidence intervals contain zero
  pvalues <- ifelse(df_sex_val$fv$ll < 0 & df_sex_val$fv$ul > 0, 0.5, 0.01)
  
  # Add significance to a dataframe
  df_sex_val$fv$pvalues <-  pvalues
  
  return(df_sex_val$fv)
  
}