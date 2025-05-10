# This code is from Supplementary Materials for NM Muncy, A Kimbler, 
# AM Hedges-Muncy, DL McMakin, AT Mattfeld. General additive models address 
# statistical issues in diffusion MRI: An example with clinically anxious adolescents. 
# Neuroimage Clin 2022:33:102937. doi: 10.1016/j.nicl.2022.102937. Epub 2022 Jan 5.

make_spline_single_group_df <- function(gam_model, 
                                tract_data, 
                                tract, 
                                sex_val) {
  # Make a dataframe of fit for one group, with CI
  #
  # Arguments:
  #   gam_model = GAM object, produced by gam/bam
  #   tract_data = dataframe with tract data
  #   tract name (string)
  #   group_level = one level of the grouping factor (e.g., "M" or "F")
  #
  # Returns:
  #   final_df = dataframe with est, nodeID, CI, etc.

  num_comparisons <- 60
  
  adj_alpha <- 0.05 / num_comparisons
  
  z_adj <- qnorm(1 - adj_alpha / 2)

  # determine predicted differences
  df_sex_val <- plot_smooth(gam_model,
                       view = "nodeID",
                       se = z_adj,
                       cond = list(sex = sex_val),
                       rm.ranef = F
  )
  
  # Check if the adjusted confidence intervals contain zero
  significant <- ifelse(df_sex_val$fv$ll < 0 & df_sex_val$fv$ul > 0, 0, 1)
  
  # Add significance to a dataframe
  df_sex_val$fv$significant <-  significant
  
  return(df_sex_val$fv)
}