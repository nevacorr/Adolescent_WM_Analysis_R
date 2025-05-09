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
  
  browser()
  
  # determine predicted differences
  df_sex_val <- plot_smooth(gam_model,
                       view = "nodeID",
                       cond = list(sex = sex_val),
                       rm.ranef = F
  )
  
  browser()
  
  final_df <- fdr_adjust_CIs(df_sex_val$fv)
  
  browser()
  
  return(final_df)
}