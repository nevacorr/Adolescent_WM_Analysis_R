# This code is from Supplementary Materials for NM Muncy, A Kimbler, 
# AM Hedges-Muncy, DL McMakin, AT Mattfeld. General additive models address 
# statistical issues in diffusion MRI: An example with clinically anxious adolescents. 
# Neuroimage Clin 2022:33:102937. doi: 10.1016/j.nicl.2022.102937. Epub 2022 Jan 5.

make_spline_diff_df <- function(gam_model, 
                                tract_data, 
                                tract, 
                                factor_a, 
                                factor_b) {
  # Make a dataframe of fit differences when testing 2 splines
  #
  # Get difference values from testing whether spline A
  # differs from spline B at each node.
  #
  # Arguments:
  #   gam_model = GAM object, produced by gam/bam
  #   factor_a = group factor (0-2)
  #   factor_b = group factor (0-2)
  #
  # Returns:
  #   df_pair = dataframe of differences
  
  # determine predicted differences
  df_pair <- plot_diff(gam_model,
                       view = "nodeID",
                       comp = list(sex = c(factor_a, factor_b)),
                       rm.ranef = T,
                       plot = F,
                       values = list(nodeID = sort(unique(tract_data$nodeID)))
  )
  
  browser()
  
  # add Comparison column to df
  colnames(df_pair) <- c(colnames(df_pair[, 1:4]), "Comp")
  df_pair$Comp <- paste0(factor_a, factor_b)
  
  browser()
  
  return(df_pair)
}