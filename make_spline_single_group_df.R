make_spline_single_group_df <- function(gam_model, 
                                tract_data, 
                                tract) {
  # Make a dataframe of fit for one group, with CI
  #
  # Arguments:
  #   gam_model = GAM object, produced by gam/bam
  #   tract_data = dataframe with tract data
  #   tract name (string)
  #   group_level = one level of the grouping factor (e.g., "M" or "F")
  #
  # Returns:
  #   df_sex_val$fv = dataframe with est, nodeID, CI, etc.

  num_comparisons <- 60
  
  adj_alpha <- 0.05 / num_comparisons

  z_adj <- qnorm(1 - adj_alpha / 2)

  # determine predicted differences
  df_sex_val <- plot_smooth(gam_model,
                       view = "nodeID",
                       se = z_adj,
                       main = tract,
                       rm.ranef = F
  )
  
  # Check if the adjusted confidence intervals contain zero
  pvalues <- ifelse(df_sex_val$fv$ll < 0 & df_sex_val$fv$ul > 0, 0.5, 0.01)
  
  # Add significance to a dataframe
  df_sex_val$fv$pvalues <-  pvalues
  
  browser()
  
  return(df_sex_val$fv)
  
ci_to_pvalue <- function(estimate, lower, upper, conf_level = 0.95) {
  # Get z critical value (e.g., 1.96 for 95% CI)
  z_critical <- qnorm(1 - (1 - conf_level) / 2)
  
  # Calculate SE from CI width
  se <- (upper - lower) / (2 * z_critical)
  
  # Calculate z-statistic
  z_stat <- estimate / se
  
  # Calculate two-sided p-value
  p_value <- 2 * (1 - pnorm(abs(z_stat)))
  
  return(p_value)
}
}