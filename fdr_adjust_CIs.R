
fdr_adjust_CIs <- function(df) {

  # Calculate the half-width of the confidence intervals
  half_width <- (df$ul - dfll) / 2
  
  # Calculate the z-values (test statistics)
  z_values <- df$fit / half_width
  
  # Calculate p-values from z-values (two-tailed test)
  p_values <- 2 * (1 - pnorm(abs(z_values)))   
  
  # Apply Benjamini-Hochberg procedure to p-values
  adjusted_pvalues <- p.adjust(p_values, method = "BH")
  
  # Calculate the adjusted z-values (inverse of p-value adjustment)
  adjusted_z_values <- qnorm(1 - adjusted_pvalues / 2)  # for two-tailed test
  
  # Adjust confidence intervals based on the adjusted z-values
  adjusted_lowerCI <- estimates - adjusted_z_values * half_width
  adjusted_upperCI <- estimates + adjusted_z_values * half_width
  
  # Check if the adjusted confidence intervals contain zero
  significant <- ifelse(adjusted_lowerCI < 0 & adjusted_upperCI > 0, 0, 1)
  
  # Step 8: Combine results into a dataframe
  df$significant <-  significant
  df$adjusted_lowerCI <- adjusted_lowerCI
  df$adjusted_upperCI <- adjusted_upperCI
  
  return (df)

}