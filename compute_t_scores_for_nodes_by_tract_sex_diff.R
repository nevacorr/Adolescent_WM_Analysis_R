compute_t_scores_for_nodes_by_tract_sex_diff <- function(df_z, tract) {

  # Initialize an empty dataframe to store t-test p-values for each node 
  node_pvalues <- data.frame(Node = integer(), 
                             P_value = numeric(), 
                             pooled_var = numeric(),
                             T_statistic = numeric(), 
                             Z_mean_M = numeric(),
                             Z_mean_F = numeric(),
                             Tract = character(), 
                             stringsAsFactors = FALSE)
  
  # Loop through each node in the current tract
  for (node in unique(df_z$nodeID)) {
    
    # Extract the observed data for the current node
    zs_M <- df_z[df_z$nodeID == node & df_z$tractID == tract 
                      & df_z$sex == "M", "z", drop = TRUE]
    zs_F <- df_z[df_z$nodeID == node & df_z$tractID == tract 
                      & df_z$sex == "F", "z", drop=TRUE]
    
    # Compute the mean z-scores for the current node 
    Z_mean_M <- mean(zs_M) 
    Z_mean_F <- mean(zs_F)
    
    Z_var_M <- var(zs_M)
    Z_var_F <- var(zs_F)
    
    n_M = length(zs_M)
    n_F = length(zs_F)
    
    # Calculate pooled variance
    pooled_var = (n_M - 1)/(n_M + n_F -2) * Z_var_M + 
                                          (n_F - 1)/(n_M + n_F -2) * Z_var_F
    
    t_stat = (Z_mean_M - Z_mean_F) / sqrt(pooled_var * (1/n_M + 1/n_F))
    
    df_t <- n_M + n_F - 2
    
    # Perform a t-test to see if the z-score is significantly different from zero
    p_value <- 2 * pt(-abs(t_stat), df= df_t)
    
    # Store the p-value and mean z-score for the current node and tract
    node_pvalues <- rbind(node_pvalues, data.frame(
      Node = node, 
      P_value = p_value, 
      Z_mean_M = Z_mean_M,
      Z_mean_F = Z_mean_F,
      Tract = tract
    ))
  }
return(node_pvalues)
}