
compute_t_scores_for_nodes_by_tract <- function(df_z, tract) {

  # Initialize an empty dataframe to store t-test p-values for each node 
  node_pvalues <- data.frame(Node = integer(), 
                             P_value = numeric(), 
                             Z_mean = numeric(),
                             Tract = character(), 
                             stringsAsFactors = FALSE)
  
  # Loop through each node in the current tract
  for (node in unique(df_z$nodeID)) {
    
    # Extract the observed data for the current node
    observed_values <- df_z[df_z$nodeID == node & df_z$tractID == tract, "z", drop = TRUE]
    
    # Compute the mean z-score for the current node 
    mean_z <- mean(observed_values) 
    
    # Perform a t-test to see if the z-score is significantly different from zero
    t_test_result <- t.test(observed_values, mu = 0)
    
    p <- t_test_result$p.value
    
    # Store the p-value and mean z-score for the current node and tract
    node_pvalues <- rbind(node_pvalues, data.frame(
      Node = node, 
      P_value = t_test_result$p.value, 
      Z_mean = mean_z,
      Tract = tract
    ))
  }
  return(node_pvalues)
}