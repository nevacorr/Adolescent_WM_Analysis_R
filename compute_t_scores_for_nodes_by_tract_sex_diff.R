compute_t_scores_for_nodes_by_tract_sex_diff <- function(df_z, tract, metric) {
  
  # Initialize an empty dataframe to store t-test p-values for each node 
  node_pvalues <- data.frame(Node = integer(), 
                             P_value = numeric(), 
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
  
  # Plot values
  
  node_pvalues_plot <- as.data.frame(node_pvalues)
  
  # Add difference column
  node_pvalues_plot$Diff <- node_pvalues_plot$Z_mean_M - node_pvalues_plot$Z_mean_F
  
  # Flag significance
  node_pvalues_plot$Significant <- node_pvalues_plot$P_value <= 0.05
  
  # Plot mean Z for males and females
  p1 <- ggplot(node_pvalues_plot, aes(x = Node)) +
    geom_line(aes(y = Z_mean_M, color = "Male")) +
    geom_line(aes(y = Z_mean_F, color = "Female")) +
    geom_point(data = subset(node_pvalues_plot, Significant),
               aes(y = (Z_mean_M + Z_mean_F)/2), 
               color = "red", size = 2) +
    labs(y = "Mean Z", color = "Group",
         title = paste0(metric, 
                        " Mean Z across nodes by sex — Tract: ", tract)) +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background  = element_rect(fill = "white"))
  print(p1)
  
  # Plot difference (Male - Female)
  p2 <- ggplot(node_pvalues_plot, aes(x = Node, y = Diff)) +
    geom_line(color = "darkblue", size = 1) +
    geom_point(data = subset(node_pvalues_plot, Significant),
               aes(y = Diff), color = "red") +
    labs(y = "Difference (Male - Female)",
         title = paste0(metric, " Difference in mean Z across nodes — Tract: ", tract)) +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background  = element_rect(fill = "white"))
  print(p2)
  
  # Save the mean Z plot
  ggsave(filename = paste0("node_zdiff_sexdiff/", metric, "_", tract, "_meanZ_plot_F_M.png"), plot = p1, width = 8, height = 4, dpi = 300)
  
  # Save the difference plot
  ggsave(filename = paste0("node_zdiff_sexdiff/", metric, "_", tract, 
                           "_diff_plot_M_F.png"), plot = p2, width = 8, height = 4, dpi = 300)
  
  return(node_pvalues)
}