library(ggplot2)

plot_nodewise_estimates <- function(model, tract, title = "Node-wise z Estimates") {
  # Extract parametric table
  model_summary <- summary(model)
  param_table <- as.data.frame(model_summary$p.table)
  
  # Extract node IDs from row names like "factor(nodeID)0"
  param_table$nodeID <- gsub("factor\\(nodeID\\)", "", rownames(param_table))
  param_table$nodeID <- as.numeric(param_table$nodeID)
  
  # Rename columns
  colnames(param_table)[1:4] <- c("Estimate", "StdError", "t_value", "p_value")
  
  # Flag significance
  param_table$significant <- param_table$p_value < 0.05
  
  # Generate the plot
  p <- ggplot(param_table, aes(x = nodeID, y = Estimate)) +
    geom_point(aes(color = significant)) +
    geom_errorbar(aes(
      ymin = Estimate - 1.96 * StdError,
      ymax = Estimate + 1.96 * StdError
    ), width = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
    labs(
      title = paste(tract, title),
      x = "Node ID",
      y = "Estimated z",
      color = "Significant (p < 0.05)"
    ) +
    theme_minimal()
  
  return(p)
}