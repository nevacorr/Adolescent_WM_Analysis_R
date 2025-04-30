calculate_node_significance <- function(tract_data, tract, metric, sex){
  # Treat nodeID as a factor and subject as a fixed effect
  # Do not include intercept
  tract_model <- gam(z ~ -1 + factor(nodeID) + s(subjectID, bs = "re"),
                      data = tract_data,
                      method = "REML")
  
  tract_summary <- summary(tract_model)
  
  # Get parametric terms (node effects)
  param_table <- as.data.frame(tract_summary$p.table)
  
  # Extract nodeID numbers from row names like "factor(nodeID)0"
  param_table$nodeID <- gsub("factor\\(nodeID\\)", "", rownames(param_table))
  param_table$nodeID <- as.numeric(param_table$nodeID)
  
  # Rename columns
  colnames(param_table)[1:4] <- c("Estimate", "StdError", "t_value", "p_value")
  
  # Mark significant nodes
  param_table$significant <- param_table$p_value < 0.05
  
  p <- plot_nodewise_estimates(tract_model, tract, metric, sex)
  
  print(p)
  
  return(param_table)
}