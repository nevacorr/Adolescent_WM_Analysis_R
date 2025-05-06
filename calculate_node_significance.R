calculate_node_significance <- function(tract_data, tract, metric, sex){
  # Treat nodeID as a factor and subject as a fixed effect
  # Do not include intercept
  
  # Make sure data is ordered properly
  tract_data <- tract_data[order(tract_data$subjectID, tract_data$nodeID), ]
  
  # Adds a new column where the first row of each subject's sequence is TRUE
  tract_data$startEvent <- c(TRUE, diff(as.numeric(tract_data$subjectID)) != 0)
  
  # Fit a model without autorcorrelation
  m1 <- gam(z ~ -1 + s(nodeID, bs = 'fs', k=10) + s(subjectID, bs = "re"),
                      data = tract_data,
                      method = "REML")
  
  print(summary(m1))
  
  # Obtain predicted z value from the model
  predicted_z <-  predict(m1, type="response")
  
  # Add predicted z to dataset
  tract_data$predicted_Z <- predicted_z
  
  newdata <- expand.grid(
    nodeID = unique(tract_data$nodeID), 
    subjectID = unique(tract_data$subjectID)
  )
  
  # Get smoothness estimates
  smooth_estimates_m1 <- smooth_estimates(m1, newdata=newdata) 
  
  # Add confidence intervals
  smooth_estimates_with_ci <- add_confint(smooth_estimates_m1)
  
  # Filter for the nodeID smooth term
  smooth_nodeID <- smooth_estimates_with_ci %>% 
    filter(grepl("nodeID", .smooth))
  
  # Check if confidence interval includes zero
  smooth_nodeID_significance <- smooth_nodeID %>% 
    mutate(
      significant = .lower_ci > 0 | .upper_ci <  0, 
      tract = tract
    ) %>% 
    select(nodeID, .estimate, significant, tract, .lower_ci, .upper_ci)
  
  # Create summary table at integer nodeID positions
  smooth_nodeID_summary <- smooth_nodeID_significance %>%
    group_by(nodeID) %>%
    summarize(
      .estimate = mean(.estimate),
      .lower_ci = mean(.lower_ci),
      .upper_ci = mean(.upper_ci),
      significant = any(significant),
      .groups = "drop"
    )
   
  # Plot: continuous smooth + confidence band + red points at significant nodes
  p <- ggplot(smooth_nodeID, aes(x = nodeID, y = .estimate)) +
    geom_line() +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(
      data = filter(smooth_nodeID_summary, significant),
      aes(x = nodeID, y = .estimate),
      color = "red",
      size = 2
    ) +
    labs(
      title = paste("Smooth estimate with 95% CI ", 
                    metric, sex, tract),
      x = "nodeID",
      y = "Estimated z"
    ) +
    theme(
      plot.title = element_text(size = 8)  
    )
  
  plot(p)
  
  return(smooth_nodeID_significance)
}