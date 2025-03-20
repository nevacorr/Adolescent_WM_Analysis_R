library(tractable)

run_tractable_single_tract_model <- function(df_z, unique_tracts, sexflag, metric) {
  
  results_df <- data.frame(
    metric = character(),
    tract = character(),
    intercept_p = numeric(),
    stringsAsFactors = FALSE
  )
  
  if (sexflag == 1) {
    results_df$sex_p <- numeric()
  }
  
  # Initialize an empty dataframe to store p-values for each node and tract
  node_pvalues <- data.frame(Node = integer(), P_value = numeric(), 
                             T_statistic = numeric(), Z_mean = numeric(),
                             Tract = character(), stringsAsFactors = FALSE)
  
  for (tract in unique_tracts) {
    # Fit the model
    # If comparing across sexes
    if (sexflag == 1) {
      model <-  tractable_single_tract(
        df = df_z,
        tract = tract,
        target = 'z',
        regressors = c("sex"),
        node_group = "sex"
      )
    } else {
      # If looking at each sex separately (df_z only has data for one sex)
      model <-  tractable_single_tract(
        df = df_z,
        tract = tract,
        target = 'z'
      )
 
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
    }
    
    model_summary = summary(model)
  
    intercept_p_value = model_summary$p.table["(Intercept)", "Pr(>|t|)"]
    
    # Construct result row
    new_row <- list(
      metric = metric, 
      tract = tract,
      intercept_p = sprintf("%.3f", intercept_p_value)
    )

    if (sexflag == 1) {
      sex_p_value = model_summary$p.table["sexM", "Pr(>|t|)"]
      new_row$sex_p <- sprintf("%.3f",sex_p_value)
    }
    
    # Append new row to dataframe
    results_df <- rbind(results_df, new_row)
    
  }
  
  if (sexflag == 0) {
    
    # Apply FDR correction to the p-values
    node_pvalues$adjusted_p_value <- p.adjust(node_pvalues$P_value, method = "fdr")
    
  }
  
  return(list(results_df = results_df, node_pvalues = node_pvalues))
}

