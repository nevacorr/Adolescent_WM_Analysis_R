run_tractable_single_tract_model <- function(df_z, 
                                             unique_tracts, 
                                             sexflag, 
                                             metric, 
                                             output_image_path,
                                             sex_str) {
  
  # Make an empty dataframe to store tract-level statistics
  results_df <- data.frame(
    metric = character(),
    tract = character(),
    intercept_p = numeric(),
    stringsAsFactors = FALSE
  )
  
  if (sexflag == 1) {
    results_df$sex_p <- numeric()
  }
  
  # Make an empty dataframe to store t-test p-values for each node for each tract
  if (sexflag == 0) {
    node_ttest_pvalues <- data.frame(
    Node = integer(), 
    P_value = numeric(), 
    adjusted_p_value = numeric(),
    Z_mean = numeric(),
    Tract = character(), 
    stringsAsFactors = FALSE)
  } 
  else if (sexflag == 1) {
    node_ttest_pvalues <- data.frame(
    Node = integer(), 
    P_value = numeric(), 
    adjusted_p_value = numeric(),
    Z_mean_M = numeric(),
    Z_mean_F = numeric(),
    Tract = character(), 
    stringsAsFactors = FALSE)
  }
  
  # Create dataframe to store confidence intervals for all nodes from all tracts
  ci_all_nodes_all_tracts <- data.frame()
  
  for (tract in unique_tracts) {
    
    print(paste('modeling tract', tract))
    
    # Fit the model
    # If comparing across sexes
    if (sexflag == 1) {
      model <-  tractable_single_tract(
        df = df_z,
        tract = tract,
        target = 'z',
        regressors = c("sex"),
        node_group = "sex", 
        family = scat()
      )
      
      node_pvalues <- compute_t_scores_for_nodes_by_tract_sex_diff(df_z, tract, metric)
      # Apply FDR correction to the node-level p-values for this tract
      node_pvalues$adjusted_p_value <- p.adjust(node_pvalues$P_value, method = "fdr")
      node_ttest_pvalues <- rbind(node_ttest_pvalues, node_pvalues)
      
      # Filter for current tract
      tract_data <- df_z[df_z$tractID == tract, ]      
      
    } else {
      # If looking at each sex separately (df_z only has data for one sex)
      model <-  tractable_single_tract(
        df = df_z,
        tract = tract,
        target = 'z', 
        family = scat()
      )
      
      # check k
      # gam.check(model, rep = 500)
      
      node_pvalues <- compute_t_scores_for_nodes_by_tract(df_z, tract)
      # Apply FDR correction to the node-level p-values for this tract
      node_pvalues$adjusted_p_value <- p.adjust(node_pvalues$P_value, method = "fdr")
      node_ttest_pvalues <- rbind(node_ttest_pvalues, node_pvalues )
      
      # Filter for current tract
      tract_data <- df_z[df_z$tractID == tract, ]
      
      # Calculate confidence intervals and significance of every node
      # using itsadug 
      ci_single <- make_spline_single_group_df(model, tract_data, tract, output_image_path, sex_str)
      ci_single$tract <- tract
      
      ci_all_nodes_all_tracts <- rbind(ci_all_nodes_all_tracts, ci_single)
    }
    
    model_summary = summary(model)
    
    intercept_p_value = model_summary$p.table["(Intercept)", "Pr(>|t|)"]
    
    # Construct result row for tract level statistics
    new_row <- list(
      metric = metric, 
      tract = tract,
      intercept_p = sprintf("%.3f", intercept_p_value)
    )
    
    # If comparing sexes, add row for statistics for sex difference
    if (sexflag == 1) {
      sex_p_value = model_summary$p.table["sexM", "Pr(>|t|)"]
      new_row$sex_p <- sprintf("%.3f",sex_p_value)
    }
    
    # Append new row to tract-level statistics dataframe
    results_df <- rbind(results_df, new_row)
  
  }
  
  return(list(
    results_df=results_df, 
    ci_all_nodes_all_tracts=ci_all_nodes_all_tracts, 
    node_ttest_pvalues=node_ttest_pvalues))
}