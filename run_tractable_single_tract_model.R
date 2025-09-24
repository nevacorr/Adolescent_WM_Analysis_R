run_tractable_single_tract_model <- function(df_z_both_sexes, df_z, 
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
  node_muncy_pvalues_all <- data.frame(
    Node = integer(), 
    P_value = numeric(), 
    Z_mean = numeric(),
    Tract = character(), 
    stringsAsFactors = FALSE)
  
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
      
      node_output <- run_single_tract(df_z_both_sexes, df_z, tract, metric, sex_str)
      
      # Compute mean z for each node for this tract
      df_z_mean <- df_z %>%
        filter(tractID == tract) %>%      # keep only this tract
        group_by(nodeID) %>%              # group by node
        summarise(Z_mean = mean(z, na.rm = TRUE)) %>%
        ungroup()
      
      # Combine with p-values
      node_muncy_pvalues <- df_z_mean %>%
        left_join(node_output %>% select(nodeID, pvalue), by = "nodeID") %>%
        rename(Node = nodeID,
               P_value = pvalue) %>%
        mutate(tract = tract)            # add tract column
      
      node_muncy_pvalues_all <- rbind(node_muncy_pvalues_all, node_muncy_pvalues )
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
    node_muncy_pvalues_all=node_muncy_pvalues_all))
}

