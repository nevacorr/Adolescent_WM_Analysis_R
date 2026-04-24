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
    effect_size = numeric(),
    ci_low = numeric(),
    ci_high = numeric(),
    stringsAsFactors = FALSE
  )
  
  if (sexflag == 1) {
    results_df$sex_p <- numeric()
    results_df$sex_effect_size <- numeric()
    results_df$sex_ci_low <- numeric()
    results_df$sex_ci_high <- numeric()
  }
  
  # Make an empty dataframe to store t-test p-values for each node for each tract
  node_muncy_pvalues_all <- data.frame(
    Node = integer(), 
    P_value = numeric(), 
    Z_mean = numeric(),
    Tract = character(), 
    stringsAsFactors = FALSE
  )
  
  # -------------------------------
  # LOOP OVER TRACTS
  # -------------------------------
  for (tract in unique_tracts) {
    
    print(paste('modeling tract', tract))
    
    # ==================================================
    # SEX COMPARISON MODEL
    # ==================================================
    if (sexflag == 1) {
      
      model <- tractable_single_tract(
        df = df_z,
        tract = tract,
        target = 'z',
        regressors = c("sex"),
        node_group = "sex", 
        family = scat()
      )
      
      model_summary <- summary(model)
      
      # ---- SEX EFFECT (COEFFICIENT + CI) ----
      beta <- model_summary$p.table["sexM", "Estimate"]
      se   <- model_summary$p.table["sexM", "Std. Error"]
      
      sex_p <- model_summary$p.table["sexM", "Pr(>|t|)"]
      
      sex_ci_low  <- beta - 1.96 * se
      sex_ci_high <- beta + 1.96 * se
      
      # tract-level placeholders (not meaningful here)
      effect_size <- NA
      ci_low <- NA
      ci_high <- NA
      
    }
    
    # ==================================================
    # SINGLE-SEX MODEL
    # ==================================================
    else {
      
      model <- tractable_single_tract(
        df = df_z,
        tract = tract,
        target = 'z', 
        family = scat()
      )
      
      model_summary <- summary(model)
      
      pred <- predict(model, type = "response", se.fit = TRUE)
      
      effect_size <- mean(pred$fit, na.rm = TRUE)
      
      ci_low <- mean(pred$fit - 1.96 * pred$se.fit, na.rm = TRUE)
      ci_high <- mean(pred$fit + 1.96 * pred$se.fit, na.rm = TRUE)
      
      # sex placeholders
      sex_p <- NA
      beta <- NA
      sex_ci_low <- NA
      sex_ci_high <- NA
      
      # calculate node values
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
      
    
    # -------------------------------
    # COMMON METRICS
    # -------------------------------
    intercept_p_value <- model_summary$p.table["(Intercept)", "Pr(>|t|)"]
    
    new_row <- list(
      metric = metric,
      tract = tract,
      intercept_p = sprintf("%.3f", intercept_p_value),
      effect_size = effect_size,
      ci_low = ci_low,
      ci_high = ci_high
    )
    
    # -------------------------------
    # SEX-SPECIFIC FIELDS
    # -------------------------------
    if (sexflag == 1) {
      new_row$sex_p <- sprintf("%.3f", sex_p)
      new_row$sex_effect_size <- beta
      new_row$sex_ci_low <- sex_ci_low
      new_row$sex_ci_high <- sex_ci_high
    }
    
    results_df <- rbind(results_df, new_row)
  }
  
  return(list(
    results_df = results_df,
    node_muncy_pvalues_all = node_muncy_pvalues_all
  ))
}