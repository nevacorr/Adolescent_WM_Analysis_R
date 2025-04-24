library(tractable)
library(itsadug)
library(mgcv)

run_tractable_single_tract_and_itsadug <- function(df_z, unique_tracts, metric) {
  
  # Dataframe to store p values for model for tracts
  results_df <- data.frame(
    metric = character(),
    tract = character(),
    intercept_p = numeric(),
    sex_p = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Dataframe to store p-values for each node and tract
  node_pvalues <- data.frame(
    Node = integer(), 
    P_value = numeric(), 
    Fit = numeric(),
    SE = numeric(),
    Tract = character(),
    Sex = character(),
    stringsAsFactors = FALSE
  )
  
  for (tract in unique_tracts) {
    # Filter for current tract
    tract_data <- df_z[df_z$tractID == tract, ]
    tract_data$nodeID <- as.numeric(tract_data$nodeID)
    
    # Fit the GAM
    model <- gam(z ~ sex + s(nodeID, by = sex, bs = "fs", k = 10) + s(subjectID, bs = "re"),
                 data = tract_data,
                 method = "REML")

    model_summary = summary(model)
    
    # Calculate p values for entire tract
    intercept_p_value = model_summary$p.table["(Intercept)", "Pr(>|t|)"]
    sex_p_value = model_summary$p.table["sexM", "Pr(>|t|)"]
    
    # Store these statistics in a dataframe
    results_df <- rbind(results_df, data.frame(
      metric = metric,
      tract = tract,
      intercept_p = round(intercept_p_value, 3),
      sex_p = round(sex_p_value, 3)
    ))
    
    # Predict on full tract data
    pred <- predict(model, newdata = tract_data, type = "response", se.fit = TRUE)
    
    # Calculate p-values across all rows
    p_vals <- 2 * (1 - pnorm(abs(pred$fit / pred$se.fit)))
    
    # Combine predictions with input data (so you keep sex + nodeID info)
    fs_data <- data.frame(
      Node = tract_data$nodeID,
      Fit = pred$fit,
      SE = pred$se.fit,
      P_value = p_vals,
      Tract = tract,
      Sex = tract_data$sex
    )
    
    # Append to full results
    node_pvalues <- rbind(node_pvalues, fs_data)
  }
  
  # Apply FDR correction per tract and separately for each sex
  node_pvalues <- node_pvalues %>%
    group_by(Tract, Sex) %>%
    mutate(adjusted_p = p.adjust(P_value, method = "fdr")) %>%
    ungroup()
  
  return(list(results_df = results_df, node_pvalues = node_pvalues))
  
}

