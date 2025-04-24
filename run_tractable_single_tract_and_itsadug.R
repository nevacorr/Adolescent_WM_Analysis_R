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
    print(tract)
    tract_data <- df_z[df_z$tractID == tract, ]
    tract_data$nodeID <- as.numeric(tract_data$nodeID)
    
    # Fit the GAM
    model <- gam(z ~ sex + s(nodeID, by = sex, bs = "fs", k = 10) + s(subjectID, bs = "re"),
                 data = tract_data,
                 method = "REML")

    model_summary = summary(model)
    
    print(model_summary)
    
    # For each sex, determine whether z scores across entire tract are significantly
    # different from zero
    female_estimate <- coef(model)["(Intercept)"]
    female_se <- sqrt(vcov(model)["(Intercept)", "(Intercept)"])
    female_p <- 2 * (1 - pnorm(abs(female_estimate / female_se)))
    
    male_estimate <- coef(model)["(Intercept)"] + coef(model)["sexM"]
    male_se <- sqrt(sum(vcov(model)[c("(Intercept)", "sexM"), c("(Intercept)", "sexM")]))
    male_p <- 2 * (1 - pnorm(abs(male_estimate / male_se)))
    
    cat(sprintf("Female z mean estimate: %.3f (p = %.4f)\n", female_estimate, female_p))
    cat(sprintf("Male z mean estimate: %.3f (p = %.4f)\n\n", male_estimate, male_p))
    
    # Calculate p values for entire tract
    intercept_p_value = model_summary$p.table["(Intercept)", "Pr(>|t|)"]
    sex_p_value = model_summary$p.table["sexM", "Pr(>|t|)"]
    
    # Store these statistics in a dataframe
    results_df <- rbind(results_df, data.frame(
      metric = metric,
      tract = tract,
      intercept_p = round(intercept_p_value, 3),
      sex_p = round(sex_p_value, 3),
      female_estimate = round(female_estimate, 3),
      female_p = round(female_p, 4),
      male_estimate = round(male_estimate, 3),
      male_p = round(male_p, 4)
    ))
    
    results_df <- results_df %>%
      mutate(
        female_p_fdr = p.adjust(female_p, method = "fdr"),
        male_p_fdr = p.adjust(male_p, method = "fdr"),
        sex_p_fdr = p.adjust(sex_p, method = "fdr")
      )
    
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

