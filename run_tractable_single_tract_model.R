library(tractable)

run_tractable_single_tract_model <- function(df_z, unique_tracts, sexflag) {
  
  results_df <- data.frame(
    metric = character(),
    tract = character(),
    intercept_p = numeric(),
    R_sq = numeric(),
    deviance_explained = numeric(),
    stringsAsFactors = FALSE
  )
  
  if (sexflag == 1) {
    results_df$sex_p <- numeric()
  }
  
  for (tract in unique_tracts) {
    # Fit the model
    if (sexflag == 1) {
      model <-  tractable_single_tract(
        df = df_z,
        tract = tract,
        target = 'z',
        regressors = c("sex"),
        node_group = "sex"
      )
    } else {
    model <-  tractable_single_tract(
      df = df_z,
      tract = tract,
      target = 'z',
    )
   }
    
    model_summary = summary(model)
  
    intercept_p_value = model_summary$p.table["(Intercept)", "Pr(>|t|)"]
    R_sq = model_summary$r.sq
    dev_exp = model_summary$dev.expl
    
    # Construct result row
    new_row <- list(
      metric = metric, 
      tract = tract,
      intercept_p = sprintf("%.3f", intercept_p_value),
      R_sq = sprintf("%.3f",R_sq),
      deviance_explained = sprintf("%.3f",dev_exp)
    )

  if (sexflag == 1) {
    sex_p_value = model_summary$p.table["sexM", "Pr(>|t|)"]
    new_row$sex_p <- sprintf("%.3f",sex_p_value)
  }
    
  # Append new row to dataframe
  results_df <- rbind(results_df, as.data.frame(new_row))
  }
  
  return(results_df)
}

