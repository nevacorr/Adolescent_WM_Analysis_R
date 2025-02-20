
run_tractable_single_tract_model <- function(df_z, unique_tracts, sexflag) {
  
  results_df <- data.frame(
    metric = character(),
    tract = character(),
    intercept_p = numeric(),
    sex_p = numeric(),
    R_sq = numeric(),
    deviance_explained = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (tract in unique_tracts) {
    # Fit the model
    if (sexflag == 1) {
      model <-  tractable_single_tract(
        df = df_z,
        tract = tract,
        target = 'z',
        regressors = c("sex"),
        node_group = "sex"
    } else {
    model <-  tractable_single_tract(
      df = df_z,
      tract = tract,
      target = 'z',
      regressors = c("sex"),
      node_group = "sex"
   }
    
    model_summary = summary(model)
    
    sex_p_value = model_summary$p.table["sexM", "Pr(>|t|)"]
    intercept_p_value = model_summary$p.table["(Intercept)", "Pr(>|t|)"]
    R_sq = model_summary$r.sq
    dev_exp = model_summary$dev.expl
    
    # Put results in dataframe
    results_df <- rbind(results_df, data.frame(
      metric = metric,
      tract = tract,
      intercept_p = sprintf("%.3f", intercept_p_value),
      sex_p = sprintf("%.3f",sex_p_value),
      R_sq = sprintf("%.3f",R_sq),
      deviance_explained = sprintf("%.3f",dev_exp)
    ))
    
  }
}

