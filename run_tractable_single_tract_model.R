library(tractable)

run_tractable_single_tract_model <- function(df_z, unique_tracts, sexflag) {
  
  results_df <- data.frame(
    metric = character(),
    tract = character(),
    intercept_p = numeric(),
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
    
    print(tract)
    print(model_summary)
    
    # Append new row to dataframe
    results_df <- rbind(results_df, new_row)
    
  }
  
  return(results_df)
}

