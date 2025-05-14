library(tractable)

run_tractable_single_tract_model <- function(df_z, 
                                             unique_tracts, 
                                             sexflag, 
                                             metric, 
                                             output_image_path) {
  
  results_df <- data.frame(
    metric = character(),
    tract = character(),
    intercept_p = numeric(),
    stringsAsFactors = FALSE
  )
  
  if (sexflag == 1) {
    results_df$sex_p <- numeric()
  }
  
  df_all_nodes = NULL
  
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
        node_k = 60,
        tract = tract,
        target = 'z'
      )
 
      # Filter for current tract
      tract_data <- df_z[df_z$tractID == tract, ]
      
      # Calculate confidence intervals and significance of every node
      df_all_nodes <- make_spline_single_group_df(model, tract_data, tract, output_image_path)

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
  
  return(list(results_df = results_df, df_all_nodes=df_all_nodes))
}

