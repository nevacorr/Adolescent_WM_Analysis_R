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
    intercept_p_value = model_summary$p.table["(Intercept)", "Pr(>|t|)"]
    sex_p_value = model_summary$p.table["sexM", "Pr(>|t|)"]
    
    results_df <- rbind(results_df, data.frame(
      metric = metric,
      tract = tract,
      intercept_p = round(intercept_p_value, 3),
      sex_p = round(sex_p_value, 3)
    ))
    
    print(model_summary)
    
    for (sex_value in c("M", "F")) {
      fs <- plot_smooth(model,
                        view = "nodeID",
                        cond = list(sex = sex_value),
                        rm.ranef = TRUE,
                        return.fit = TRUE)
      
      # Get fitted values and standard errors using predict()
      pred <- predict(model, type = "response", se.fit = TRUE)
      
      # Ensure all data have the same length and nodeID is in the same order as the data
      fs_data <- data.frame(
        nodeID = tract_data$nodeID,  # Use original nodeID from tract_data
        fit = pred$fit,  # Get the fitted values from the model
        se.fit = pred$se.fit,  # Get the standard errors from the model
        tract = tract,
        sex = sex_value
      )
      
      # Check if lengths match before binding
      if (length(fs_data$nodeID) == length(fs_data$fit) && length(fs_data$nodeID) == length(fs_data$se.fit)) {
        node_pvalues <- rbind(node_pvalues, fs_data)
      } else {
        warning("Mismatch in lengths of nodeID, fit, and se.fit. Skipping this iteration.")
      }
      fs$p_value <- 2 * (1 - pnorm(abs(fs$fit / fs$se.fit)))
      
      node_pvalues <- rbind(node_pvalues, fs)
    }
  }
  
  # Apply FDR correction per tract and separately for each sex
  node_pvalues <- node_pvalues %>%
    group_by(Tract, Sex) %>%
    mutate(adjusted_p = p.adjust(P_value, method = "fdr")) %>%
    ungroup()
  
  return(list(results_df = results_df, node_pvalues = node_pvalues))
  
}

