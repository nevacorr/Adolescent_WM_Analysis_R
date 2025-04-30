library(tractable)
library(itsadug)
library(mgcv)

run_tractable_single_tract_and_itsadug <- function(df_z, unique_tracts, metric) {
  
  # Create dataframe to store p values for model for tracts
  results_df <- data.frame(
    metric = character(),
    tract = character(),
    intercept_p = numeric(),
    sex_p = numeric(),
    female_estimate = numeric(),
    female_p = numeric(),
    male_estimate = numeric(),
    male_p = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Create dataframe to store p-values for each node and tract
  node_pvalues <- data.frame(
    Node = integer(), 
    P_value = numeric(), 
    Tract = character(),
    sex = character(),
    Metric = character(),
    Z_mean = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop through all tracts to calculate statistics
  for (tract in unique_tracts) {
    print(tract)
    
    # Filter for current tract
    tract_data <- df_z[df_z$tractID == tract, ]
    
    # Convert node column to numeric
    tract_data$nodeID <- as.numeric(tract_data$nodeID)
    
    # Fit the GAM for z scores with node and sex as fixed effects and subject as a random effect
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
    
    # print pvalues and estimated z to console
    cat(sprintf("Female z mean estimate: %.3f (p = %.4f)\n", female_estimate, female_p))
    cat(sprintf("Male z mean estimate: %.3f (p = %.4f)\n\n", male_estimate, male_p))
    
    # Extract p values for entire tract
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
    
    # Calculate node-based statistics
    # Separate data for females and males
    female_data <- tract_data[tract_data$sex == "F", ]
    male_data <- tract_data[tract_data$sex == "M", ]
    
    female_param_table = calculate_node_significance(female_data, tract, metric, "female")
    
    female_node_p_values <- female_param_table$p_value
    female_nodes <- female_param_table$nodeID
    female_mean_z <- female_param_table$Estimate
    
    # Store results for females
    for (i in seq_along(female_node_p_values)) {
      node_pvalues <- rbind(node_pvalues, data.frame(
        Node = as.integer(female_nodes[i]),
        P_value = female_node_p_values[i],
        Tract = tract,
        sex = "F",
        Metric = metric,
        Z_mean = female_mean_z[i]
      ))
    }
    
    male_param_table = calculate_node_significance(male_data, tract, metric, "male")
    
    male_node_p_values <- male_param_table$p_value
    male_nodes <- male_param_table$nodeID
    male_mean_z <- male_param_table$Estimate
    
    # Store results for males
    for (i in seq_along(male_node_p_values)) {
      node_pvalues <- rbind(node_pvalues, data.frame(
        Node = as.integer(male_nodes[i]),
        P_value = male_node_p_values[i],
        Tract = tract,
        sex = "M",
        Metric = metric,
        Z_mean = male_mean_z[i]
      ))
    }
  }
  
    # correct tract statistics for multiple comparisons across tracts
    results_df <- results_df %>%
      mutate(
        female_p_fdr = p.adjust(female_p, method = "fdr"),
        male_p_fdr = p.adjust(male_p, method = "fdr"),
        sex_p_fdr = p.adjust(sex_p, method = "fdr")
      )
    
    # correct within each tract for node-level p-values
    node_pvalues <- node_pvalues %>%
      group_by(Tract) %>%
      mutate(adjusted_p_value = p.adjust(P_value, method = "fdr")) %>%
      ungroup()
  
  return(list(results_df = results_df, node_pvalues = node_pvalues))
  
}

