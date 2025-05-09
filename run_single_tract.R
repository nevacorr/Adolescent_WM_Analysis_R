
run_single_tract <- function(df_z, unique_tracts, metric) {
  
  wd="/Users/nevao/R_Projects/AdolWMAnalysis"
  out_dir <- paste0(wd, '/output/')
  gam_plot_dir <- paste0(out_dir, "plots_gam/")
  gam_stats_dir <- paste0(out_dir, "stats_gam/")
  table_dir <- paste0(out_dir, "tables/")
  
  # Loop through all tracts to calculate statistics
  for (tract in unique_tracts) {
    print(tract)

    # Set sex as a factor
    df_z$sex <- factor(df_z$sex)
    
    # Filter for current tract
    tract_data <- df_z[df_z$tractID == tract, ]

    gam_file <- paste0(out_dir, "gam_", tract, ".Rda")
    if (!file.exists(gam_file)) {
      gam_model <- calc_gam_stats(tract_data, tract, gam_stats_dir)
      saveRDS(gam_model, file = gam_file)
      rm(gam_model)
      }
      
    gam_model <- readRDS(gam_file)
    
    # Plot the model for this tract
    plot_gam_splines(gam_model, tract_data, tract, gam_plot_dir)
    
  }
  
  
  #   
  #   # Fit the GAM for z scores with node and sex as fixed effects and subject as a random effect
  #   model <- gam(z ~ sex + s(nodeID, by = sex, bs = "fs", k = 10) + s(subjectID, bs = "re"),
  #                data = tract_data,
  #                method = "REML")
  # 
  #   model_summary = summary(model)
  #   print(model_summary)
  #   
  #   # For each sex, determine whether z scores across entire tract are significantly
  #   # different from zero
  #   female_estimate <- coef(model)["(Intercept)"]
  #   female_se <- sqrt(vcov(model)["(Intercept)", "(Intercept)"])
  #   female_p <- 2 * (1 - pnorm(abs(female_estimate / female_se)))
  #   
  #   male_estimate <- coef(model)["(Intercept)"] + coef(model)["sexM"]
  #   male_se <- sqrt(sum(vcov(model)[c("(Intercept)", "sexM"), c("(Intercept)", "sexM")]))
  #   male_p <- 2 * (1 - pnorm(abs(male_estimate / male_se)))
  #   
  #   # print pvalues and estimated z to console
  #   cat(sprintf("Female z mean estimate: %.3f (p = %.4f)\n", female_estimate, female_p))
  #   cat(sprintf("Male z mean estimate: %.3f (p = %.4f)\n\n", male_estimate, male_p))
  #   
  #   # Extract p values for entire tract
  #   intercept_p_value = model_summary$p.table["(Intercept)", "Pr(>|t|)"]
  #   sex_p_value = model_summary$p.table["sexM", "Pr(>|t|)"]
  #   
  #   # Store these statistics in a dataframe
  #   results_df <- rbind(results_df, data.frame(
  #     metric = metric,
  #     tract = tract,
  #     intercept_p = round(intercept_p_value, 3),
  #     sex_p = round(sex_p_value, 3),
  #     female_estimate = round(female_estimate, 3),
  #     female_p = round(female_p, 4),
  #     male_estimate = round(male_estimate, 3),
  #     male_p = round(male_p, 4)
  #   ))
  #   
  #   # Calculate node-based statistics
  #   # Separate data for females and males
  #   female_data <- tract_data[tract_data$sex == "F", ]
  #   male_data <- tract_data[tract_data$sex == "M", ]
  #   
  #   female_param_table = calculate_node_significance(female_data, tract, metric, "female")
  #   
  #   female_node_p_values <- female_param_table$p_value
  #   female_nodes <- female_param_table$nodeID
  #   female_mean_z <- female_param_table$Estimate
  #   
  #   # Store results for females
  #   for (i in seq_along(female_node_p_values)) {
  #     node_pvalues <- rbind(node_pvalues, data.frame(
  #       Node = as.integer(female_nodes[i]),
  #       P_value = female_node_p_values[i],
  #       Z_mean = female_mean_z[i],
  #       Tract = tract,
  #       sex = "F",
  #       Metric = metric
  #     ))
  #   }
  #   
  #   male_param_table = calculate_node_significance(male_data, tract, metric, "male")
  #   
  #   male_node_p_values <- male_param_table$p_value
  #   male_nodes <- male_param_table$nodeID
  #   male_mean_z <- male_param_table$Estimate
  #   
  #   # Store results for males
  #   for (i in seq_along(male_node_p_values)) {
  #     node_pvalues <- rbind(node_pvalues, data.frame(
  #       Node = as.integer(male_nodes[i]),
  #       P_value = male_node_p_values[i],
  #       Z_mean = male_mean_z[i],
  #       Tract = tract,
  #       sex = "M",
  #       Metric = metric
  #     ))
  #   }
  # }
  # 
  #   # correct tract statistics for multiple comparisons across tracts
  #   results_df <- results_df %>%
  #     mutate(
  #       female_p_fdr = p.adjust(female_p, method = "fdr"),
  #       male_p_fdr = p.adjust(male_p, method = "fdr"),
  #       sex_p_fdr = p.adjust(sex_p, method = "fdr")
  #     )
  #   
  # 
  #   # correct within each tract for node-level p-values
  #   node_pvalues <- node_pvalues %>%
  #     group_by(Node) %>%
  #     mutate(adjusted_p_value = p.adjust(P_value, method = "fdr")) %>%
  #     ungroup()
  # 
  return(list(results_df = results_df, node_pvalues = node_pvalues))
  
}

