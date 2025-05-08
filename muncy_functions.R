switch_tract_name <- function(tract) {
  # Switch for decoding AFQ tract names
  #
  # Arguments:
  #   tract = AFQ tract string
  #
  # Returns:
  #   x_tract = str, reformatted tract name
  
  x_tract <- switch(
    tract,
    "UNC_L" = "L. Uncinate",
  )
  return(x_tract)
}



# GAM Functions ------
#
# Functions for calculating GAMs, drawing splines, testing
# for differences between splines, and making dataframes of
# FA values for nodes which differ between groups.

plot_gam_splines <- function(gam_model, tract, df_tract) {
  # Plot splines for a GAM
  #
  # Will plot smoothed splines produced by GAM
  #   by creating a prediction data frame.
  #
  # Arguments:
  #   gam_model = GAM object, produced by gam/bam
  #   tract = AFQ tract name
  #   df_tract = dataframe of AFQ nodes for certain tract
  #
  # Writes:
  #   gam_plot_dir/Plot_GAM_*.png
  
  # generate predictions
  df_pred <- predict.bam(
    gam_model,
    exclude_terms = c("pds", "sex", "subjectID"),
    values = list(pds = NULL, sex = NULL),
    se.fit = T,
    type = "response"
  )
  
  # convert predictions to dataframe
  df_pred <- data.frame(
    Group = df_tract$group,
    sex = df_tract$sex,
    subjectID = df_tract$subjectID,
    pds = df_tract$pds,
    nodeID = df_tract$nodeID,
    fit = df_pred$fit,
    se.fit = df_pred$se.fit
  )
  
  # set up for plot
  h_tract <- switch_tract_name(tract)
  h_title <- paste0("GAM Fit of ", h_tract, " FA Values")
  
  h_cols <- c(
    switch_plot_values("0")[[1]][1],
    switch_plot_values("1")[[1]][1],
    switch_plot_values("2")[[1]][1]
  )
  names(h_cols) <- c("0", "1", "2")
  h_breaks <- c("0", "1", "2")
  h_labels <- c(
    switch_plot_values("0")[[2]][1],
    switch_plot_values("1")[[2]][1],
    switch_plot_values("2")[[2]][1]
  )
  
  # draw plot
  p <- ggplot(data = df_pred) +
    geom_smooth(mapping = aes(x = nodeID, y = fit, color = Group)) +
    ggtitle(h_title) +
    ylab("Fit FA") +
    xlab("Tract Node") +
    theme(text = element_text(
      family = "Times New Roman", face = "bold", size = 14
    ))
  
  p + scale_color_manual(
    values = h_cols,
    breaks = h_breaks,
    labels = h_labels
  )
  
  ggsave(
    paste0(gam_plot_dir, "Plot_GAM_", tract, "_G3.png"),
    units = "in",
    width = 6,
    height = 6,
    device = "png"
  )
}

browser()

calc_gam_stats <- function(tract, df_tract) {
  
  # Calculate GAM for tract node FA values
  #
  # This function has a series of steps:
  #   1) Use distribution to determine GAM family
  #       Gamma and beta are used for the tract
  #   2) Build the GAM, test if basis function is appropriate
  #       It was determined that k=40 worked for each tract
  #   3) Compare different GAMs to find best fit
  #   4) Add covariates, repeat 2-3
  #
  # Arguments:
  #   tract = AFQ tract name
  #   df_tract = dataframe of node FAs for single tract
  #
  # Writes:
  #   gam_stats_dir/Stats_GAM-*.txt
  #
  # Returns:
  #   fit_cov_pds = preferred GAM object for each tract   
  #
  # Notes: 
  #   1) Given that k=40 was appropriate for each tract, and the covariate
  #   model better fit the non-covariate model, only gam_cov_pds is
  #   plotted for each tract.
  
  # plot mean data 
  print(
    ggplot(data = df_tract) +
      geom_smooth(mapping = aes(x=nodeID, y=dti_fa, color=group))
  )
  browser()
  
  # plot mean data with original data points as well
  print(
    ggplot(data = df_tract) +
      geom_point(mapping = aes(x=nodeID, y=dti_fa, color=group), size=0.3) +
      geom_smooth(mapping = aes(x=nodeID, y=dti_fa, color=group))
  )
  browser()
  
  # determine distribution - produces Cullen and Frey graph
  # This graph shows which all possible distributions and how
  # our data compares to them. 
  descdist(df_tract$dti_fa, discrete=F) # beta or gamma dist
  
  browser()
  
  # This is an exploratory step to assess if the beta distribution
  # fits the data well, ignoring predictors like group,sex and nodeID.
  # It gives a value of AIC that can be compared to the gamma model.
  fit.beta <- fitdist(df_tract$dti_fa, "beta")
  plot(fit.beta)
  fit.beta$aic
  
  browser()
  # This is an exploratory step to assess if the beta distribution
  # fits the data well, ignoring predictors like group,sex and nodeID.
  # It gives a value of AIC that can bee compared to the beta model.
  fit.gamma <- fitdist(df_tract$dti_fa, "gamma")
  plot(fit.gamma)
  fit.gamma$aic
  
  browser()
  
  #  write gam with gamma family
  fit_gamma <- bam(dti_fa ~ group +
                     sex + #control for sex
                     s(nodeID, by = group, k = 40) +
                     s(subjectID, bs = "re"),
                   data = df_tract,
                   family = Gamma(link = "logit"), # logit function used when data has bounded values
                   method = "REML" #method to assess model fit 
  )
  
  browser()
  
  # determine k
  gam.check(fit_gamma, rep = 500) # k = 40 works well
  
  browser()
  
  # Save model summary to file
  capture.output(
    summary(fit_gamma),
    file = paste0(
      gam_stats_dir, "Stats_GAM-gamma_", tract, "_G3.txt"
    )
  )
  
  browser()
  
  # write gam with beta family
  fit_beta <- bam(dti_fa ~ group +
                    sex +
                    s(nodeID, by = group, k = 40) +
                    s(subjectID, bs = "re"),
                  data = df_tract,
                  family = betar(link = "logit"),
                  method = "REML"
  )
  
  browser()
  
  gam.check(fit_beta, rep = 500) # k = 40 works well
  
  # Write model summary to file
  capture.output(
    summary(fit_beta),
    file = paste0(
      gam_stats_dir, "Stats_GAM-beta_", tract, "_G3.txt"
    )
  )
  
  browser()
  
  # determine which model to keep
  # Compare the model and write the summaries to file
  capture.output(
    compareML(fit_gamma, fit_beta),
    file = paste0(
      gam_stats_dir,
      "Stats_GAM-comp_gam-beta_",
      tract, "_G3.txt"
    )
  )
  
  browser()
  
  # After choosing model, add in covariates of interest
  # to model
  fit_cov_pds <- bam(dti_fa ~ group +
                       sex +
                       s(nodeID, by = group, k = 40) +
                       s(pds, by = sex) +
                       s(subjectID, bs = "re"),
                     data = df_tract,
                     family = Gamma(link = "logit"),
                     method = "REML"
  )
  
  browser()
  
  # Check that the k values makes sense
  # determine k
  gam.check(fit_cov_pds, rep = 500) # again, k=40 works well
  
  # Write model summary to file
  capture.output(
    summary(fit_cov_pds),
    file = paste0(
      gam_stats_dir,
      "Stats_GAM-cov_",
      tract, "_G3.txt"
    )
  )
  
  browser()
  
  # compare model with covariate to model without covariate to 
  # see if adding the covariate improved the model fit
  # test cov model against non-covariate
  capture.output(
    compareML(fit_gamma, fit_cov_pds),
    file = paste0(
      gam_stats_dir,
      "Stats_GAM-comp_gam-cov_",
      tract, "_G3.txt"
    )
  )
  
  return(fit_cov_pds)
}

plot_spline_diff <- function(gam_model, 
                             tract, 
                             factor_a, 
                             factor_b) {
  # Draw a spline-difference plot for 2 splines
  #
  # This will make plots and write tables of sig
  # node differences for GAM splines bx 2 factors (groups)
  #
  # Arguments:
  #   gam_model = GAM object, produced by gam/bam
  #   tract = AFQ tract name
  #   factor_a = group factor (0-2), string
  #   factor_b = group factor (0-2), string
  #
  # Writes:
  #   gam_plot_dir/Plot_Diff_*_pair.png
  #   table_dir/Table_Diff_*.txt
  
  # setup for plotting
  group_a <- switch_plot_values(factor_a)[[2]][1]
  group_b <- switch_plot_values(factor_b)[[2]][1]
  
  # determine sig nodes
  p_summary <- capture.output(plot_diff(gam_model,
                                        view = "nodeID",
                                        comp = list(group = c(factor_a, factor_b)),
                                        rm.ranef = T
  ))
  sig_regions <- p_summary[10:length(p_summary)]
  sig_regions <- gsub("\\t", "", sig_regions)
  
  # make list of start and end nodes, for shading
  sig_list <- as.list(strsplit(sig_regions, " - "))
  start_list <- as.numeric(sapply(sig_list, "[[", 1))
  end_list <- as.numeric(sapply(sig_list, "[[", 2))
  
  # determine bottom of plot
  p_est <- plot_diff(gam_model,
                     view = "nodeID",
                     comp = list(group = c(factor_a, factor_b)),
                     rm.ranef = T,
                     plot = F
  )
  h_min <- min(p_est$est)
  h_ci <- p_est[which(p_est$est == h_min), ]$CI
  min_val <- h_min - h_ci
  
  # set output
  png(
    filename = paste0(
      gam_plot_dir, "Plot_Diff_", tract, "_G3_pair.png"
    ),
    width = 600, height = 600
  )
  
  # draw plot
  par(mar = c(5, 5, 4, 2), family = "Times New Roman")
  plot_diff(gam_model,
            view = "nodeID",
            comp = list(group = c(factor_a, factor_b)),
            rm.ranef = T,
            main = paste0(
              "Difference Scores, ", group_a, "-", group_b
            ),
            ylab = "Est. FA difference",
            xlab = "Tract Node",
            cex.lab = 2,
            cex.axis = 2,
            cex.main = 2,
            cex.sub = 1.5,
            col.diff = "red"
  )
  
  # shade significant regions
  for (h_ind in 1:length(start_list)) {
    polygon(
      x = c(rep(start_list[h_ind],2), rep(end_list[h_ind], 2)), 
      y = c(0, min_val, min_val, 0), 
      col = rgb(1, 0, 0, 0.2), 
      border = NA
    )
  }
  
  par(mar = c(5, 4, 4, 2))
  dev.off()
}


make_spline_diff_df <- function(gam_model, factor_a, factor_b) {
  # Make a dataframe of fit differences when testing 2 splines
  #
  # Get difference values from testing whether spline A
  # differs from spline B at each node.
  #
  # Arguments:
  #   gam_model = GAM object, produced by gam/bam
  #   factor_a = group factor (0-2)
  #   factor_b = group factor (0-2)
  #
  # Returns:
  #   df_pair = dataframe of differences
  
  # determine predicted differences
  df_pair <- plot_diff(gam_model,
                       view = "nodeID",
                       comp = list(group = c(factor_a, factor_b)),
                       rm.ranef = T,
                       plot = F
  )
  
  # add Comparison column to df
  colnames(df_pair) <- c(colnames(df_pair[, 1:4]), "Comp")
  df_pair$Comp <- paste0(factor_a, factor_b)
  return(df_pair)
}


calc_spline_diff <- function(gam_model, 
                             tract, 
                             comp_list) {
  # Investigate whether differences exist between spline fit FAs
  #
  # This function does a number of things:
  #   1) Makes plots and tables
  #   2) Make dataframes of difference estimates
  #   3) Determine nodes that differ in difference estimation 
  #       for average. Repeats what is done in tables.
  #       Also gets node of maximum difference.
  #
  # Arguments:
  #   gam_model = GAM object, produced by gam/bam
  #   tract = AFQ tract name
  #   comp_list = list of 2 factors for pairwise comparison
  #
  # Returns:
  #   diff_list, node_max = indexed list of nodes which differed
  #     [[1]] = nodes which differed, [[2]] = node of maximum difference
  
  browser()
  
  # make plots and tables
  plot_spline_diff(gam_model, tract, comp_list[1], comp_list[2])
  
  browser()
  
  # get plot_diff data frames
  df_est_diff <- make_spline_diff_df(gam_model, comp_list[1], comp_list[2])
  
  browser()
  
  # determine where nodes differ
  node_list <- unique(df_est_diff$nodeID)
  diff_list <- vector()
  
  browser()
  
  for (node in node_list) {
    ind_node <- which(df_est_diff$nodeID == node)
    h_est <- abs(df_est_diff[ind_node[1], ]$est)
    h_ci <- df_est_diff[ind_node[1], ]$CI
    if ((h_est - h_ci) > 0) {
      diff_list <- c(diff_list, node)
    }
  }
  
  browser()
  
  # find node of max difference
  h_df <- subset(df_est_diff, nodeID %in% diff_list)
  ind_max <- which(abs(h_df$est) == max(abs(h_df$est)))
  node_max <- h_df[ind_max, ]$nodeID
  
  browser()
  
  return(list(diff_list, node_max))
}

