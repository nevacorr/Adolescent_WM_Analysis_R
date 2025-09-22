
# This code is modified from Supplementary Materials for NM Muncy, A Kimbler, 
# AM Hedges-Muncy, DL McMakin, AT Mattfeld. General additive models address 
# statistical issues in diffusion MRI: An example with clinically anxious adolescents. 
# Neuroimage Clin 2022:33:102937. doi: 10.1016/j.nicl.2022.102937. Epub 2022 Jan 5.

calc_spline_diff <- function(gam_model, 
                             tract_data, 
                             tract, 
                             comp_list,
                             gam_plot_dir, 
                             gam_stats_dir) {
  # Investigate whether differences exist between spline fit 
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
  
  # make plots and tables
  
  plot_spline_diff(gam_model, tract_data, tract, comp_list[1], comp_list[2], 
                   gam_plot_dir, gam_stats_dir)
  
  df_m <- make_spline_single_group_df(gam_model, tract_data, tract, 
                                      gam_plot_dir, gam_stats_dir, "M")
  df_f <- make_spline_single_group_df(gam_model, tract_data, tract, 
                                      gam_plot_dir, gam_stats_dir, "F")
  
  df_all <- rbind(df_m, df_f)
  
  # get plot_diff data frames
  df_est_diff <- make_spline_diff_df(gam_model, tract_data, tract, 
                                     comp_list[1], comp_list[2])
  
  # determine where nodes differ
  node_list <- unique(df_est_diff$nodeID)
  diff_list <- vector()
  
  for (node in node_list) {
    ind_node <- which(df_est_diff$nodeID == node)
    h_est <- abs(df_est_diff[ind_node[1], ]$est)
    h_ci <- df_est_diff[ind_node[1], ]$CI
    if ((h_est - h_ci) > 0) {
      diff_list <- c(diff_list, node)
    }
  }
  
  # find node of max difference
  h_df <- subset(df_est_diff, nodeID %in% diff_list)
  ind_max <- which(abs(h_df$est) == max(abs(h_df$est)))
  node_max <- h_df[ind_max, ]$nodeID
  
  return(list(diff_list, node_max))
}

