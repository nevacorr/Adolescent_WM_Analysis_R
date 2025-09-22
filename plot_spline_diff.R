# This code is modified from Supplementary Materials for NM Muncy, A Kimbler, 
# AM Hedges-Muncy, DL McMakin, AT Mattfeld. General additive models address 
# statistical issues in diffusion MRI: An example with clinically anxious adolescents. 
# Neuroimage Clin 2022:33:102937. doi: 10.1016/j.nicl.2022.102937. Epub 2022 Jan 5.

plot_spline_diff <- function(gam_model, 
                             tract_data, 
                             tract,
                             factor_a, 
                             factor_b,
                             gam_plot_dir, 
                             gam_stats_dir) {
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
  # group_a <- switch_plot_values(factor_a)[[2]][1]
  # group_b <- switch_plot_values(factor_b)[[2]][1]
  
  # determine sig nodes
  
  print('plot spline diff')
  
  pdf(NULL)  # open invisible device because next line produces unwanted plot
  
  # This next line uses itsadug function plot_diff
  p_summary <- capture.output(plot_diff(gam_model,
                                        view = "nodeID",
                                        comp = list(sex = c(factor_a, factor_b)),
                                        rm.ranef = TRUE, 
                                        values = list(nodeID = sort(unique(tract_data$nodeID))))
  )
  dev.off()   # close invisible device
  
  sig_regions <- p_summary[8:length(p_summary)]
  sig_regions <- gsub("\\t", "", sig_regions)
  
  # clean sig_regions
  print(sig_regions)
  print(class(sig_regions))
  print(length(sig_regions))
  
  # Clean sig_regions
  sig_regions_clean <- sig_regions[!is.na(sig_regions) & sig_regions != "Difference is not significant."]
  
  # Handle significant regions robustly
  if(length(sig_regions_clean) > 0) {
    # Split each interval only if it contains " - "
    sig_list <- strsplit(sig_regions_clean, " - ", fixed = TRUE)
    
    # Use safe extraction
    start_list <- as.numeric(sapply(sig_list, `[`, 1))
    end_list   <- as.numeric(sapply(sig_list, function(x) if(length(x) > 1) x[2] else x[1]))
  } else {
    start_list <- numeric(0)
    end_list   <- numeric(0)
  }

  # determine bottom of plot
  p_est <- plot_diff(gam_model,
                     view = "nodeID",
                     comp = list(sex = c(factor_a, factor_b)),
                     rm.ranef = TRUE,
                     plot = FALSE
  )

  h_min <- min(p_est$est)
  h_ci <- p_est[which(p_est$est == h_min), ]$CI
  min_val <- h_min - h_ci

  # create significance table-
  sig_table <- data.frame(
    nodeID = sort(unique(tract_data$nodeID)),
    signif = 0
  )
  
  # mark nodes in each significant region as 1
  for(i in seq_along(start_list)) {
    sig_nodes <- start_list[i]:end_list[i]
    sig_table$signif[sig_table$nodeID %in% sig_nodes] <- 1
  }
  
  # write CSV
  out_file <- file.path(gam_stats_dir, paste0(tract, "_M_vs_F_significance.csv"))
  write.csv(sig_table, out_file, row.names = FALSE)
  
  # Make sure the plot folder exists
  dir.create(file.path(gam_plot_dir, "diff_plots"), showWarnings = FALSE)
  
  
  # make plot
  png(
    filename = file.path(
      gam_plot_dir, "diff_plots", paste0("Plot_Diff_", tract, "_pair.png")
    ),
    width = 600, height = 600
  )
  
  # draw plot
  par(mar = c(5, 5, 4, 2), family = "Times New Roman")
  plot_diff(gam_model,
            view = "nodeID",
            comp = list(sex = c(factor_a, factor_b)),
            rm.ranef = T,
            main = paste0(
              "Difference Scores, ", factor_a, "-", factor_b
            ),
            ylab = "Est. z score difference",
            xlab = "Node",
            cex.lab = 2,
            cex.axis = 2,
            cex.main = 2,
            cex.sub = 1.5,
            col.diff = "red"
  )

  # shade significant regions
  if(length(start_list) > 0 && length(end_list) > 0) {
    for (h_ind in 1:length(start_list)) {
      polygon(
        x = c(rep(start_list[h_ind],2), rep(end_list[h_ind], 2)), 
        y = c(0, min_val, min_val, 0), 
        col = rgb(1, 0, 0, 0.2), 
        border = NA
      )
    }
  }
  
  par(mar = c(5, 4, 4, 2))
  dev.off()
  
  print('finish plot spline diff')
}