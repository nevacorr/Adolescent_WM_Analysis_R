# This code is from Supplementary Materials for NM Muncy, A Kimbler, 
# AM Hedges-Muncy, DL McMakin, AT Mattfeld. General additive models address 
# statistical issues in diffusion MRI: An example with clinically anxious adolescents. 
# Neuroimage Clin 2022:33:102937. doi: 10.1016/j.nicl.2022.102937. Epub 2022 Jan 5.

plot_spline_diff <- function(gam_model, 
                             tract_data, 
                             tract,
                             factor_a, 
                             factor_b,
                             gam_plot_dir) {
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
  
  # This next line uses itsadug function plot_diff
  p_summary <- capture.output(plot_diff(gam_model,
                                        view = "nodeID",
                                        comp = list(sex = c(factor_a, factor_b)),
                                        rm.ranef = T, 
                                        values = list(nodeID = sort(unique(tract_data$nodeID)))
  ))
  sig_regions <- p_summary[8:length(p_summary)]
  sig_regions <- gsub("\\t", "", sig_regions)
  
  # make list of start and end nodes, for shading
  sig_list <- as.list(strsplit(sig_regions, " - "))
  start_list <- as.numeric(sapply(sig_list, "[[", 1))
  end_list <- as.numeric(sapply(sig_list, "[[", 2))
  
  # determine bottom of plot
  p_est <- plot_diff(gam_model,
                     view = "nodeID",
                     comp = list(sex = c(factor_a, factor_b)),
                     rm.ranef = T,
                     plot = F
  )
  h_min <- min(p_est$est)
  h_ci <- p_est[which(p_est$est == h_min), ]$CI
  min_val <- h_min - h_ci

  # set output
  png(
    filename = paste0(
      gam_plot_dir, "Plot_Diff_", tract, "_pair.png"
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