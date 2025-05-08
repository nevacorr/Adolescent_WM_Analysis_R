# This code is from Supplementary Materials for NM Muncy, A Kimbler, 
# AM Hedges-Muncy, DL McMakin, AT Mattfeld. General additive models address 
# statistical issues in diffusion MRI: An example with clinically anxious adolescents. 
# Neuroimage Clin 2022:33:102937. doi: 10.1016/j.nicl.2022.102937. Epub 2022 Jan 5.

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