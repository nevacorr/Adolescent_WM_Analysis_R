# This code is from Supplementary Materials for NM Muncy, A Kimbler, 
# AM Hedges-Muncy, DL McMakin, AT Mattfeld. General additive models address 
# statistical issues in diffusion MRI: An example with clinically anxious adolescents. 
# Neuroimage Clin 2022:33:102937. doi: 10.1016/j.nicl.2022.102937. Epub 2022 Jan 5.

plot_gam_splines_one_group <- function(gam_model, df_tract, tract_name, gam_plot_dir) {
  # Plot splines for a GAM
  #
  # Will plot smoothed splines produced by GAM
  #   by creating a prediction data frame.
  #
  # Writes:
  #   gam_plot_dir/Plot_GAM_*.png
  
  # generate predictions
  df_pred <- predict(
    gam_model,
    exclude_terms = c("sex", "subjectID"),
    values = list(sex = NULL),
    se.fit = T,
    type = "response"
  )
  
  # convert predictions to dataframe
  df_pred <- data.frame(
    sex = df_tract$sex,
    subjectID = df_tract$subjectID,
    nodeID = df_tract$nodeID,
    fit = df_pred$fit,
    se.fit = df_pred$se.fit
  )
  
  # set up for plot
  h_title <- paste0("GAM Fit of ", tract_name, " z scores")
  
  # draw plot
  p <- ggplot(data = df_pred) +
    geom_smooth(mapping = aes(x = nodeID, y = fit)) +
    ggtitle(h_title) +
    ylab("Fit z") +
    xlab("Node") +
    theme(text = element_text(
      family = "Times New Roman", face = "bold", size = 14
    ))
  
  ggsave(
    paste0(gam_plot_dir, "Plot_GAM_", tract_name, ".png"),
    units = "in",
    width = 6,
    height = 6,
    device = "png"
  )
}
