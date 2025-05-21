plot_behav_vs_measure <- function(data, tract_name, brain_measure_name, behav_measure_name,
                                  metric) {
  # Filter for the specified tract
  tract_data <- data %>%
    filter(tractID == tract_name)
  
  # make a plot
  p <- ggplot(tract_data, aes_string(x = brain_measure_name, y = behav_measure_name)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(
      title = paste("Post-COVID Z", metric, "vs. Z", behav_measure_name, "in", tract_name),
      x = metric,
      y = behav_measure_name
    ) +
    theme_minimal()
  
  return(p)
}