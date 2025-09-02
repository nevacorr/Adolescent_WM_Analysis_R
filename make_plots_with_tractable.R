library(tractable)
library(dplyr)
library(tidyr)
library(ggplot2)

plot_tracts <- function(df_z, sex, path){
  if (sex == 1) {
    plot_tract_profiles(
      df = df_z, 
      y = "z", 
      group_col = "sex",
      n_groups = 2,
      save_figure = TRUE,
      ribbon_alpha = 0.20,
      group_pal = "Set1",
      width=10,
      height=10
    )
    # Rename output file
    old_filename <- "tracts_by-sex_param-z_profile.png"
    new_filename <- file.path(path, paste0("tracts_by-sex_", metric, "-z_profile_", splits, "splits_alltracts.png"))
    file.rename(old_filename, new_filename)
  }
}

plot_specific_tracts = function(df_z, tractnames, sex, title_suff, width, height, metric) {
  if (sex==1) {
    plot_tract_profiles(
      df = df_z, 
      y = "z", 
      group_col = "sex",
      tracts = tractnames,
      n_groups = 2,
      save_figure = TRUE,
      ribbon_alpha = 0.20,
      group_pal = "Set1",
      width=width,
      height=height
    )
    # Rename output file
    old_filename <- "tracts_by-sex_param-z_profile.png"
    new_filename <- paste0("tracts_", metric,"_splits_", splits, title_suff,".png")
    file.rename(old_filename, new_filename)
  } else if (sex==0) {
    plot_tract_profiles(
      df = df_z, 
      y = "z", 
      tracts = tractnames,
      save_figure = TRUE,
      ribbon_alpha = 0.20,
      group_pal = "Set1",
      width=10,
      height=10
    )    
    # Rename output file
    old_filename <- "tracts_param-z_profile.png"
    new_filename <- paste0("tracts_", metric,"_splits_", splits, title_suff, ".png")
    file.rename(old_filename, new_filename)
  }
}

plot_specific_tracts_new_format = function(df_z, tractnames, sex, title_suff, width, 
                                           height, metric, pvalues, x_axis_string, 
                                           out_path) {
  if (sex==1) {
    plot_tract_profiles_my_edit(
      df = df_z, 
      y = "z", 
      metric = metric,
      pvalues = pvalues, 
      x_axis_string = x_axis_string,
      group_col = "sex",
      tracts = tractnames,
      n_groups = 2,
      save_figure = TRUE,
      ribbon_alpha = 0.20,
      group_pal = "Set1",
      width=width,
      height=height
    )
    # Rename output file
    old_filename <- "tracts_by-sex_param-z_profile.png"
    new_filename <- file.path(out_path, paste0("tracts_", metric,"_splits_", splits, title_suff,"_new_format_gam.png"))
    file.rename(old_filename, new_filename)
  }
  
  # else if (sex==0) {
  # 
  #   # Compute mean z value at each node for each tract and sex
  #   mean_by_sex <- df_z %>%
  #     group_by(tractID, nodeID, sex) %>% 
  #     summarize(mean_z = mean(z, na.rm = TRUE), .groups = "drop")
  #   
  #   # Separate mean z for males and females into different columns
  #   mean_wide <- mean_by_sex %>% 
  #     pivot_wider(names_from = sex, values_from = mean_z)
  #   
  #   # Compute difference between male and female z scores
  #   mean_wide <- mean_wide %>% 
  #     mutate(meanz_diff = F- M) %>% 
  #     select(tractID, nodeID, meanz_diff)
  #   
  #   # Merge with pvalues
  #   final_z_df <- mean_wide %>% 
  #     left_join(pvalues, by = c("tractID" = "Tract", "nodeID"="Node"))
  #   
  #   #  Prepare df_curr for difference line
  #   df_curr <- final_df %>%   # final_df has tractID, nodeID, meanz_diff, adjusted_p_value
  #     dplyr::rename(
  #       y = meanz_diff,
  #       tracts = tractID,
  #       x = nodeID
  #     ) %>%
  #     dplyr::mutate(
  #       color_col = ifelse(adjusted_p_value <= 0.05, rgb(0.85, 0.37, 0.01), "gray"),
  #       group = "diff"  # single group for one line
  #     )
  #   
  #   # Create continuous colored segments (like plot_colored_segments does)
  #   segment_lines_diff <- plot_colored_segments(df_curr, x_col = "x", y_col = "y", color_col = "color_col")
  #   
  #   # Plot the difference line
  #   plot_handle <- ggplot(segment_lines_diff, aes(x = x, y = y, group = group, color = color_col)) +
  #     geom_line(linewidth = 1.2, show.legend = FALSE) +
  #     scale_color_identity() +
  #     
  #     # Horizontal line at y=0
  #     geom_hline(yintercept = 0, color = "black", size = 1) +
  #     
  #     # Facet by tract
  #     facet_wrap(~ tracts) +
  #     
  #     # Axis labels
  #     scale_x_continuous(name = x_axis_string) +
  #     scale_y_continuous(name = "F - M Z-score", limits = c(-1.2, 0.8)) +
  #     
  #     # Formatting same as your original
  #     theme_bw() +
  #     theme(
  #       panel.grid = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.ticks.x = element_blank(),
  #       strip.background = element_blank(),
  #       strip.text = element_text(size = 16),
  #       axis.title.x = element_text(size = 18),
  #       axis.title.y = element_text(size = 18),
  #       axis.text.y = element_text(size = 16),
  #       legend.text = element_text(size = 16),
  #       legend.title = element_text(size = 18)
  #     )
  #   
  #     print(plot_handle)
  # }
}
