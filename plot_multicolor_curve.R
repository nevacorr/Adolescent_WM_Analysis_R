library(dplyr)
library(tidyr)
library(ggplot2)

plot_colored_segments <- function(df, x_col, y_col, color_col) {
  
  # Convert column names to symbols
  x_col <- sym(x_col)
  y_col <- sym(y_col)
  color_col <- sym(color_col)
  
  # Build segment-wise data
  segments <- df %>%
    mutate(
      xend = lead(!!x_col),  # Dynamically use x_col
      yend = lead(!!y_col),  # Dynamically use y_col
      group = row_number()
    ) %>%
    filter(!is.na(xend))  # remove last NA row
  
  # Reshape to long format: one row per point in each segment
  segment_lines <- segments %>%
    select(group, !!color_col, !!x_col, !!y_col) %>%
    rename(x1 = !!x_col, y1 = !!y_col) %>%
    bind_rows(
      segments %>%
        select(group, !!color_col, x = xend, y = yend) %>%
        rename(x2 = x, y2 = y)
    ) %>%
    arrange(group) %>%
    tidyr::pivot_longer(cols = c(x1, x2, y1, y2), 
                        names_to = c(".value", "pt"),
                        names_pattern = "(.)(.)") %>%
    arrange(group, pt)
  
  # Plot using geom_line by group
  plot <- ggplot(segment_lines, aes(x = x, y = y, group = group, color = !!color_col)) +
    geom_line(linewidth = 1.2) +
    scale_color_identity() +
    theme_minimal()
  
  # print(plot)
  return(segment_lines)
}

