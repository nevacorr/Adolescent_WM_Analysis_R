library(dplyr)
library(gt)
library(patchwork)

metric <- "fa"
output_stats_path <- "/Users/nevao/R_Projects/AdolWMAnalysis/tract stats files"

# ==================================================
# LOAD DATA
# ==================================================
results_male <- read.csv(paste0(output_stats_path, "/effect_sizes_male_", metric, ".csv"))
results_female <- read.csv(paste0(output_stats_path, "/effect_sizes_female_", metric, ".csv"))
results_sex <- read.csv(paste0(output_stats_path, "/effect_sizes_sex_diff_", metric, ".csv"))

# ==================================================
# FIX SEX COLUMN DUPLICATION
# ==================================================
results_sex <- results_sex[, names(results_sex) != "effect_size"]
names(results_sex)[names(results_sex) == "sex_effect_size"] <- "effect_size"

# ==================================================
# FORMAT NUMERIC VALUES (p + FDR)
# ==================================================
fmt2 <- function(x) sprintf("%.2f", x)

results_male   <- mutate(results_male,   across(where(is.numeric), fmt2))
results_female <- mutate(results_female, across(where(is.numeric), fmt2))
results_sex    <- mutate(results_sex,    across(where(is.numeric), fmt2))

# ==================================================
# GT TABLE FUNCTION (NO CI ANYMORE)
# ==================================================
make_table <- function(df, title, cols, center_cols, use_sex_p = FALSE) {
  
  tbl <- df %>%
    select(all_of(cols)) %>%
    gt() %>%
    
    tab_header(title = title)
  
  # ---------------------------
  # COLUMN LABELS
  # ---------------------------
  if (use_sex_p) {
    tbl <- tbl %>%
      cols_label(
        tract = "Tract",
        effect_size = "Effect size",
        sex_p = "p",
        FDR_corrected = "FDR"
      )
  } else {
    tbl <- tbl %>%
      cols_label(
        tract = "Tract",
        effect_size = "Effect size",
        intercept_p = "p",
        FDR_corrected = "FDR"
      )
  }
  
  # ---------------------------
  # ALIGNMENT
  # ---------------------------
  tbl <- tbl %>%
    
    cols_align(
      align = "center",
      columns = all_of(center_cols)
    ) %>%
    
    cols_align(
      align = "left",
      columns = tract
    ) %>%
    
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels(columns = all_of(center_cols))
    )
  
  # ---------------------------
  # BOLD SIGNIFICANT FDR (< 0.05)
  # ---------------------------
  tbl <- tbl %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = FDR_corrected,
        rows = as.numeric(FDR_corrected) < 0.05
      )
    )
  
  return(tbl)
}

# ==================================================
# BUILD TABLES
# ==================================================
male_tbl <- make_table(
  results_male,
  "Male",
  c("tract", "effect_size", "intercept_p", "FDR_corrected"),
  c("effect_size", "intercept_p", "FDR_corrected"),
  use_sex_p = FALSE
)

female_tbl <- make_table(
  results_female,
  "Female",
  c("tract", "effect_size", "intercept_p", "FDR_corrected"),
  c("effect_size", "intercept_p", "FDR_corrected"),
  use_sex_p = FALSE
)

sex_tbl <- make_table(
  results_sex,
  "Sex Difference",
  c("tract", "effect_size", "sex_p", "FDR_corrected"),
  c("effect_size", "sex_p", "FDR_corrected"),
  use_sex_p = TRUE
)

# ==================================================
# CONVERT TO GROBS + COMBINE
# ==================================================
male_g   <- gt::as_gtable(male_tbl)
female_g <- gt::as_gtable(female_tbl)
sex_g    <- gt::as_gtable(sex_tbl)

final_plot <- wrap_elements(male_g) +
  wrap_elements(female_g) +
  wrap_elements(sex_g) +
  plot_layout(ncol = 3)

# ==================================================
# SAVE FIGURE
# ==================================================
ggsave(
  filename = paste0(output_stats_path, "/effect_size_tables_", metric, ".png"),
  plot = final_plot,
  width = 14,
  height = 7,
  dpi = 300
)