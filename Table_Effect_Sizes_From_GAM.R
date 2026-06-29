library(dplyr)
library(gt)
library(patchwork)
library(ggplot2)
library(gt)
library(flextable)
library(officer)

sensitivity_analysis <- FALSE
metric <- "md"

if (sensitivity_analysis) {
  output_stats_path <- "/Users/nevao/R_Projects/AdolWMAnalysis/sens_analysis_tract stats files"
} else {
  output_stats_path <- "/Users/nevao/R_Projects/AdolWMAnalysis/tract stats files"
}

# ==================================================
# LOAD DATA
# ==================================================
results_male <- read.csv(paste0(output_stats_path, "/effect_sizes_male_", metric, ".csv"))
results_female <- read.csv(paste0(output_stats_path, "/effect_sizes_female_", metric, ".csv"))
results_all <- read.csv(paste0(output_stats_path, "/effect_sizes_all_", metric, ".csv"))
results_sex <- read.csv(paste0(output_stats_path, "/effect_sizes_sex_diff_", metric, ".csv"))

# ==================================================
# FIX SEX COLUMN DUPLICATION
# ==================================================
results_sex <- results_sex[, names(results_sex) != "effect_size"]
names(results_sex)[names(results_sex) == "sex_effect_size"] <- "effect_size"

# ==================================================
# REMOVE TRACTS FOR FINAL FIGURES
# ==================================================

results_male$tract   <- gsub("\\.", " ", results_male$tract)
results_female$tract <- gsub("\\.", " ", results_female$tract)
results_all$tract    <- gsub("\\.", " ", results_all$tract)
results_sex$tract    <- gsub("\\.", " ", results_sex$tract)

if (metric == "md") {
  
  tracts_to_remove <- c(
    "Callosum Forceps Major",
    "Right ILF"
  )
  
} else if (metric == "fa") {
  
  tracts_to_remove <- c(
    "Callosum Forceps Major",
    "Left Corticospinal",
    "Left IFOF",
    "Left ILF",
    "Left Uncinate",
    "Right Arcuate",
    "Right ILF",
    "Right Thalamic Radiation",
    "Right Uncinate"
  )
  
} else {
  
  tracts_to_remove <- character(0)
  
}

results_male   <- results_male   %>% filter(!tract %in% tracts_to_remove)
results_female <- results_female %>% filter(!tract %in% tracts_to_remove)
results_all    <- results_all    %>% filter(!tract %in% tracts_to_remove)
results_sex    <- results_sex    %>% filter(!tract %in% tracts_to_remove)

# ==================================================
# FORMAT NUMERIC VALUES (p + FDR)
# ==================================================
fmt2 <- function(x) sprintf("%.2f", x)

results_male   <- mutate(results_male,   across(where(is.numeric), fmt2))
results_female <- mutate(results_female, across(where(is.numeric), fmt2))
results_all <- mutate(results_all, across(where(is.numeric), fmt2))
results_sex    <- mutate(results_sex,    across(where(is.numeric), fmt2))

# ==================================================
# GT TABLE FUNCTION 
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

pooled_tbl <- make_table(
  results_all,
  "All Subjects",
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
pooled_g <- gt::as_gtable(pooled_tbl)
sex_g    <- gt::as_gtable(sex_tbl)

final_plot <- wrap_elements(male_g) +
  wrap_elements(female_g) +
  wrap_elements(sex_g) +
  plot_layout(ncol = 3)

# ==================================================
# SAVE FIGURES
# ==================================================
ggsave(
  filename = paste0(output_stats_path, "/effect_size_tables_revision2", metric, ".png"),
  plot = final_plot,
  width = 14,
  height = 7,
  dpi = 300
)
ggsave(
  filename = paste0(output_stats_path, "/effect_size_table_all_revision2", metric, ".png"),
  plot = wrap_elements(pooled_g),
  width = 5,
  height = 7,
  dpi = 300
)
# save in editable table format
gtsave(male_tbl,   filename = paste0(output_stats_path, "/male_table_", metric, ".docx"))
gtsave(female_tbl, filename = paste0(output_stats_path, "/female_table_", metric, ".docx"))
gtsave(pooled_tbl, filename = paste0(output_stats_path, "/pooled_table_", metric, ".docx"))
gtsave(sex_tbl,    filename = paste0(output_stats_path, "/sex_table_", metric, ".docx"))

# ==================================================
# PREPARE EACH SUB-TABLE — select and rename columns
# ==================================================
male_sub <- results_male %>%
  select(tract,
         m_effect   = effect_size,
         m_p        = intercept_p,
         m_fdr      = FDR_corrected)

female_sub <- results_female %>%
  select(tract,
         f_effect   = effect_size,
         f_p        = intercept_p,
         f_fdr      = FDR_corrected)

sex_sub <- results_sex %>%
  select(tract,
         s_effect   = effect_size,
         s_p        = sex_p,
         s_fdr      = FDR_corrected)

# ==================================================
# COMBINE SIDE BY SIDE with spacer columns
# ==================================================
combined <- male_sub %>%
  left_join(female_sub, by = "tract") %>%
  left_join(sex_sub,    by = "tract") %>%
  mutate(
    spacer1 = "",   # gap between Male and Female
    spacer2 = ""    # gap between Female and Sex Difference
  ) %>%
  select(
    tract,
    m_effect, m_p, m_fdr,
    spacer1,
    f_effect, f_p, f_fdr,
    spacer2,
    s_effect, s_p, s_fdr
  )

# ==================================================
# BUILD FLEXTABLE
# ==================================================
ft <- flextable(combined) %>%
  
  # ── Column labels ──
  set_header_labels(
    tract    = "Tract",
    m_effect = "Effect Size", m_p = "p", m_fdr = "FDR",
    spacer1  = "",
    f_effect = "Effect Size", f_p = "p", f_fdr = "FDR",
    spacer2  = "",
    s_effect = "Effect Size", s_p = "p", s_fdr = "FDR"
  ) %>%
  
  # ── Spanner header row — group labels ──
  add_header_row(
    values = c("Tract", "Male", "", "Female", "", "Sex Difference"),
    colwidths = c(1, 3, 1, 3, 1, 3)
  ) %>%
  
  
  # ── Alignment ──
  align(j = "tract",  align = "left",   part = "all") %>%
  
  # ── Bold significant FDR values ──
  bold(j = "m_fdr", i = ~ as.numeric(m_fdr) < 0.05, part = "body") %>%
  bold(j = "f_fdr", i = ~ as.numeric(f_fdr) < 0.05, part = "body") %>%
  bold(j = "s_fdr", i = ~ as.numeric(s_fdr) < 0.05, part = "body") %>%
  
  # ── Spacer columns — narrow and no borders ──
  width(j = c("spacer1", "spacer2"), width = 0.15) %>%
  padding(j = "f_effect", padding.left = 0, part = "all") %>%
  padding(j = "s_effect", padding.left = 0, part = "all") %>%
  border_remove() %>%
  hline_top(part = "header", border = fp_border(width = 1.5)) %>%
  hline_bottom(part = "header", border = fp_border(width = 1.5)) %>%
  hline_bottom(part = "body",   border = fp_border(width = 1.5)) %>%
  
  # ── Add vertical dividers between sections ──
  vline(j = "tract",   border = fp_border(width = 1),   part = "all") %>%
  vline(j = "m_fdr",   border = fp_border(width = 1),   part = "all") %>%
  vline(j = "f_fdr",   border = fp_border(width = 1),   part = "all") %>%
  
  # ── Font and size ──
  font(fontname = "Arial", part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  bold(part = "header") %>%
  bg(bg = "#F2F2F2", part = "header") %>%
  
  autofit()

# ==================================================
# SAVE TO WORD — landscape for width
# ==================================================
outfile <- paste0(output_stats_path,
                  "/effect_size_tables_sidebyside_", metric, ".docx")

save_as_docx(
  ft,
  path = outfile,
  pr_section = prop_section(
    page_size    = page_size(orient = "landscape", width = 11, height = 8.5),
    page_margins = page_mar(top = 0.5, bottom = 0.5, left = 0.5, right = 0.5)
  )
)

cat("Saved:", outfile, "\n")