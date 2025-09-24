
run_single_tract <- function(df_z_both_sexes, df_z, tract, metric, sex_str) {
  
# Define directories for output
wd="/Users/nevao/R_Projects/AdolWMAnalysis"
out_dir <- paste0(wd,"/output_", metric, "/")
gam_plot_dir <- paste0(out_dir, "plots_gam/")
gam_stats_dir <- paste0(out_dir, "stats_gam/")
table_dir <- paste0(out_dir, "tables/")

# Make output directories if they don't already exist
dirs <- c(out_dir, gam_plot_dir, gam_stats_dir)
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

print(tract)

# Set sex as a factor
df_z_both_sexes$sex <- factor(df_z_both_sexes$sex)

# Filter for current tract
tract_data <- df_z_both_sexes[df_z_both_sexes$tractID == tract, ]

# gam_file <- paste0(out_dir, "gam_", tract, ".Rda")
# if (!file.exists(gam_file)) {
gam_model <- calc_gam_stats(tract_data, tract, gam_stats_dir, sex_str)
  # saveRDS(gam_model, file = gam_file)
  # rm(gam_model)
  # }
  
# gam_model <- readRDS(gam_file)

print('calc_spline_diff')
 
# determine nodes significantly different from zero

output <- calc_spline_diff(gam_model, tract_data, tract, 
                              gam_plot_dir, gam_stats_dir, sex_str)

return(output)

}

