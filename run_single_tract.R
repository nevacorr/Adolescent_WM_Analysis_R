
run_single_tract <- function(df_z, unique_tracts, metric) {
  
  wd="/Users/nevao/R_Projects/AdolWMAnalysis"
  out_dir <- paste0(wd,"/output_", metric, "/")
  gam_plot_dir <- paste0(out_dir, "plots_gam/")
  gam_stats_dir <- paste0(out_dir, "stats_gam/")
  table_dir <- paste0(out_dir, "tables/")
  
  # Loop through all tracts to calculate statistics
  for (tract in unique_tracts) {
    print(tract)

    # Set sex as a factor
    df_z$sex <- factor(df_z$sex)
    
    # Filter for current tract
    tract_data <- df_z[df_z$tractID == tract, ]
    
    gam_file <- paste0(out_dir, "gam_", tract, ".Rda")
    if (!file.exists(gam_file)) {
      gam_model <- calc_gam_stats(tract_data, tract, gam_stats_dir)
      saveRDS(gam_model, file = gam_file)
      rm(gam_model)
      }
      
    gam_model <- readRDS(gam_file)
   
    print('calc_spline_diff')
     
    # determine nodes of group differences
    comp_list <- c("M", "F")
    node_list <- calc_spline_diff(gam_model, tract_data, tract, comp_list, 
                                  gam_plot_dir, gam_stats_dir)
    
  }
}

