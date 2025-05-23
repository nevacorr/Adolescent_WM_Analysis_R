
library(tractable)

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
}
