
plot_tracts <- function(df_z, sex){
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
    new_filename <- paste0("tracts_by-sex_", metric, "-z_profile_", splits, "splits_alltracts.png")
    file.rename(old_filename, new_filename)
  }
}

plot_specific_tracts = function(df_z, tractnames, sex, title_suff, width, height) {
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
    new_filename <- paste0("tracts_", metric,"_splits_", splits, "both_sexes_sig", title_suff,".png")
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
    new_filename <- paste0("tracts_", metric,"_splits_", splits, "_female_sig.png")
    file.rename(old_filename, new_filename)
  }
}
  