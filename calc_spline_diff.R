
# This code is modified from Supplementary Materials for NM Muncy, A Kimbler, 
# AM Hedges-Muncy, DL McMakin, AT Mattfeld. General additive models address 
# statistical issues in diffusion MRI: An example with clinically anxious adolescents. 
# Neuroimage Clin 2022:33:102937. doi: 10.1016/j.nicl.2022.102937. Epub 2022 Jan 5.

calc_spline_diff <- function(gam_model, 
                             tract_data, 
                             tract, 
                             gam_plot_dir, 
                             gam_stats_dir, 
                             sex_str) {
# Investigate nodes where spline is different from zero

df <- make_spline_single_group_df(gam_model, tract_data, tract, 
                                    gam_plot_dir, gam_stats_dir, sex_str)

return(df)

}

