# This code is modified from Supplementary Materials for NM Muncy, A Kimbler, 
# AM Hedges-Muncy, DL McMakin, AT Mattfeld. General additive models address 
# statistical issues in diffusion MRI: An example with clinically anxious adolescents. 
# Neuroimage Clin 2022:33:102937. doi: 10.1016/j.nicl.2022.102937. Epub 2022 Jan 5.

calc_gam_stats <- function(tract, tract_name, gam_stats_dir, sex_str) {

  sex_char <- ifelse(sex_str == "male", "M", "F")
  
  print('calc_gam_stats: plot mean data')
  # plot mean data 
  print(
    ggplot(data = tract) +
      geom_smooth(mapping = aes(x=nodeID, y=z))
  )
  
  print('calc_gam_stats: plot mean data with data points')
  # plot mean data with original data points as well
  print(
    ggplot(data = tract) +
      geom_point(mapping = aes(x=nodeID, y=z, color=subjectID), size=0.3) +
      geom_smooth(mapping = aes(x=nodeID, y=z)) +
      theme(legend.position = "none")
  )
  
  # determine distribution - produces Cullen and Frey graph
  # This graph shows which all possible distributions and how
  # our data compares to them. 
  descdist(tract$z, discrete=F)  
  
  fit_gaussian <- gam(z ~ 
                 sex + 
                 s(nodeID, by = sex, bs = "fs", k = 40) + 
                 s(subjectID, bs = "re"),
                 data = tract,
                 method = "REML")

  
  # check k
  gam.check(fit_gaussian, rep = 500) # check if k works well
  
  print(summary(fit_gaussian))
  
  # Save model summary to file
  capture.output(
    summary(fit_gaussian),
    file = paste0(
      gam_stats_dir, "Stats_GAM-gaussian_", tract_name, "_", sex_char, ".txt"
    )
  )

  return(fit_gaussian)
}