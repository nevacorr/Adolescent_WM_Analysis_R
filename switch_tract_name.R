# This code is from Supplementary Materials for NM Muncy, A Kimbler, 
# AM Hedges-Muncy, DL McMakin, AT Mattfeld. General additive models address 
# statistical issues in diffusion MRI: An example with clinically anxious adolescents. 
# Neuroimage Clin 2022:33:102937. doi: 10.1016/j.nicl.2022.102937. Epub 2022 Jan 5.

switch_tract_name <- function(tract) {
  # Switch for decoding AFQ tract names
  #
  # Arguments:
  #   tract = AFQ tract string
  #
  # Returns:
  #   x_tract = str, reformatted tract name
  
  x_tract <- switch(
    tract,
    "UNC_L" = "L. Uncinate",
  )
  return(x_tract)
}