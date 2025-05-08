# This code is from Supplementary Materials for NM Muncy, A Kimbler, 
# AM Hedges-Muncy, DL McMakin, AT Mattfeld. General additive models address 
# statistical issues in diffusion MRI: An example with clinically anxious adolescents. 
# Neuroimage Clin 2022:33:102937. doi: 10.1016/j.nicl.2022.102937. Epub 2022 Jan 5.

calc_gam_stats <- function(tract, df_tract) {
  
  # Calculate GAM for tract node FA values
  #
  # This function has a series of steps:
  #   1) Use distribution to determine GAM family
  #       Gamma and beta are used for the tract
  #   2) Build the GAM, test if basis function is appropriate
  #       It was determined that k=40 worked for each tract
  #   3) Compare different GAMs to find best fit
  #   4) Add covariates, repeat 2-3
  #
  # Arguments:
  #   tract = AFQ tract name
  #   df_tract = dataframe of node FAs for single tract
  #
  # Writes:
  #   gam_stats_dir/Stats_GAM-*.txt
  #
  # Returns:
  #   fit_cov_pds = preferred GAM object for each tract   
  #
  # Notes: 
  #   1) Given that k=40 was appropriate for each tract, and the covariate
  #   model better fit the non-covariate model, only gam_cov_pds is
  #   plotted for each tract.
  
  # plot mean data 
  print(
    ggplot(data = df_tract) +
      geom_smooth(mapping = aes(x=nodeID, y=dti_fa, color=group))
  )
  browser()
  
  # plot mean data with original data points as well
  print(
    ggplot(data = df_tract) +
      geom_point(mapping = aes(x=nodeID, y=dti_fa, color=group), size=0.3) +
      geom_smooth(mapping = aes(x=nodeID, y=dti_fa, color=group))
  )
  browser()
  
  # determine distribution - produces Cullen and Frey graph
  # This graph shows which all possible distributions and how
  # our data compares to them. 
  descdist(df_tract$dti_fa, discrete=F) # beta or gamma dist
  
  browser()
  
  # This is an exploratory step to assess if the beta distribution
  # fits the data well, ignoring predictors like group,sex and nodeID.
  # It gives a value of AIC that can be compared to the gamma model.
  fit.beta <- fitdist(df_tract$dti_fa, "beta")
  plot(fit.beta)
  fit.beta$aic
  
  browser()
  # This is an exploratory step to assess if the beta distribution
  # fits the data well, ignoring predictors like group,sex and nodeID.
  # It gives a value of AIC that can bee compared to the beta model.
  fit.gamma <- fitdist(df_tract$dti_fa, "gamma")
  plot(fit.gamma)
  fit.gamma$aic
  
  browser()
  
  #  write gam with gamma family
  fit_gamma <- bam(dti_fa ~ group +
                     sex + #control for sex
                     s(nodeID, by = group, k = 40) +
                     s(subjectID, bs = "re"),
                   data = df_tract,
                   family = Gamma(link = "logit"), # logit function used when data has bounded values
                   method = "REML" #method to assess model fit 
  )
  
  browser()
  
  # determine k
  gam.check(fit_gamma, rep = 500) # k = 40 works well
  
  browser()
  
  # Save model summary to file
  capture.output(
    summary(fit_gamma),
    file = paste0(
      gam_stats_dir, "Stats_GAM-gamma_", tract, "_G3.txt"
    )
  )
  
  browser()
  
  # write gam with beta family
  fit_beta <- bam(dti_fa ~ group +
                    sex +
                    s(nodeID, by = group, k = 40) +
                    s(subjectID, bs = "re"),
                  data = df_tract,
                  family = betar(link = "logit"),
                  method = "REML"
  )
  
  browser()
  
  gam.check(fit_beta, rep = 500) # k = 40 works well
  
  # Write model summary to file
  capture.output(
    summary(fit_beta),
    file = paste0(
      gam_stats_dir, "Stats_GAM-beta_", tract, "_G3.txt"
    )
  )
  
  browser()
  
  # determine which model to keep
  # Compare the model and write the summaries to file
  capture.output(
    compareML(fit_gamma, fit_beta),
    file = paste0(
      gam_stats_dir,
      "Stats_GAM-comp_gam-beta_",
      tract, "_G3.txt"
    )
  )
  
  browser()
  
  # After choosing model, add in covariates of interest
  # to model
  fit_cov_pds <- bam(dti_fa ~ group +
                       sex +
                       s(nodeID, by = group, k = 40) +
                       s(pds, by = sex) +
                       s(subjectID, bs = "re"),
                     data = df_tract,
                     family = Gamma(link = "logit"),
                     method = "REML"
  )
  
  browser()
  
  # Check that the k values makes sense
  # determine k
  gam.check(fit_cov_pds, rep = 500) # again, k=40 works well
  
  # Write model summary to file
  capture.output(
    summary(fit_cov_pds),
    file = paste0(
      gam_stats_dir,
      "Stats_GAM-cov_",
      tract, "_G3.txt"
    )
  )
  
  browser()
  
  # compare model with covariate to model without covariate to 
  # see if adding the covariate improved the model fit
  # test cov model against non-covariate
  capture.output(
    compareML(fit_gamma, fit_cov_pds),
    file = paste0(
      gam_stats_dir,
      "Stats_GAM-comp_gam-cov_",
      tract, "_G3.txt"
    )
  )
  
  return(fit_cov_pds)
}