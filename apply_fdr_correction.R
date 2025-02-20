apply_fdr_correction <- function(df, column_name) {
  
  p_values <- df[[column_name]]
  
  df$FDR_corrected <- p.adjust(p_values, method = 'fdr')
  
  return(df)
}