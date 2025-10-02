
get_x_axis_string <- function(tract_name) {
  case_when(
    grepl("Left", tract_name) & grepl("Arcuate|Thalamic|ILF|IFOF|SLF|Uncinate", 
                                      tract_name) ~ "Anterior                    Posterior",
    grepl("Right", tract_name) & grepl("Arcuate|Thalamic|ILF|IFOF|SLF|Uncinate"
                                       , tract_name) ~ "Posterior                     Anterior",
    grepl("Corticospinal", tract_name) ~ "Inferior                       Superior",
    grepl("Forceps", tract_name) ~ "Left                       Right"
  )
}