# 12. fun_field.actual.production.date
#
field.actual.production.date <- function(data) {
  # fapd: field actual production date 
  # function to calculate field actual production date     
  fapd = max(data$date, na.rm = T)
  return(fapd)
}
