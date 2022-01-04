## 13.4 fun_field.date.model.extend
field.date.model.extend <- function(data) {
  # fdme: field.date.model.extend 
  # function to calculate field actual production date     
  fdme = max(data$date, na.rm = T)
}
