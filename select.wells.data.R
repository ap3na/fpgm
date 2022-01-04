# filter data function
# data: input data
# var1: variable to be selected
# var2: variable to be filtered

select.wells.data <- function(data, var1, var2) {
  data <- data %>% filter(str_detect(var1, var2))
  return(data)
}
