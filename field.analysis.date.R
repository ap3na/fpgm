# 1.2 fun_field.analysis.date 
# 
# 
field.analysis.date <- function(wfd, fad) {
  # fad: field analysis date  
  wfd %>% filter(date <= fad)
}

