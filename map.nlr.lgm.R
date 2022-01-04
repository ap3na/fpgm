## 5.3 fun_map.nlr.lgm
map.nlr.lgm <- function(data) {
  data <- data %>% 
    mutate(nls_fit = map(data, ~possibly(~nlr.lgm(.x),
                                         otherwise = NA,
                                         quiet = TRUE)(.x)))
  return(data)
}
