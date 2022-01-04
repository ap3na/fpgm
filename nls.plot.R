## 7. fun_nls.plot 
nls.plot <- function(data) {
  data %>%
    dplyr::select(well_name, data) %>%
    unnest(data)
}
