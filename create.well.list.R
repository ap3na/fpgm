# 3. create list of wells
# 
create.well.list <- function(data) {
  well_name <- date <- t <- Q <- q <- NULL
  data %>%
    dplyr::select(well_name, date, t, q, Q) %>%
    dplyr::group_by(well_name) %>%
    dplyr::group_nest()
}
