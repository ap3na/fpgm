## 2. fun_Qbywells  
Q.by.wells <- function(data) {
  
  well_name <- date <- field <- q <-  days <- NULL
  qm <- n <- month <- year <- Q <- NULL
  data %>%
    dplyr::group_by(well_name) %>%
    dplyr::mutate(
      qm = q * days,
      # divide Q by 1000 to convert thousands to millions
      Q = cumsum(qm) / 1000) %>%
    # calculate time vector
    dplyr::mutate(t = 1:n()) %>%
    dplyr::select(
      field, date, t, days, month,
      year, well_name, q, Q)
}
