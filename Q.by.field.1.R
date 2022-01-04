## 16.1 fun_Q.by.field.1
Q.by.field.1 <- function(well_data) {
  well_data %>%
    dplyr::group_by(year, month) %>%  # group by year & months
    dplyr::summarise(
      date = unique(date),         # extract date
      qm = q * days) %>%           # calculate production by months
    dplyr::ungroup()
}
